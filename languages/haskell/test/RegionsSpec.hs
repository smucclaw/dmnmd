{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}

-- | Tests for "DMN.Regions", the region enumerator D-21 and D-22 build on.
--
-- Three layers, cheapest first:
--
--  * hand-counted tables, whose blocks, live sets and representatives are
--    written out in full below;
--  * a property, run over those tables and over every markdown fixture
--    @test\/roundtrip\/run-roundtrip.sh@ iterates: at every region's
--    representative, dmnmd's OWN row matcher ('matches', which is 'fEvals' and
--    so 'fEval') selects exactly the region's live set, the pruned
--    enumerations agree with filtering the full one, and 'conflicts' agrees
--    with working the pairs out from every region;
--  * the corpus measurement, which pins the fixtures the reader refuses for a
--    conflict region (D-22 rule 2) and checks that no table it accepts has one.
--
-- The refusal itself, 'conflictErrors', is tested between the two, on
-- hand-written tables and through the markdown reader; @DmnXmlSpec@ covers the
-- XML reader.
--
-- The corpus is read with @app\/ParseMarkdown@, the binary's own reader, so a
-- table means here what it means to @dmnmd@.
module RegionsSpec (regionsSpec) where

import           Control.Applicative  ((<|>))
import           Control.Exception    (SomeException, bracket, displayException, evaluate, try)
import           Control.Monad        (filterM, forM)
import           Data.Either          (isLeft)
import           Data.List            (isInfixOf, isPrefixOf, nub, sort)
import           Data.Maybe           (listToMaybe)
import qualified Data.Map.Strict      as M
import qualified Data.Text            as T
import           GHC.IO.Handle        (hDuplicate, hDuplicateTo)
import           System.Directory     (doesDirectoryExist, listDirectory)
import           System.FilePath      ((</>), takeFileName)
import           System.IO            (IOMode (WriteMode), hClose, openFile, stderr)
import           Test.Hspec
import           Text.RawString.QQ

import           DMN.BuildTable       (tableErrors)
import           DMN.DecisionTable    (evalTable, fEvals, fNEval, matches, mkFsEither, outputOrder,
                                       tableWarnings, uniquenessErrors)
import           DMN.Diagnostic       (Diagnostic (..), Severity (..))
import           DMN.ParseTable       (parseTable, parseTableD)
import           DMN.ParsingUtils     (parseOnly)
import           DMN.Regions
import           DMN.Types
import           Options              (ArgOptions (..), FileFormat (..))
import           ParseMarkdown        (parseMarkdown)

regionsSpec :: Spec
regionsSpec = describe "DMN.Regions" $ do
  handCounted
  unsupported
  refusing
  corpus

-- * Helpers

-- | A table as the markdown reader builds it, whether or not the reader would
-- then refuse it for a conflict region.
--
-- Since D-22 part 2 the reader refuses a @U@ or @A@ table with a conflict
-- region, and several tables below exist precisely to have one. So a @U@ or
-- @A@ table is parsed under @F@, which the reader refuses for nothing that
-- depends on the hit policy here, and its own hit policy is put back. The
-- hit policy reaches nothing else the reader does to the cells: inference,
-- re-typing and the cell checks are the same under every letter.
table :: String -> DecisionTable
table src = case dropWhile (== '\n') src of
  '|' : ' ' : c : ' ' : '|' : rest
    | c == 'U' -> (parse ("| F |" ++ rest)) { hitpolicy = HP_Unique }
    | c == 'A' -> (parse ("| F |" ++ rest)) { hitpolicy = HP_Any }
  s' -> parse s'
  where parse = either error id . parseOnly (parseTable "T") . T.pack

mapOf :: DecisionTable -> RegionMap
mapOf dt = either (error . unsupportedMessage) id (regionMap dt)

-- | Each region as the unary tests naming its blocks, and its live rules.
summary :: RegionMap -> [([String], [RuleIx])]
summary rm = [ (showBlock <$> regionBlocks r, liveRules r) | r <- regions rm ]

-- | What dmnmd's own matcher says is live at an input: the rows 'evalTable'
-- would keep before applying the hit policy.
matcherLive :: DecisionTable -> [FEELexp] -> [RuleIx]
matcherLive dt input = [ i | (i, row) <- zip [0 ..] (allrows dt), input `matches` row_inputs row ]

kindOf :: DecisionTable -> Either UnsupportedKind ()
kindOf dt = either (Left . unsupportedKind) (const (Right ())) (regionMap dt)

-- | Every claim a 'RegionMap' makes that dmnmd's matcher can check.
-- Returns the failures, so a corpus run can name them all.
selfCheck :: RegionMap -> [String]
selfCheck rm = concat
  [ [ "representative " ++ show (regionInput r) ++ ": matcher says " ++ show m
        ++ ", region says " ++ show (liveRules r)
    | r <- rs, let m = matcherLive dt (regionInput r), m /= liveRules r ]
  , [ "conflictRegions disagrees with filtering regions"
    | map liveRules (conflictRegions rm) /= map liveRules (filter (isConflict rm) rs)
      || map regionInput (conflictRegions rm) /= map regionInput (filter (isConflict rm) rs) ]
  , [ "noMatchRegions disagrees with filtering regions"
    | map regionInput (noMatchRegions rm) /= map regionInput (filter (isNoMatch rm) rs) ]
  , [ "conflicts says " ++ show got ++ ", where enumerating the regions says " ++ show want
    | let got  = [ (conflictEarlier c, conflictLater c, regionInput (conflictRegion c)) | c <- conflicts rm ]
          want = conflictOracle rm
    , got /= want ]
  , [ "conflicts and conflictRegions disagree about whether there is a conflict"
    | null (conflicts rm) /= null (conflictRegions rm) ]
  , [ "regionCount " ++ show (regionCount rm) ++ " but " ++ show (length rs) ++ " regions"
    | regionCount rm /= toInteger (length rs) ]
  , [ "column " ++ show (varname (cbHeader c)) ++ ": block " ++ showBlock b
        ++ " holds " ++ show v ++ ", where the matcher selects rules " ++ show got
        ++ " and the block says " ++ show (blockRules b)
    | (ci, c) <- zip [0 ..] (rmColumns rm)
    , let cells = [ row_inputs row !! ci | row <- allrows dt ]
    , b <- cbBlocks c
    , v <- probes b
    , let got = [ i | (i, cell) <- zip [0 ..] cells, fEvals (FNullary v) cell ]
    , got /= blockRules b ]
  , [ "representative " ++ show (regionInput r) ++ ": evalTable says " ++ show (evalTable dt (regionInput r))
        ++ ", where the region (live " ++ show (liveRules r) ++ ", default " ++ show (rmDefault rm) ++ ") says " ++ e
    | r <- rs, Just e <- [interpreterDisagrees rm r] ]
  , [ "column " ++ show (varname (cbHeader c)) ++ ": block " ++ showBlock b
        ++ " does not read back as a Number cell that selects exactly it"
    | c <- rmColumns rm
    , vartype (cbHeader c) == Just DMN_Number
    , b <- cbBlocks c
    , not (readsBack b (cbBlocks c)) ]
  ]
  where
    dt = rmTable rm
    rs = regions rm

-- | What 'conflicts' must say, worked out the slow way from every region: for
-- each rule, the lowest-numbered earlier rule it clashes with somewhere, and
-- the representative of the first region in which both are live. Under @U@ two
-- rules clash when neither is the trailing catch-all; under @A@, when their
-- outputs differ as written; under anything else, never.
conflictOracle :: RegionMap -> [(RuleIx, RuleIx, [FEELexp])]
conflictOracle rm =
  [ (i, j, regionInput r)
  | j <- [0 .. length rows - 1]
  , (i, r) <- take 1 [ (i, r) | i <- [0 .. j - 1], clash i j
                              , r <- take 1 [ r | r <- regions rm, i `elem` liveRules r, j `elem` liveRules r ] ]
  ]
  where
    dt   = rmTable rm
    rows = allrows dt
    answering k = rmDefault rm /= TrailingCatchAll k
    clash i j = case hitpolicy dt of
      HP_Unique -> answering i && answering j
      HP_Any    -> row_outputs (rows !! i) /= row_outputs (rows !! j)
      _         -> False

-- | Does 'evalTable', at the region's representative, answer as the region
-- says it must? 'Nothing' if it does, else what the region expected.
--
-- What the region says, by hit policy, once any 'TrailingCatchAll' is set
-- aside as the default:
--
--  * a conflict region is a refusal (@Left@) under @U@ and @A@;
--  * otherwise the answering row is the only live rule under @U@, the first
--    under @A@ (they all agree) and @F@, and the 'outputOrder' winner under @P@;
--  * with no live rule, the default answers — the catch-all's outputs, else
--    the declared default — and with none, null, which is @Right []@.
--
-- Output arithmetic is evaluated as 'evalTable' evaluates it: every live rule's
-- outputs, then the default only if it is the answer. A failure is a @Left@.
interpreterDisagrees :: RegionMap -> Region -> Maybe String
interpreterDisagrees rm r
  | isConflict rm r = check isLeft "a refusal (conflict region)"
  | otherwise = case traverse evalRow liveRows of
      Left _       -> check isLeft "a refusal (a live rule's output does not evaluate)"
      Right evRows -> case pick evRows of
        Just row -> check (== Right [row_outputs row]) ("rule " ++ show (row_number row) ++ "'s outputs")
        Nothing  -> case dflt of
          Nothing -> check (== Right []) "null (no-match, no default)"
          Just d  -> case evalOuts d of
            Left _   -> check isLeft "a refusal (the default does not evaluate)"
            Right d' -> check (== Right [d']) ("the default " ++ show d')
  where
    dt       = rmTable rm
    input    = regionInput r
    actual   = evalTable dt input
    check ok what = if ok actual then Nothing else Just what
    catchAll = case rmDefault rm of TrailingCatchAll k -> Just k; _ -> Nothing
    liveRows = [ allrows dt !! i | i <- liveRules r, Just i /= catchAll ]
    dflt     = (row_outputs . (allrows dt !!) <$> catchAll) <|> dtDefaultOutput dt
    symtab   = M.fromList (zip [ varname ch | ch <- header dt, label ch == DTCH_In ] input)
    evalOuts = traverse (traverse evalCell)
    evalCell (FFunction f) = FNullary <$> fNEval symtab f
    evalCell x             = Right x
    evalRow row = (\os -> row { row_outputs = os }) <$> evalOuts (row_outputs row)
    pick rows = case hitpolicy dt of
      HP_Priority -> listToMaybe (outputOrder (header dt) rows)
      _           -> listToMaybe rows

-- | Values the block claims to contain: its representative, every closed
-- endpoint, every interior point halfway along a bounded interval, and every
-- listed value.
probes :: Block -> [DMNVal]
probes b = blockRep b : case blockValues b of
  Numbers ivs -> concat
    [ [ VN x | Just (BClosed, x) <- [lo] ] ++ [ VN x | Just (BClosed, x) <- [hi] ]
      ++ [ VN ((x + y) * 0.5) | Just (_, x) <- [lo], Just (_, y) <- [hi], x < y ]
    | Interval lo hi <- ivs ]
  Values vs   -> vs
  AllExcept _ -> []

-- | 'showBlock' of a Number block, parsed back by dmnmd's own Number-cell
-- reader, accepts the block's own probes and rejects every other block's.
readsBack :: Block -> [Block] -> Bool
readsBack b siblings = case mkFsEither (Just DMN_Number) (showBlock b) of
  Left _      -> False
  Right cells ->
    all (\v -> fEvals (FNullary v) cells) (probes b)
      && not (any (\v -> fEvals (FNullary v) cells) (concatMap probes (filter (/= b) siblings)))

-- * Hand-counted tables

-- | Memo §5.3's Reg CF sketch, as ROOTSTOCK step 0 wrote it.
regCF :: DecisionTable
regCF = table [r|
| F | aggregate : Number | previously sold : Boolean | requirement (out) |
|---|--------------------|---------------------------|-------------------|
| 1 | <= 124000          | -                         | certified         |
| 2 | <= 618000          | -                         | reviewed          |
| 3 | <= 1235000         | false                     | reviewed          |
| 4 | -                  | -                         | audited           |
|]

-- | Every combination of open and closed interval ends, and the point between
-- @(20..30)@ and @> 30@ that neither rule covers.
endpoints :: DecisionTable
endpoints = table [r|
| U | x : Number | Band (out) |
|---|------------|------------|
| 1 | [0..10)    | a          |
| 2 | [10..20]   | b          |
| 3 | (20..30)   | c          |
| 4 | > 30       | d          |
|]

-- | @policy/l4-hitpolicy-unique-silently-first@'s table, which the reader
-- refuses since D-22 part 2.
uniqueOverlap :: DecisionTable
uniqueOverlap = table [r|
| U | Age   | Fee : Number |
|---|-------|--------------|
| 1 | <= 20 | 5            |
| 2 | >= 10 | 10           |
|]

-- | @policy/eval-hp-any-two-rows-disagree@'s table.
anyDisagree :: DecisionTable
anyDisagree = table [r|
| A | Age  | Verdict (out) |
|---|------|---------------|
| 1 | < 10 | ok            |
| 2 | < 20 | deny          |
|]

anyAgree :: DecisionTable
anyAgree = table [r|
| A | Age  | Verdict (out) |
|---|------|---------------|
| 1 | < 10 | ok            |
| 2 | < 20 | ok            |
|]

-- | @policy/hp-unique-overlap-shared-member-refused@'s table, which was
-- @policy/hp-unique-near-duplicate-rows-accepted@'s before D-22 part 2: rows 3 and 4
-- overlap on Winter, and row 5 is a trailing catch-all.
nearMiss :: DecisionTable
nearMiss = table [r|
| U | Version | Tier          | Season         | Status (out) |
|---|---------|---------------|----------------|--------------|
| 1 | 1.1     | basic         | Fall, Winter   | ended        |
| 2 | 1.2     | basic         | Fall, Winter   | active       |
| 3 | 1.2     | premium       | Fall, Winter   | extended     |
| 4 | 1.2     | premium       | Spring, Winter | seasonal     |
| 5 | -       | -             | -              | unknown      |
|]

-- | @policy/enum-domain-test-not-checked@'s table: two declared domains.
band :: DecisionTable
band = table [r|
| F | Age : Number | Risk Category     | Routing (out)          |
|---|--------------|-------------------|------------------------|
|   | [0..150]     | LOW, MEDIUM, HIGH | DECLINE, REFER, ACCEPT |
| 1 | < 18         | -                 | DECLINE                |
| 2 | [18..65]     | HIGH              | REFER                  |
| 3 | > 65         | LOW               | ACCEPT                 |
|]

-- | A negated String cell. The markdown reader cannot produce one (it is
-- recorded as @symptom/md-negation-in-string-column-silent@) but the IR and
-- 'fEval' can hold it, so it is built directly.
negatedString :: DecisionTable
negatedString = DTable "Neg" HP_Unique
  [ DTCH DTCH_In "Season" (Just DMN_String) Nothing
  , DTCH DTCH_Out "Dish" (Just DMN_String) Nothing ]
  [ DTrow (Just 1) [[FNot (FNullary (VS "Fall"))]] [[FNullary (VS "stew")]] []
  , DTrow (Just 2) [[FNullary (VS "Fall")]]        [[FNullary (VS "spareribs")]] [] ]
  Nothing

handCounted :: Spec
handCounted = describe "hand-counted tables" $ do
  describe "Reg CF (memo §5.3; ROOTSTOCK step 0's 8 regions)" $ do
    let rm = mapOf regCF
    it "has 4 aggregate blocks and 2 previously-sold blocks, so 8 regions" $ do
      map (map showBlock . cbBlocks) (rmColumns rm) `shouldBe`
        [ ["<= 124000", "(124000..618000]", "(618000..1235000]", "> 1235000"]
        , ["true", "false"] ]
      regionCount rm `shouldBe` 8
    it "gives each region its live rules" $
      summary rm `shouldBe`
        [ (["<= 124000", "true"], [0, 1, 3]),        (["<= 124000", "false"], [0, 1, 2, 3])
        , (["(124000..618000]", "true"], [1, 3]),    (["(124000..618000]", "false"], [1, 2, 3])
        , (["(618000..1235000]", "true"], [3]),      (["(618000..1235000]", "false"], [2, 3])
        , (["> 1235000", "true"], [3]),              (["> 1235000", "false"], [3]) ]
    -- Under F the answer is the first live rule. Read down the list and merge
    -- equal neighbours column by column: 0 | 1 | {3, 2} | 3, which is the memo's
    -- 5-leaf tree in authored column order. The tree is l4-ide's (D-21); the
    -- regions are what it is built from.
    it "answers, under F, with the memo's five leaves" $
      map (head . liveRules) (regions rm) `shouldBe` [0, 0, 1, 1, 3, 2, 3, 3]
    it "picks representatives inside each block, preferring a value some cell names" $
      map regionInput (take 2 (regions rm)) ++ map regionInput (drop 6 (regions rm)) `shouldBe`
        [ [FNullary (VN 124000),  FNullary (VB True)], [FNullary (VN 124000),  FNullary (VB False)]
        , [FNullary (VN 1235001), FNullary (VB True)], [FNullary (VN 1235001), FNullary (VB False)] ]
    it "has no conflict region and no no-match region" $ do
      conflictRegions rm `shouldSatisfy` null
      noMatchRegions rm `shouldSatisfy` null
    it "passes the matcher self-check" $ selfCheck rm `shouldBe` []

  describe "open and closed interval endpoints" $ do
    let rm = mapOf endpoints
    it "cuts at every endpoint and keeps open and closed ends exact" $
      summary rm `shouldBe`
        [ (["< 0"], []), (["[0..10)"], [0]), (["[10..20]"], [1]), (["(20..30)"], [2])
        , (["30"], []), (["> 30"], [3]) ]
    it "finds the two no-match regions: below 0, and exactly 30" $
      map showBlock . regionBlocks <$> noMatchRegions rm `shouldBe` [["< 0"], ["30"]]
    it "chooses a value some cell names where the block has one, else an integer" $
      map regionInput (regions rm) `shouldBe`
        [ [FNullary (VN n)] | n <- [-1, 0, 10, 21, 30, 31] ]
    it "passes the matcher self-check" $ selfCheck rm `shouldBe` []

  describe "a decimal block with no integer in it" $ do
    let rm = mapOf (table [r|
| U | x : Number | Band (out) |
|---|------------|------------|
| 1 | (1.1..1.2) | a          |
|])
    it "is represented by its midpoint, computed exactly" $ do
      summary rm `shouldBe` [(["<= 1.1"], []), (["(1.1..1.2)"], [0]), ([">= 1.2"], [])]
      regionInput <$> regions rm `shouldBe` [[FNullary (VN 1.1)], [FNullary (VN 1.15)], [FNullary (VN 1.2)]]
    it "passes the matcher self-check" $ selfCheck rm `shouldBe` []

  describe "conflicts under U" $ do
    let rm = mapOf uniqueOverlap
    it "names the one region where both rules match" $ do
      summary rm `shouldBe` [(["< 10"], [0]), (["[10..20]"], [0, 1]), (["> 20"], [1])]
      map showBlock . regionBlocks <$> conflictRegions rm `shouldBe` [["[10..20]"]]
    it "does not count a trailing catch-all, which D-22 reads as the default" $ do
      let nm = mapOf nearMiss
      rmDefault nm `shouldBe` TrailingCatchAll 4
      regionCount nm `shouldBe` 60
      summaryOf (conflictRegions nm) `shouldBe` [(["1.2", "\"premium\"", "\"Winter\""], [2, 3, 4])]
      noMatchRegions nm `shouldSatisfy` null
      selfCheck nm `shouldBe` []
    it "is not a conflict when the only overlap is with the catch-all" $ do
      let rm' = mapOf (table [r|
| U | x : Number | Band (out) |
|---|------------|------------|
| 1 | < 5        | low        |
| 2 | >= 5       | high       |
| 3 | -          | other      |
|])
      conflictRegions rm' `shouldSatisfy` null
      noMatchRegions rm' `shouldSatisfy` null
    it "reads the catch-all as the default even when a default is declared, as evalTable does" $ do
      -- D-22 part 1: evalTable's catchAllDefault <|> dtDefaultOutput. The
      -- declared default is unreachable, and the region agrees at every point.
      let both = mapOf nearMiss { dtDefaultOutput = Just [[FNullary (VS "declared")]] }
      rmDefault both `shouldBe` TrailingCatchAll 4
      map liveRules (conflictRegions both) `shouldBe` [[2, 3, 4]]
      noMatchRegions both `shouldSatisfy` null
      selfCheck both `shouldBe` []
    it "uses a declared default when there is no catch-all" $ do
      let declared = mapOf uniqueOverlap { dtDefaultOutput = Just [[FNullary (VN 0)]] }
      rmDefault declared `shouldBe` DeclaredDefault
      noMatchRegions declared `shouldSatisfy` null
      selfCheck declared `shouldBe` []

  describe "conflicts under A" $ do
    it "is a region where live rules disagree" $ do
      let rm = mapOf anyDisagree
      summary rm `shouldBe` [(["< 10"], [0, 1]), (["[10..20)"], [1]), ([">= 20"], [])]
      map showBlock . regionBlocks <$> conflictRegions rm `shouldBe` [["< 10"]]
      map showBlock . regionBlocks <$> noMatchRegions rm `shouldBe` [[">= 20"]]
    it "is not a region where live rules agree" $
      conflictRegions (mapOf anyAgree) `shouldSatisfy` null
    it "compares identical arithmetic outputs as equal" $
      conflictRegions (mapOf (table [r|
| A | x : Number | y : Number (out) |
|---|------------|------------------|
| 1 | < 10       | x * 2            |
| 2 | < 20       | x * 2            |
|])) `shouldSatisfy` null

  describe "F and P have no conflict regions" $ do
    it "F" $ conflictRegions (mapOf regCF) `shouldSatisfy` null
    it "P" $ conflictRegions (mapOf anyDisagree { hitpolicy = HP_Priority }) `shouldSatisfy` null

  describe "declared domains" $ do
    let rm = mapOf band
    it "drop values outside the domain, including the String 'other' block" $
      map (map showBlock . cbBlocks) (rmColumns rm) `shouldBe`
        [ ["[0..18)", "[18..65]", "(65..150]"]
        , ["\"LOW\"", "\"MEDIUM\"", "\"HIGH\""] ]
    it "find the four no-match regions of an F table with no catch-all" $
      summaryOf (noMatchRegions rm) `shouldBe`
        [ (["[18..65]", "\"LOW\""], []), (["[18..65]", "\"MEDIUM\""], [])
        , (["(65..150]", "\"MEDIUM\""], []), (["(65..150]", "\"HIGH\""], []) ]
    it "merge values the domain separates when no rule tells them apart" $ do
      -- policy/xml-eq-test-domain-warned: N may only be 1, 2 or 3, and no rule
      -- distinguishes them, so the three points are one block (step 0: 1 region).
      let rm' = mapOf (table [r|
| U | N : Number | R : String |
|---|------------|------------|
|   | 1, 2, 3    |            |
| 1 | = 9        | nine       |
| 2 | -          | other      |
|])
      map (map showBlock . cbBlocks) (rmColumns rm') `shouldBe` [["1, 2, 3"]]
      selfCheck rm' `shouldBe` []
    it "pass the matcher self-check" $ selfCheck rm `shouldBe` []

  describe "String columns" $ do
    it "give each literal a block, plus one for every other value" $ do
      let rm = mapOf nearMiss
      map (map showBlock . cbBlocks) (drop 1 (rmColumns rm)) `shouldBe`
        [ ["\"basic\"", "\"premium\"", "not(\"basic\", \"premium\")"]
        , ["\"Fall\"", "\"Winter\"", "\"Spring\"", "not(\"Fall\", \"Winter\", \"Spring\")"] ]
    it "treat a negation as a set" $ do
      let rm = mapOf negatedString
      summary rm `shouldBe` [(["\"Fall\""], [1]), (["not(\"Fall\")"], [0])]
      selfCheck rm `shouldBe` []
    it "merge literals no rule tells apart" $
      map (map showBlock . cbBlocks) (rmColumns (mapOf (table [r|
| U | Season         | Dish (out) |
|---|----------------|------------|
| 1 | Fall, Winter   | stew       |
| 2 | Spring         | salad      |
|]))) `shouldBe` [["\"Fall\", \"Winter\"", "\"Spring\"", "not(\"Fall\", \"Winter\", \"Spring\")"]]
    it "keep text that merely contains punctuation" $
      -- policy/md-quoted-literal-all-or-nothing: 5' 10" and Non-Participating
      -- are values, not FEEL, and must not be refused as FEEL-shaped.
      kindOf (table [r|
| U | Season            | Dish (out) |
|---|-------------------|------------|
| 1 | "Fall", "Winter"  | Spareribs  |
| 2 | "Spring"          | Steak      |
| 3 | 5' 10"            | Tall       |
| 4 | Non-Participating | Nothing    |
|]) `shouldBe` Right ()

  describe "zero input columns" $
    it "is one region in which every rule is live, and a U table of two rules has a conflict there and no default" $ do
      -- A trailing catch-all needs an input column to be a wildcard in
      -- (uniqueCatchAll), so nothing is split off as the default and both
      -- rules are live. Until audit 10 f8 this test said the last row was the
      -- default (TrailingCatchAll 1) and there was no conflict.
      let dt = DTable "Z" HP_Unique [DTCH DTCH_Out "o" (Just DMN_String) Nothing]
                 [ DTrow (Just 1) [] [[FNullary (VS "a")]] [], DTrow (Just 2) [] [[FNullary (VS "b")]] [] ]
                 Nothing
          rm = mapOf dt
      map liveRules (regions rm) `shouldBe` [[0, 1]]
      rmDefault rm `shouldBe` NoDefault
      length (conflictRegions rm) `shouldBe` 1
      selfCheck rm `shouldBe` []
      conflictRegions (mapOf dt { allrows = take 1 (allrows dt) }) `shouldSatisfy` null
  where
    summaryOf rs = [ (showBlock <$> regionBlocks r, liveRules r) | r <- rs ]

-- * Unsupported shapes

unsupported :: Spec
unsupported = describe "unsupported shapes are a Left, never a partial answer" $ do
  it "C, R and O are list-valued" $
    mapM_ (\hp -> kindOf anyAgree { hitpolicy = hp } `shouldBe` Left ListValuedHitPolicy)
      [HP_Collect Collect_All, HP_Collect Collect_Sum, HP_Collect Collect_Cnt, HP_RuleOrder, HP_OutputOrder]
  it "a collection input column" $
    kindOf (table [r|
| U | tags : [Number] | Score (out) |
|---|-----------------|-------------|
| 1 | 1               | a           |
|]) `shouldBe` Left CollectionColumn
  it "a computed input cell" $
    kindOf DTable { tableName = "C", hitpolicy = HP_Unique
                  , header = [DTCH DTCH_In "x" (Just DMN_Number) Nothing, DTCH DTCH_Out "o" (Just DMN_Number) Nothing]
                  , allrows = [DTrow (Just 1) [[FFunction (FNF3 (FNF1 "x") FNMul (FNF0 (VN 2)))]] [[FNullary (VN 1)]] []]
                  , dtDefaultOutput = Nothing }
      `shouldBe` Left ComputedInputCell
  describe "a String cell holding FEEL test syntax" $ do
    it "a comparison (symptom/infer-explicit-type-contradiction-silent)" $
      kindOf (table [r|
| U | Guest Count : String | Dish (out) |
|---|----------------------|------------|
| 1 | <= 8                 | Spareribs  |
| 2 | > 8                  | Stew       |
|]) `shouldBe` Left FeelShapedStringCell
    it "a negation (symptom/md-negation-in-string-column-silent)" $
      kindOf (table [r|
| U | Season : String | Dish (out) |
|---|-----------------|------------|
| 1 | not(Fall)       | stew       |
| 2 | Fall            | spareribs  |
|]) `shouldBe` Left FeelShapedStringCell
    it "arithmetic over a column name (symptom/md-string-col-arith-dead-rule)" $
      kindOf (table [r|
| U | Age : String | Result : Number |
|---|---|---|
| 1 | (Age * 2) + 1 | 5 |
|]) `shouldBe` Left FeelShapedStringCell
  -- Built by hand: the markdown reader refuses a row short of an input or
  -- output column itself since audit 10 f2 (policy/struct-short-row-refused),
  -- so no fixture reaches this any more.
  it "a short row, which the matcher would read as wildcards" $
    kindOf DTable { tableName = "ShortRow", hitpolicy = HP_Unique
                  , header = [ DTCH DTCH_In "Season" (Just DMN_String) Nothing
                             , DTCH DTCH_In "Guests" (Just DMN_Number) Nothing
                             , DTCH DTCH_Out "Dish" (Just DMN_String) Nothing ]
                  , allrows = [ DTrow (Just 1) [[FNullary (VS "Fall")], [FSection Flt (VN 8)]] [[FNullary (VS "Stew")]] []
                              , DTrow (Just 2) [[FNullary (VS "Winter")]] [[FNullary (VS "Soup")]] []
                              , DTrow (Just 3) [[FNullary (VS "Spring")], [FSection Flt (VN 4)]] [[FNullary (VS "Salad")]] [] ]
                  , dtDefaultOutput = Nothing }
      `shouldBe` Left RowArity
  it "an ordering comparison against a String, which fEval has no arm for" $
    kindOf negatedString { allrows = [DTrow (Just 1) [[FSection Flt (VS "m")]] [[FNullary (VS "x")]] []] }
      `shouldBe` Left CellTypeMismatch
  it "an A table whose overlapping rules give different arithmetic" $
    kindOf (table [r|
| A | x : Number | y : Number (out) |
|---|------------|------------------|
| 1 | < 10       | x * 2            |
| 2 | < 20       | 5                |
|]) `shouldBe` Left UncomparableAnyOutputs
  it "but not one whose differing arithmetic rules never overlap" $
    kindOf (table [r|
| A | x : Number | y : Number (out) |
|---|------------|------------------|
| 1 | < 10       | x * 2            |
| 2 | >= 10      | 5                |
|]) `shouldBe` Right ()
  it "names the table, column and row in the message" $
    either unsupportedMessage (const "") (regionMap (table [r|
| U | Season : String | Dish (out) |
|---|-----------------|------------|
| 1 | not(Fall)       | stew       |
|])) `shouldSatisfy` ("table \"T\": column \"Season\": row 1: " `isPrefixOf`)

-- * Refusing conflicts

-- | @policy/md-eval-unique-conflict@'s table.
overlapping :: DecisionTable
overlapping = table [r|
| U | Season | Guests | Dish (out) |
|---|--------|--------|------------|
| 1 | Fall   | <= 20  | Spareribs  |
| 2 | Fall   | >= 10  | Stew       |
|]

-- | @policy/hp-unique-overlap-comparisons-refused@'s table, which was
-- @policy/md-prefix-comparisons@'s before D-22 part 2: two separate
-- overlaps, with a trailing catch-all that is in neither.
prefixComparisons :: DecisionTable
prefixComparisons = table [r|
| U | Age   | Band (out) |
|---|-------|------------|
| 1 | < 18  | minor      |
| 2 | <= 21 | young      |
| 3 | > 65  | senior     |
| 4 | >= 40 | middle     |
| 5 | -     | adult      |
|]

-- | @policy/hp-unique-overlap-multivalue-dash-refused@'s table, which was
-- @policy/md-multivalue-dash-reprocessed@'s before D-22 part 2.
multiDash :: DecisionTable
multiDash = table [r|
| U | Guest Count | Dish (out) |
|---|-------------|------------|
| 1 | 4, -        | Spareribs  |
| 2 | 8           | Stew       |
|]

-- | @policy/hp-unique-overlap-negation-refused@'s table, which was
-- @policy/md-negation-in-numeric-column-emitted@'s before D-22 part 2.
notRange :: DecisionTable
notRange = table [r|
| U | Age         | Band (out) |
|---|-------------|------------|
| 1 | not([1..5]) | outside    |
| 2 | [10..20]    | either     |
|]

-- | Two negated String cells, built directly for the reason 'negatedString' is.
twoNegations :: DecisionTable
twoNegations = negatedString
  { allrows = [ DTrow (Just 1) [[FNot (FNullary (VS "Fall"))]]   [[FNullary (VS "stew")]] []
              , DTrow (Just 2) [[FNot (FNullary (VS "Winter"))]] [[FNullary (VS "salad")]] [] ] }

-- | A U table with no input column, and @n@ rows.
noInputs :: Int -> DecisionTable
noInputs n = DTable "Z" HP_Unique [DTCH DTCH_Out "o" (Just DMN_String) Nothing]
  [ DTrow (Just k) [] [[FNullary (VS (show k))]] [] | k <- [1 .. n] ] Nothing

refusing :: Spec
refusing = describe "refusing conflict regions (D-22 rule 2)" $ do
  let pairs rm = [ (conflictEarlier c, conflictLater c) | c <- conflicts rm ]
      uniqueAdvice = ": a table with hit policy Unique must not contain overlapping rules"
        ++ " (DMN 1.3 §8.2.10). Change an input cell of row 1 or row 2 so that the two"
        ++ " rules select different inputs or, if the earlier rule is meant to win,"
        ++ " make the hit policy F (First)."
      startsWith prefixes msgs = length msgs == length prefixes && and (zipWith isPrefixOf prefixes msgs)

  describe "conflicts" $ do
    it "pairs each later rule with the first earlier rule it can match alongside" $ do
      let rm = mapOf (table [r|
| U | Age   | Band (out) |
|---|-------|------------|
| 1 | <= 20 | a          |
| 2 | >= 10 | b          |
| 3 | 15    | c          |
|])
      pairs rm `shouldBe` [(0, 1), (0, 2)]
      selfCheck rm `shouldBe` []
    it "takes its witness from the first region in which both rules are live" $
      map (regionInput . conflictRegion) (conflicts (mapOf uniqueOverlap)) `shouldBe` [[FNullary (VN 10)]]
    it "never pairs a rule with a trailing catch-all, which D-22 reads as the default" $
      pairs (mapOf nearMiss) `shouldBe` [(2, 3)]
    it "under A, pairs only rules whose outputs differ" $ do
      pairs (mapOf anyDisagree) `shouldBe` [(0, 1)]
      pairs (mapOf anyAgree) `shouldBe` []
    it "under F and P, pairs nothing" $ do
      pairs (mapOf regCF) `shouldBe` []
      pairs (mapOf anyDisagree { hitpolicy = HP_Priority }) `shouldBe` []
    it "agrees with enumerating every region, on every hand-written table" $
      concatMap selfCheck (mapOf <$> [ uniqueOverlap, anyDisagree, anyAgree, nearMiss, overlapping
                                     , prefixComparisons, multiDash, notRange, twoNegations, noInputs 3 ])
        `shouldBe` []

  describe "conflictErrors" $ do
    it "under U, names both rules, one witness input, and where they overlap" $
      conflictErrors uniqueOverlap `shouldBe`
        [ "row 1 and row 2 both match Age = 10 (they overlap wherever Age is in [10..20])" ++ uniqueAdvice ]
    it "under A, also names the outputs that disagree" $
      conflictErrors anyDisagree `shouldBe`
        [ "row 1 and row 2 both match Age = 9 (they overlap wherever Age < 10) and disagree on"
          ++ " Verdict (\"ok\" against \"deny\"): under hit policy A (Any), rules may overlap only"
          ++ " where their outputs agree (DMN 1.3 §8.2.10). Give the two rules the same outputs,"
          ++ " or change an input cell of row 1 or row 2 so that they select different inputs." ]
    it "names only the output columns that disagree" $
      conflictErrors (table [r|
| A | Age  | Verdict (out) | Fee : Number (out) |
|---|------|---------------|--------------------|
| 1 | < 10 | ok            | 5                  |
| 2 | < 20 | ok            | 10                 |
|]) `shouldSatisfy` startsWith ["row 1 and row 2 both match Age = 9 (they overlap wherever Age < 10) and disagree on Fee (5 against 10): "]
    it "gives the witness in every input column, and the overlap in each column it narrows" $
      conflictErrors overlapping `shouldSatisfy` startsWith
        ["row 1 and row 2 both match Season = \"Fall\", Guests = 10 (they overlap wherever Season = \"Fall\" and Guests is in [10..20]): "]
    it "leaves the overlap out when it is the witness and nothing more" $
      conflictErrors nearMiss `shouldSatisfy` startsWith
        ["row 3 and row 4 both match Version = 1.2, Tier = \"premium\", Season = \"Winter\": "]
    it "reports every conflicting pair, and leaves the trailing catch-all out of all of them" $
      conflictErrors prefixComparisons `shouldSatisfy` startsWith
        [ "row 1 and row 2 both match Age = 17 (they overlap wherever Age < 18): "
        , "row 3 and row 4 both match Age = 66 (they overlap wherever Age > 65): " ]
    it "reads a multi-value cell holding a dash as matching everything" $
      conflictErrors multiDash `shouldSatisfy` startsWith ["row 1 and row 2 both match Guest Count = 8: "]
    it "reads a negation as its complement" $
      conflictErrors notRange `shouldSatisfy` startsWith
        ["row 1 and row 2 both match Age = 10 (they overlap wherever Age is in [10..20]): "]
    it "describes a String overlap by what it leaves out" $
      conflictErrors twoNegations `shouldSatisfy` startsWith
        ["row 1 and row 2 both match Season = \"other\" (they overlap wherever Season is none of \"Fall\", \"Winter\"): "]
    it "describes a Number overlap of several intervals as a list of tests" $
      conflictErrors (table [r|
| U | Age       | Band (out) |
|---|-----------|------------|
| 1 | < 5, > 10 | a          |
| 2 | -         | b          |
| 3 | 7         | c          |
|]) `shouldSatisfy` startsWith
        [ "row 1 and row 2 both match Age = 4 (they overlap wherever Age matches < 5, > 10): "
        , "row 2 and row 3 both match Age = 7: " ]
    it "says every input when the table has no input column, and says there is none to tell the rules apart" $ do
      conflictErrors (noInputs 3) `shouldSatisfy` startsWith ["row 1 and row 2 both match every input: ", "row 1 and row 3 both match every input: "]
      conflictErrors (noInputs 2) `shouldSatisfy` startsWith ["row 1 and row 2 both match every input: "]
      conflictErrors (noInputs 2) `shouldSatisfy` all ("This table has no input column, so every rule matches every input." `isInfixOf`)
      conflictErrors (noInputs 1) `shouldBe` []

  describe "beside D-13's uniquenessErrors, in tableErrors" $ do
    it "leaves two rows with identical guards to D-13, which says more about them" $ do
      let dup = table [r|
| U | Season | Dish (out) |
|---|--------|------------|
| 1 | Fall   | stew       |
| 2 | Fall   | salad      |
|]
      conflictErrors dup `shouldBe` []
      tableErrors dup `shouldBe` uniquenessErrors dup
      length (tableErrors dup) `shouldBe` 1
    it "says nothing about a row D-13 reports, even to pair it with a different row" $ do
      let t = table [r|
| U | Age   | Band (out) |
|---|-------|------------|
| 1 | <= 20 | a          |
| 2 | 5     | b          |
| 3 | 5     | c          |
|]
      map (take 17) (uniquenessErrors t) `shouldBe` ["row 2 and row 3 h"]
      map (take 17) (conflictErrors t) `shouldBe` ["row 1 and row 2 b"]
      length (tableErrors t) `shouldBe` 2
    it "is silent where regions cannot be computed, and D-13 still refuses identical guards there" $ do
      let coll = table [r|
| U | tags : [Number] | grade (out) |
|---|-----------------|-------------|
| 1 | 5               | pass        |
| 2 | 5               | fail        |
|]
      conflictErrors coll `shouldBe` []
      tableErrors coll `shouldBe` uniquenessErrors coll
      length (tableErrors coll) `shouldBe` 1
    it "keeps the collection-input overlap warning under U and A, and refuses neither" $
      mapM_ (\hp -> do
        let coll = (table [r|
| U | tags : [Number] | grade (out) |
|---|-----------------|-------------|
| 1 | 5               | pass        |
| 2 | 7               | fail        |
|]) { hitpolicy = hp }
        tableErrors coll `shouldBe` []
        tableWarnings coll `shouldSatisfy` any ("membership tests over disjoint values still overlap" `isInfixOf`))
        [HP_Unique, HP_Any]

  describe "the markdown reader" $
    it "refuses the table with a located Error, and returns no table" $ do
      let (diags, tables) = either error id (parseOnly (parseTableD "T") (T.pack (dropWhile (== '\n') [r|
| U | Age   | Fee : Number |
|---|-------|--------------|
| 1 | <= 20 | 5            |
| 2 | >= 10 | 10           |
|])))
      map tableName tables `shouldBe` []
      [ diagMessage d | d <- diags, diagSeverity d == Error ] `shouldBe`
        [ "table \"T\": row 1 and row 2 both match Age = 10 (they overlap wherever Age is in [10..20])" ++ uniqueAdvice ]

-- * The corpus

-- | The fixtures @test/roundtrip/run-roundtrip.sh@ iterates, in its
-- @collect_fixtures@ order, deduplicated, keyed by its @slug_for@ slug.
roundtripFixtures :: IO [(String, FilePath)]
roundtripFixtures = do
  mds <- sort . filter ((/= "README.md") . takeFileName) <$> findMd "test"
  let paths = nub (["../../README.md"] ++ mds ++ ["test/golden/README.md"])
  pure [ (slug p, p) | p <- paths ]
  where
    findMd dir = do
      entries <- map (dir </>) . sort <$> listDirectory dir
      dirs    <- filterM doesDirectoryExist entries
      deeper  <- concat <$> mapM findMd dirs
      pure ([ e | e <- entries, e `notElem` dirs, ".md" `isSuffixOf'` e ] ++ deeper)
    isSuffixOf' suf s = reverse suf `isPrefixOf` reverse s
    slug p
      | p == "../../README.md" = "README.md"
      | otherwise = stripSuffix "/input.md" (strip "corpus/cases/" (strip "test/" p))
    strip pre s = if pre `isPrefixOf` s then drop (length pre) s else s
    stripSuffix suf s = if suf `isSuffixOf'` s then take (length s - length suf) s else s

-- | Run an action with stderr sent to /dev/null: the markdown reader prints a
-- @note:@ for every prose pipe table it skips, which is not this test's business.
quietly :: IO a -> IO a
quietly act = bracket acquire release (const act)
  where
    acquire = do
      saved <- hDuplicate stderr
      devnull <- openFile "/dev/null" WriteMode
      hDuplicateTo devnull stderr
      hClose devnull
      pure saved
    release saved = hDuplicateTo saved stderr >> hClose saved

-- | Enough forcing that a reader exception surfaces inside the 'try'.
force' :: ([Diagnostic], [DecisionTable]) -> ([Diagnostic], [DecisionTable])
force' r@(ds, ts) = length (show ts) `seq` length (concatMap diagMessage ds) `seq` r

readFixture :: FilePath -> IO ([Diagnostic], [DecisionTable])
readFixture path = parseMarkdown ArgOptions
  { verbose = False, query = False, propstyle = False, informat = Md
  , outformat = Unknown, out = "-", pick = "", input = [path] }

-- | The fixtures the reader refuses for a conflict region (D-22 rule 2).
--
-- ROOTSTOCK step 0 found seven tables with a conflict region, and D-22 part 1
-- re-fixtured an eighth (@policy\/md-eval-unique-conflict@) to have one. Part 2
-- made the reader refuse all eight. Four of them had been written to pin
-- something else, so each was re-fixtured without its overlap and its old
-- table moved to a new @hp-unique-overlap-*@ case; the other four are refusal
-- cases now. The XML reader's copy of the refusal is
-- @policy\/xml-unique-overlap-refused@, which this markdown-only walk does not
-- read.
expectedConflictRefusals :: [String]
expectedConflictRefusals = sort
  [ "policy/md-eval-unique-conflict"
  , "policy/eval-hp-any-two-rows-disagree"
  , "policy/l4-hitpolicy-unique-silently-first"
  , "policy/hp-any-duplicate-rows-disagree-silent"
  , "policy/hp-unique-overlap-shared-member-refused"
  , "policy/hp-unique-overlap-comparisons-refused"
  , "policy/hp-unique-overlap-multivalue-dash-refused"
  , "policy/hp-unique-overlap-negation-refused"
  , "policy/md-zero-input-unique-refused"
  ]

corpus :: Spec
corpus = describe "the round-trip fixture corpus" $ do
  fixtures <- runIO roundtripFixtures
  results  <- runIO $ quietly $ forM fixtures $ \(s, p) -> do
    r <- try (readFixture p >>= evaluate . force')
    pure $ case r of
      Left (e :: SomeException) -> Left (s, displayException e)
      Right (ds, ts)            -> Right (s, ds, [ (s, tableName dt, regionMap dt) | dt <- ts ])
  let tables      = concat [ ts | Right (_, _, ts) <- results ]
      -- A conflict refusal is the one Error whose text says two rows "both match".
      refusedForConflict = sort
        [ s | Right (s, ds, _) <- results
            , any (\d -> diagSeverity d == Error && " both match " `isInfixOf` diagMessage d) ds ]
      analysed    = [ (s, n, rm) | (s, n, Right rm) <- tables ]
      refused     = [ unsupportedKind u | (_, _, Left u) <- tables ]
      byKind      = M.toList (M.fromListWith (+) [ (k, 1 :: Int) | k <- refused ])
      conflicted  = sort [ (s, n) | (s, n, rm) <- analysed, not (null (conflictRegions rm)) ]
      noMatchers  = [ () | (_, _, rm) <- analysed, not (null (noMatchRegions rm)) ]
      nRegions    = sum [ regionCount rm | (_, _, rm) <- analysed ]
      nConflicts  = sum [ length (conflictRegions rm) | (_, _, rm) <- analysed ]
      nNoMatch    = sum [ length (noMatchRegions rm) | (_, _, rm) <- analysed ]
  it ("reads " ++ show (length fixtures) ++ " fixtures and analyses "
        ++ show (length analysed) ++ " tables, " ++ show nRegions ++ " regions") $ do
    [ e | Left e <- results ] `shouldBe` []
    length analysed `shouldSatisfy` (> 100)
  it "refuses, for a conflict region, exactly the fixtures that have one" $
    refusedForConflict `shouldBe` expectedConflictRefusals
  it ("leaves no conflict region in any table it accepts (" ++ show nConflicts ++ " found)") $
    conflicted `shouldBe` []
  it ("finds " ++ show nNoMatch ++ " no-match regions in " ++ show (length noMatchers) ++ " tables") $
    length noMatchers `shouldSatisfy` (> 0)
  -- The kinds, not the counts: a new fixture should not break this, but a new
  -- KIND of refusal in the corpus is a finding and should be read. Step 0
  -- skipped three of these and read a short row's missing cells as "-".
  -- 'RowArity' is not in the list: two fixtures used to reach it, and the
  -- markdown reader now refuses a row short of an input column before regions
  -- are computed.
  it ("refuses three kinds of table and no others: " ++ show byKind) $
    map fst byKind `shouldBe` [ListValuedHitPolicy, CollectionColumn, FeelShapedStringCell]
  it "agrees with dmnmd's matcher and with evalTable at every region representative" $
    [ (s, n, e) | (s, n, rm) <- analysed, e <- take 3 (selfCheck rm) ] `shouldBe` []
