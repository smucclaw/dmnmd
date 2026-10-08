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
--    so 'fEval') selects exactly the region's live set, and the pruned
--    enumerations agree with filtering the full one;
--  * the corpus measurement, which pins the set of tables with a conflict
--    region to the seven that ROOTSTOCK step 0 found.
--
-- The corpus is read with @app\/ParseMarkdown@, the binary's own reader, so a
-- table means here what it means to @dmnmd@.
module RegionsSpec (regionsSpec) where

import           Control.Applicative  ((<|>))
import           Control.Exception    (SomeException, bracket, displayException, evaluate, try)
import           Control.Monad        (filterM, forM)
import           Data.Either          (isLeft)
import           Data.List            (isPrefixOf, nub, sort)
import           Data.Maybe           (listToMaybe)
import qualified Data.Map.Strict      as M
import qualified Data.Text            as T
import           GHC.IO.Handle        (hDuplicate, hDuplicateTo)
import           System.Directory     (doesDirectoryExist, listDirectory)
import           System.FilePath      ((</>), takeFileName)
import           System.IO            (IOMode (WriteMode), hClose, openFile, stderr)
import           Test.Hspec
import           Text.RawString.QQ

import           DMN.DecisionTable    (evalTable, fEvals, fNEval, matches, mkFsEither, outputOrder)
import           DMN.ParseTable       (parseTable)
import           DMN.ParsingUtils     (parseOnly)
import           DMN.Regions
import           DMN.Types
import           Options              (ArgOptions (..), FileFormat (..))
import           ParseMarkdown        (parseMarkdown)

regionsSpec :: Spec
regionsSpec = describe "DMN.Regions" $ do
  handCounted
  unsupported
  corpus

-- * Helpers

table :: String -> DecisionTable
table = either error id . parseOnly (parseTable "T") . T.pack . dropWhile (== '\n')

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

-- | @symptom/l4-hitpolicy-unique-silently-first@'s table.
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

-- | @policy/hp-unique-near-duplicate-rows-accepted@'s table: rows 3 and 4
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
    it "is one region in which every rule is live, and a U table's last row is its default" $ do
      -- uniqueCatchAll's predicate holds vacuously with no input columns, so
      -- evalTable answers "a" from row 1 and keeps "b" as the default.
      let dt = DTable "Z" HP_Unique [DTCH DTCH_Out "o" (Just DMN_String) Nothing]
                 [ DTrow (Just 1) [] [[FNullary (VS "a")]] [], DTrow (Just 2) [] [[FNullary (VS "b")]] [] ]
                 Nothing
          rm = mapOf dt
      map liveRules (regions rm) `shouldBe` [[0, 1]]
      rmDefault rm `shouldBe` TrailingCatchAll 1
      conflictRegions rm `shouldSatisfy` null
      selfCheck rm `shouldBe` []
      conflictRegions (mapOf dt { allrows = allrows dt ++ [DTrow (Just 3) [] [[FNullary (VS "c")]] []] })
        `shouldSatisfy` ((== 1) . length)
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
  it "a short row, which the matcher would read as wildcards (symptom/struct-short-row-truncated)" $
    kindOf (table [r|
| U | Season | Guests | Dish (out) |
|---|--------|--------|------------|
| 1 | Fall   | <= 8   | Stew       |
| 2 | Winter |
| 3 | Spring | <= 4   | Salad      |
|]) `shouldBe` Left RowArity
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
force' :: [DecisionTable] -> [DecisionTable]
force' ts = length (show ts) `seq` ts

readFixture :: FilePath -> IO [DecisionTable]
readFixture path = snd <$> parseMarkdown ArgOptions
  { verbose = False, query = False, propstyle = False, informat = Md
  , outformat = Unknown, out = "-", pick = "", input = [path] }

-- | The seven tables ROOTSTOCK step 0 found with a conflict region, which D-22
-- lists as the cases that move when part 2 lands, and one more.
-- @policy\/md-eval-unique-conflict@ held a catch-all when step 0 ran; D-22
-- part 1 re-fixtured it with a genuine overlap (@<= 20@ \/ @>= 10@) so that
-- the run-time refusal stays pinned, and its old table survives, conflict-free,
-- as @policy\/md-eval-unique-catchall-default@.
expectedConflictTables :: [(String, String)]
expectedConflictTables = sort
  [ ("policy/md-eval-unique-conflict",                "Overlapping")
  , ("policy/hp-unique-near-duplicate-rows-accepted", "NearMiss")
  , ("policy/md-prefix-comparisons",                  "PrefixComparisons")
  , ("policy/md-multivalue-dash-reprocessed",         "MultiDash")
  , ("policy/md-negation-in-numeric-column-emitted",  "NotRange")
  , ("policy/eval-hp-any-two-rows-disagree",          "AnyDisagree")
  , ("symptom/l4-hitpolicy-unique-silently-first",    "UniqueOverlap")
  , ("symptom/hp-any-duplicate-rows-disagree-silent", "AnyDup")
  ]

corpus :: Spec
corpus = describe "the round-trip fixture corpus" $ do
  fixtures <- runIO roundtripFixtures
  results  <- runIO $ quietly $ forM fixtures $ \(s, p) -> do
    r <- try (readFixture p >>= evaluate . force')
    pure $ case r of
      Left (e :: SomeException) -> Left (s, displayException e)
      Right ts                  -> Right [ (s, tableName dt, regionMap dt) | dt <- ts ]
  let tables      = concat [ ts | Right ts <- results ]
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
  it ("finds conflict regions in exactly step 0's seven tables and D-22's re-fixtured one ("
        ++ show nConflicts ++ " conflict regions)") $
    conflicted `shouldBe` expectedConflictTables
  it ("finds " ++ show nNoMatch ++ " no-match regions in " ++ show (length noMatchers) ++ " tables") $
    length noMatchers `shouldSatisfy` (> 0)
  -- The kinds, not the counts: a new fixture should not break this, but a new
  -- KIND of refusal in the corpus is a finding and should be read. Step 0
  -- skipped three of these and read a short row's missing cells as "-".
  it ("refuses four kinds of table and no others: " ++ show byKind) $
    map fst byKind `shouldBe` [ListValuedHitPolicy, RowArity, CollectionColumn, FeelShapedStringCell]
  it "agrees with dmnmd's matcher and with evalTable at every region representative" $
    [ (s, n, e) | (s, n, rm) <- analysed, e <- take 3 (selfCheck rm) ] `shouldBe` []
