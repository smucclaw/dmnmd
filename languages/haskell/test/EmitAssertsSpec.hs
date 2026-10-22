{-# LANGUAGE LambdaCase, QuasiQuotes, ScopedTypeVariables #-}

-- | Tests for @L4Opts.emitAsserts@ (@DECISIONS.md@ D-21 step 2): one @#ASSERT@
-- per region of "DMN.Regions", saying that the emitted function at the region's
-- input equals what 'DMN.DecisionTable.evalTable' answers there.
--
-- Two layers.
--
--  * Pure, always run: the positions an over-cap table is sampled at, the exact
--    text for each kind of result (a scalar, a @MAYBE@, a record, a sum type, a
--    table with no input column), and each of the cases that must say something
--    instead of asserting.
--
--  * A cross-check against the real @l4@, pending when there is none, exactly as
--    "TranslateL4Spec"'s semantic gate is: every markdown fixture that
--    @--to=l4@ accepts is emitted with assertions, must typecheck, and @l4 run@
--    must report every assertion true. A false one is a real disagreement
--    between the L4 emitter and the interpreter, so none is tolerated except the
--    named exceptions below, each with its reason.
--
-- The cross-check can fail: 'canary' corrupts one emitted guard and checks the
-- harness reports it.
module EmitAssertsSpec (emitAssertsSpec) where

import           Control.Monad        (forM)
import           Data.List            (isInfixOf, isPrefixOf, isSuffixOf, nub, tails)
import           Data.Maybe           (isJust)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           System.Directory     (createDirectoryIfMissing)
import           System.FilePath      ((</>))
import           System.Process       (readProcessWithExitCode)
import           Test.Hspec
import           Text.RawString.QQ

import           DMN.Diagnostic       (Diagnostic (..), Severity (..))
import           DMN.ParseTable       (parseTable)
import           DMN.ParsingUtils     (parseOnly)
import           DMN.Regions          (firstRegionOf, regionCount, regionMap)
import           DMN.Translate.L4
import           DMN.Types

import           RegionsSpec          (force', quietly, readFixture, roundtripFixtures)
import           TranslateL4Spec      (withL4)

parse :: String -> Text -> DecisionTable
parse name = either error id . parseOnly (parseTable name)

withAsserts :: L4Opts
withAsserts = defaultL4Opts { emitAsserts = True }

-- | The @#ASSERT@ lines of an emission.
assertsOf :: String -> [String]
assertsOf = filter ("#ASSERT" `isPrefixOf`) . lines

-- | The @-- emitAsserts:@ comment lines of an emission.
notesOf :: String -> [String]
notesOf = filter ("-- emitAsserts:" `isPrefixOf`) . lines

-- * Tables

-- | No catch-all and gaps, so the result is a @MAYBE@ and some region answers
-- @NOTHING@. A negative representative must be parenthesised.
gapTable :: Text
gapTable = T.pack $ dropWhile (== '\n') [r|
| F | x : Number | label : String (out) |
|---+------------+----------------------|
| 1 | > 10       | big                  |
| 2 | < 0        | negative             |
|]

-- | A total table, so bare; a boolean input and a decimal output.
totalTable :: Text
totalTable = T.pack $ dropWhile (== '\n') [r|
| U | ok : Boolean | rate : Number (out) |
|---+--------------+---------------------|
| 1 | true         | 0.25                |
| 2 | false        | 1                   |
|]

-- | Two outputs, so a record; an input column with a space in its name; a
-- negative output, which the record constructor takes as an argument.
recordTable :: Text
recordTable = T.pack $ dropWhile (== '\n') [r|
| F | age : Number | the name : String | delta : Number (out) | label : String (out) |
|---+--------------+-------------------+----------------------+----------------------|
| 1 | < 18         | Bob               | -2.5                 | minor Bob            |
| 2 | >= 18        | -                 | 3                    | adult                |
|]

-- | A sum-typed input and a sum-typed output. The subheader row declares both
-- domains, so every region's input is a member and a constructor.
enumTable :: Text
enumTable = T.pack $ dropWhile (== '\n') [r|
| U | cat : String   | card : String (out) |
|---+----------------+---------------------|
|   | Dining, Travel | Amex, PRVI          |
| 1 | Dining         | PRVI                |
| 2 | Travel         | Amex                |
|]

-- | The corpus table @md-prefix-comparisons@. 'DMN.Regions' cuts Age at 18 and
-- 21 and gives each its own point piece, but the pieces [18], (18, 21) and [21]
-- are live for the same rules, so they are ONE block, [18..21], and a block has
-- one representative: its lowest named point, 18. Age = 21, the closed upper
-- bound of rule 3, is a value of that block no assertion visits.
prefixTable :: Text
prefixTable = T.pack $ dropWhile (== '\n') [r|
| U | Age   | Scheme | Band (out) |
|---|-------|--------|------------|
| 1 | < 18  | child  | minor      |
| 2 | >= 18 | child  | adult      |
| 3 | <= 21 | youth  | young      |
| 4 | > 21  | youth  | adult      |
|]

-- | Three columns cut like 'prefixTable''s Age: three blocks each, the middle one
-- [18..21]. 27 regions, and the 19 that are not all in an outer block have a
-- second input at 21.
bandsTable :: Text
bandsTable = T.pack $ dropWhile (== '\n') [r|
| F | a     | b     | c     | out (out) |
|---+-------+-------+-------+-----------|
| 1 | < 18  | < 18  | < 18  | r1        |
| 2 | >= 18 | >= 18 | >= 18 | r2        |
| 3 | <= 21 | <= 21 | <= 21 | r3        |
| 4 | > 21  | > 21  | > 21  | r4        |
|]

-- | No input column: one region, a function of no arguments.
noInputTable :: Text
noInputTable = T.pack $ dropWhile (== '\n') [r|
| F | greeting : String (out) |
|---+-------------------------|
| 1 | hello                   |
|]

-- | A collection input, which 'regionMap' declines.
collectionTable :: Text
collectionTable = T.pack $ dropWhile (== '\n') [r|
| F | roles : [String] | access : String (out) |
|---+------------------+-----------------------|
| 1 | admin            | full                  |
| 2 | -                | none                  |
|]

-- | Division by an input that a region represents as 0.
quotientTable :: Text
quotientTable = T.pack $ dropWhile (== '\n') [r|
| F | Amount : Number | Divisor : Number | Quotient : Number (out) |
|---+-----------------+------------------+-------------------------|
| 1 | -               | -                | Amount / Divisor        |
|]

-- | Three columns of fifteen blocks each (seven points and the eight gaps around
-- them), so 15^3 = 3375 regions, over any small cap.
bigTable :: Text
bigTable = T.pack $ dropWhile (== '\n') [r|
| F | a : Number | b : Number | c : Number | out : Number (out) |
|---+------------+------------+------------+--------------------|
| 1 | 1          | 1          | 1          | 1                  |
| 2 | 2          | 2          | 2          | 2                  |
| 3 | 3          | 3          | 3          | 3                  |
| 4 | 4          | 4          | 4          | 4                  |
| 5 | 5          | 5          | 5          | 5                  |
| 6 | 6          | 6          | 6          | 6                  |
| 7 | 7          | 7          | 7          | 7                  |
|]

-- * The spec

emitAssertsSpec :: Spec
emitAssertsSpec = describe "DMN.Translate.L4 emitAsserts (D-21 step 2)" $ do
  positions
  text
  unanalysed
  capping
  secondInputs
  crossCheck

positions :: Spec
positions = describe "assertPositions" $ do
  it "takes every region when they fit under the cap" $ do
    assertPositions 5 3 [] `shouldBe` [0, 1, 2]
    assertPositions 3 3 [1] `shouldBe` [0, 1, 2]
  it "is empty for a table with no regions or a cap of 0" $ do
    assertPositions 5 0 [] `shouldBe` []
    assertPositions 0 7 [] `shouldBe` []
  it "takes the first region alone for a cap of 1" $
    assertPositions 1 9 [4] `shouldBe` [0]
  it "spreads a cap's worth evenly, the first and last included, when no rule has a witness" $
    assertPositions 4 10 [] `shouldBe` [0, 3, 6, 9]
  it "adds the witnesses of the first rules, and spaces the rest of the cap" $
    assertPositions 10 100 [5, 40, 77] `shouldBe` [0, 5, 16, 33, 40, 49, 66, 77, 82, 99]
  it "gives the witnesses at most half the cap, so the spread is never crowded out" $
    assertPositions 4 100 [10, 20, 30] `shouldBe` [0, 10, 20, 99]
  it "does not repeat a position that a witness and the spread share" $
    assertPositions 4 10 [0] `shouldBe` [0, 4, 9]
  it "gives at most cap increasing, in-range positions, with the first included and, when the witnesses leave room, the last" $
    sequence_
      [ do length ps `shouldSatisfy` (<= fromInteger cap)
           and (zipWith (<) ps (drop 1 ps)) `shouldBe` True
           take 1 ps `shouldBe` [0]
           -- the last region is spaced in unless witnesses leave room for one position alone
           (drop (length ps - 1) ps == [n - 1] || room < 2) `shouldBe` True
           all (\p -> 0 <= p && p < n) ps `shouldBe` True
           all (`elem` ps) (take (fromInteger cap `div` 2) ws) `shouldBe` True
      | cap <- [2, 3, 7, 200]
      , n <- [cap + 1, cap + 2, 2 * cap, 1000, 10 ^ (30 :: Int)]
      , ws <- [[], [n `div` 2], [n - 1, 3, n `div` 3]]
      , let ps   = assertPositions cap n ws
            room = cap - min (cap `div` 2) (toInteger (length (nub ws))) ]
  it "gives exactly cap positions when there is no witness" $
    sequence_
      [ length (assertPositions cap n []) `shouldBe` fromInteger cap
      | cap <- [2, 3, 7, 200], n <- [cap + 1, 1000, 10 ^ (30 :: Int)] ]

text :: Spec
text = describe "the assertions for each kind of result" $ do
  let emit name = toL4 withAsserts . parse name
  it "writes nothing without the option, and the option only appends" $ do
    let plain = toL4 defaultL4Opts (parse "FirstGap" gapTable)
    plain `shouldNotContain` "emitAsserts"
    plain `shouldNotContain` "#ASSERT"
    emit "FirstGap" gapTable `shouldSatisfy` (plain `isPrefixOf`)
  it "asserts a MAYBE result with JUST and NOTHING, and parenthesises a negative input" $
    -- 0 and 10 are named by the cells and live for no rule, so [0..10] is one block:
    -- 0 is its representative and 10, where > 10 must not match, its highest named value
    notesAndAsserts (emit "FirstGap" gapTable) `shouldBe`
      [ "-- emitAsserts: table \"FirstGap\": 3 regions, 4 asserted (1 at the highest value that a cell names in a block, each marked)."
      , "#ASSERT FirstGap (-1) EQUALS JUST \"negative\""
      , "#ASSERT FirstGap 0 EQUALS NOTHING"
      , "#ASSERT FirstGap 10 EQUALS NOTHING  -- region 1, highest named value"
      , "#ASSERT FirstGap 11 EQUALS JUST \"big\"" ]
  it "asserts a bare result with the value alone, a boolean input as TRUE and FALSE" $
    notesAndAsserts (emit "Totals" totalTable) `shouldBe`
      [ "-- emitAsserts: table \"Totals\": 2 regions, 2 asserted."
      , "#ASSERT Totals TRUE EQUALS 0.25"
      , "#ASSERT Totals FALSE EQUALS 1" ]
  it "asserts a record through its constructor, with every argument atomic, under JUST" $
    notesAndAsserts (emit "Rec" recordTable) `shouldBe`
      [ "-- emitAsserts: table \"Rec\": 4 regions, 4 asserted."
      , "#ASSERT Rec 17 \"Bob\" EQUALS JUST (mkRec (-2.5) \"minor Bob\")"
      , "#ASSERT Rec 17 \"other\" EQUALS NOTHING"
      , "#ASSERT Rec 18 \"Bob\" EQUALS JUST (mkRec 3 \"adult\")"
      , "#ASSERT Rec 18 \"other\" EQUALS JUST (mkRec 3 \"adult\")" ]
  it "writes a sum type's members as constructors on both sides" $
    notesAndAsserts (emit "Pick" enumTable) `shouldBe`
      [ "-- emitAsserts: table \"Pick\": 2 regions, 2 asserted."
      , "#ASSERT Pick `Dining` EQUALS `PRVI`"
      , "#ASSERT Pick `Travel` EQUALS `Amex`" ]
  it "asserts a table with no input column as a function of no arguments" $
    notesAndAsserts (emit "Hi" noInputTable) `shouldBe`
      [ "-- emitAsserts: table \"Hi\": 1 region, 1 asserted."
      , "#ASSERT Hi EQUALS \"hello\"" ]
  it "asserts a block's highest named value as well as its representative, so a closed upper bound is exercised at the bound" $ do
    let out = assertsOf (emit "PrefixComparisons" prefixTable)
    -- the block [18..21]: 18 is its representative (the lowest value a cell names), 21 the highest
    out `shouldSatisfy` (elem "#ASSERT PrefixComparisons 18 \"youth\" EQUALS JUST \"young\"")
    out `shouldSatisfy` (elem "#ASSERT PrefixComparisons 21 \"child\" EQUALS JUST \"adult\"  -- region 3, highest named value")
    out `shouldSatisfy` (elem "#ASSERT PrefixComparisons 21 \"youth\" EQUALS JUST \"young\"  -- region 4, highest named value")
    out `shouldSatisfy` (elem "#ASSERT PrefixComparisons 21 \"other\" EQUALS NOTHING  -- region 5, highest named value")
    -- the second inputs are marked, and without them the assertions are the nine
    -- there were, one per region and in region order
    length [ l | l <- out, "-- region " `isInfixOf` l ] `shouldBe` 3
    [ l | l <- out, not ("-- region " `isInfixOf` l) ] `shouldBe`
      [ "#ASSERT PrefixComparisons " ++ age ++ " \"" ++ scheme ++ "\" EQUALS " ++ ans
      | (age, scheme, ans) <- [ ("17", "child", "JUST \"minor\""), ("17", "youth", "JUST \"young\""), ("17", "other", "NOTHING")
                              , ("18", "child", "JUST \"adult\""), ("18", "youth", "JUST \"young\""), ("18", "other", "NOTHING")
                              , ("22", "child", "JUST \"adult\""), ("22", "youth", "JUST \"adult\""), ("22", "other", "NOTHING") ] ]
    notesOf (emit "PrefixComparisons" prefixTable) `shouldBe`
      [ "-- emitAsserts: table \"PrefixComparisons\": 9 regions, 12 asserted (3 at the highest value that a cell names in a block, each marked)." ]
  it "puts every table's assertions after every table in a file" $ do
    let (_, file) = toL4File withAsserts [parse "FirstGap" gapTable, parse "Hi" noInputTable]
        ls        = lines file
        lastDecl  = last [ i | (i, l) <- zip [0 :: Int ..] ls, "OTHERWISE" `isInfixOf` l ]
        firstNote = head [ i | (i, l) <- zip [0 :: Int ..] ls, "-- emitAsserts:" `isPrefixOf` l ]
    firstNote `shouldSatisfy` (> lastDecl)
    length (assertsOf file) `shouldBe` 5
  where
    notesAndAsserts s = [ l | l <- lines s, "-- emitAsserts:" `isPrefixOf` l || "#ASSERT" `isPrefixOf` l ]

unanalysed :: Spec
unanalysed = describe "what is said instead of asserting" $ do
  let emit name = toL4 withAsserts . parse name
  it "gives a table regionMap cannot analyse one comment with the reason, and no assertions" $ do
    let out = emit "Access" collectionTable
    assertsOf out `shouldBe` []
    case notesOf out of
      [note] -> do
        note `shouldSatisfy` ("-- emitAsserts: no asserts for table \"Access\": column \"roles\"" `isPrefixOf`)
        note `shouldSatisfy` ("collection" `isInfixOf`)
      other -> expectationFailure ("expected one note, got " ++ show other)
  it "does not sample such a table" $
    emit "Access" collectionTable `shouldNotContain` "regions,"
  it "names a region the interpreter refuses, and asserts nothing for it" $ do
    let out = emit "Quotient" quotientTable
    assertsOf out `shouldBe` []
    length (notesOf out) `shouldBe` 3
    out `shouldContain` "region 0 not asserted: the interpreter refuses this input: division by zero"
    out `shouldContain` "1 region, 0 asserted"
  it "says nothing is asserted when the caller supplied the OTHERWISE" $ do
    let out = toL4 withAsserts { defaultResult = "\"none\"" } (parse "FirstGap" gapTable)
    assertsOf out `shouldBe` []
    notesOf out `shouldSatisfy` (any ("the caller supplied the OTHERWISE" `isInfixOf`))
  it "makes a disagreement about null loud: a failing assertion and a DIVERGENCE note" $ do
    -- Unreachable from toL4, whose MAYBE and whose regionMap agree (noRuleMayMatch
    -- asks the same regions). Built by hand to test the branch itself.
    let dt = parse "FirstGap" gapTable
        shape = AssertShape { shapeOuts = [], shapeMulti = False, shapeMk = "", shapeMaybe = False }
        outs  = getOutputHeaders' dt
        ls    = assertLines withAsserts dt shape { shapeOuts = outs }
    ls `shouldSatisfy` (\xs -> "#ASSERT FALSE" `elem` xs)
    ls `shouldSatisfy` (any ("DIVERGENCE at region 1" `isInfixOf`))
  where
    getOutputHeaders' dt = [ h | h <- header dt, label h == DTCH_Out ]

capping :: Spec
capping = describe "assertCap" $ do
  let dt     = parse "Big" bigTable
      emitAt cap = toL4 withAsserts { assertCap = cap } dt
      all'   = assertsOf (emitAt 4000)
      sample = assertsOf (emitAt 5)
      rm     = either (error . show) id (regionMap dt)
  it "asserts every region when they fit" $ do
    regionCount rm `shouldBe` 3375
    length all' `shouldBe` 3375
  it "asserts a cap's worth when they do not, and says how many exist and how many were skipped" $ do
    length sample `shouldBe` 5
    notesOf (emitAt 5) `shouldBe`
      [ "-- emitAsserts: table \"Big\": 3375 regions, 5 asserted (assertCap is 5; the regions asserted are the first region"
        ++ " in which each of the first rules is live, and the rest evenly spaced through positions"
        ++ " 0 .. 3374 of the sequence of regions, first and last included, so the other 3370 regions are not asserted)." ]
  it "chooses from the whole sequence: the first region, the last, and others between" $ do
    take 1 sample `shouldBe` take 1 all'
    drop 4 sample `shouldBe` drop 3374 all'
    all (`elem` all') sample `shouldBe` True
    -- the middle ones differ from the first few regions, which share their first column's first block
    take 5 all' `shouldNotBe` sample
  it "includes the first region of each rule when the cap leaves room, so a sparse table is not sampled where nothing fires" $ do
    let witnesses = [ k | i <- [0 .. 6], Just k <- [firstRegionOf rm i] ]
        big       = assertsOf (emitAt 50)
    length witnesses `shouldBe` 7
    -- row i is the rule whose three cells are all i, and these are its regions
    [ all' !! fromInteger k | k <- witnesses ] `shouldSatisfy` all (`elem` big)
    -- and they are the lines where a rule fires: the other regions are no-match
    length [ () | l <- big, not ("EQUALS NOTHING" `isSuffixOf` l) ] `shouldBe` 7

-- | The second inputs of 'bandsTable' under different caps.
secondInputs :: Spec
secondInputs = describe "the second input of a region (assertCap)" $ do
  let dt         = parse "Bands" bandsTable
      emitAt cap = toL4 withAsserts { assertCap = cap } dt
      marked ls  = [ l | l <- ls, "-- region " `isInfixOf` l ]
  it "asserts all 27 regions and all 19 second inputs when the cap leaves room" $ do
    let out = assertsOf (emitAt 100)
    length out `shouldBe` 46
    length (marked out) `shouldBe` 19
    notesOf (emitAt 100) `shouldBe`
      [ "-- emitAsserts: table \"Bands\": 27 regions, 46 asserted (19 at the highest value that a cell names in a block, each marked)." ]
  it "puts a second input right after its region's first, with every column at the top of its block" $ do
    -- region 13 is the middle block of all three columns: (18, 18, 18), then (21, 21, 21)
    let out = assertsOf (emitAt 100)
        ix  = length (takeWhile (not . ("Bands 21 21 21" `isInfixOf`)) out)
    take 2 (drop (ix - 1) out) `shouldBe`
      [ "#ASSERT Bands 18 18 18 EQUALS JUST \"r2\""
      , "#ASSERT Bands 21 21 21 EQUALS JUST \"r2\"  -- region 13, highest named value" ]
  it "never makes more assertions than the cap, left-out second inputs counted and said" $ do
    sequence_
      [ length (assertsOf (emitAt cap)) `shouldSatisfy` (<= cap)
      | cap <- [1, 2, 5, 20, 26, 27, 28, 30, 45, 46, 47, 100] ]
    let out30 = emitAt 30
    length (assertsOf out30) `shouldBe` 30
    length (marked (assertsOf out30)) `shouldBe` 3
    notesOf out30 `shouldBe`
      [ "-- emitAsserts: table \"Bands\": 27 regions, 30 asserted (3 at the highest value that a cell names in a block, each marked)."
      , "-- emitAsserts: table \"Bands\": 16 inputs at the highest value that a cell names could not be asserted for want of room under assertCap 30." ]
  it "keeps a quarter of the cap for them when the regions do not fit, and counts everything under the cap" $ do
    let out = emitAt 20
    length (assertsOf out) `shouldSatisfy` (<= 20)
    length (marked (assertsOf out)) `shouldSatisfy` (> 0)
    head (notesOf out) `shouldSatisfy` ("27 regions, " `isInfixOf`)
    -- 15 of the 20 for first inputs, of which 13 distinct positions, and 7 second inputs fill the rest
    head (notesOf out) `shouldSatisfy` ("so the other 14 regions are not asserted" `isInfixOf`)
    head (notesOf out) `shouldSatisfy` ("20 asserted (7 at the highest value" `isInfixOf`)
  it "leaves a table without a higher named point exactly as it was" $
    notesOf (toL4 withAsserts (parse "Rec" recordTable)) `shouldBe`
      [ "-- emitAsserts: table \"Rec\": 4 regions, 4 asserted." ]

-- * The cross-check against the real l4

-- | Fixtures that typecheck without assertions and fail @l4 check@ with them,
-- and why. A fixture here that stops failing fails the test: delete the line.
knownCheckFailures :: [(String, String)]
knownCheckFailures =
  [ ( "symptom/struct-tablename-collision"
    , "two tables share a name, so two functions do; l4 accepts the definitions and finds every call ambiguous (symptom/struct-tablename-collision)" ) ]

-- | Fixtures where @l4 run@ reports an assertion false, and why. A false
-- assertion is a real disagreement between the emitter and the interpreter, so a
-- fixture is here only with its reason, and one that stops disagreeing fails the
-- test: delete the line.
knownDivergences :: [(String, String)]
knownDivergences =
  [ ( "symptom/l4-asserts-quotient-rounded-vs-exact"
    , "dmnmd rounds a quotient that does not terminate to 34 digits and l4 divides exactly (symptom/l4-asserts-quotient-rounded-vs-exact)" ) ]

-- | What one fixture came to.
data Verdict
  = NotChecked String              -- ^ not cross-checkable, and why
  | Checked Int                    -- ^ this many assertions, all true
  | KnownFailure String            -- ^ failed @l4 check@, as 'knownCheckFailures' says
  | KnownDivergence String         -- ^ an assertion false, as 'knownDivergences' says
  | Problem String                 -- ^ a real finding

outDir :: FilePath
outDir = "test/golden/.out/asserts"

crossCheck :: Spec
crossCheck = describe "against the real l4" $ do
  fixtures <- runIO roundtripFixtures
  tables   <- runIO $ quietly $ forM fixtures $ \(slug, path) -> do
    (ds, ts) <- readFixture path >>= \r -> pure (force' r)
    pure (slug, [ d | d <- ds, diagSeverity d == Error ], ts)
  it ("emits assertions for the " ++ show (length fixtures) ++ " markdown fixtures, and l4 run satisfies every one of those that --to=l4 accepts") $ withL4 $ \l4bin -> do
    createDirectoryIfMissing True outDir
    verdicts <- forM tables $ \(slug, errs, ts) -> (,) slug <$> verdictFor l4bin slug errs ts
    let problems = [ slug ++ ": " ++ why | (slug, Problem why) <- verdicts ]
        checked  = [ n | (_, Checked n) <- verdicts ]
        known    = [ slug | (slug, KnownFailure _) <- verdicts ]
        diverge  = [ slug | (slug, KnownDivergence _) <- verdicts ]
        present  = [ slug | (slug, _, _) <- tables ]
        stale    = [ slug | (slug, _) <- knownCheckFailures, slug `elem` present, slug `notElem` known ]
                ++ [ slug | (slug, _) <- knownDivergences, slug `elem` present, slug `notElem` diverge ]
    -- What was covered, for a reader of the test directory: the counts the
    -- assertions below only bound.
    writeFile (outDir </> "summary.txt") $ unlines $
      [ show (length fixtures) ++ " fixtures read"
      , show (length checked) ++ " cross-checked, " ++ show (sum checked) ++ " assertions, all true"
      , show (length known) ++ " known check failures, " ++ show (length diverge) ++ " known divergences"
      , show (length [ () | (_, NotChecked _) <- verdicts ]) ++ " not cross-checked, by reason:" ]
      ++ [ "  " ++ show n ++ "  " ++ why
         | (why, n) <- tally [ why | (_, NotChecked why) <- verdicts ] ]
    problems `shouldBe` []
    stale `shouldBe` []
    length checked `shouldSatisfy` (> 100)
    sum checked `shouldSatisfy` (> 500)
  -- The tables whose assertion text is pinned above, through 'toL4' on its own
  -- (the sum types are declared in the table's own text, not a file preamble),
  -- so that the pinned text is also shown TRUE and not only shown stable.
  it "satisfies the assertions pinned above, for each table on its own" $ withL4 $ \l4bin -> do
    createDirectoryIfMissing True outDir
    results <- forM [ ("FirstGap", gapTable), ("Totals", totalTable), ("Rec", recordTable)
                    , ("Pick", enumTable), ("Hi", noInputTable), ("Big", bigTable) ] $ \(name, tbl) -> do
      let src = toL4 withAsserts { assertCap = 50 } (parse name tbl)
      r <- runAsserts l4bin (outDir </> ("pinned-" ++ name ++ ".l4")) src
      pure (name, length (assertsOf src), r)
    [ (name, n, r) | (name, n, r) <- results, r /= Right (n, n, 0, 0) ] `shouldBe` []
  -- A mutation test: the emitter is imagined to write @<= 21@ as @< 21@, which
  -- changes the function at Age = 21 and nowhere else. Only an assertion at
  -- Age = 21 can see it, and before the highest named value of a block was
  -- asserted none did (the block [18..21] was visited at 18 alone).
  it "catches a closed upper bound written as an open one (mutation)" $ withL4 $ \l4bin -> do
    createDirectoryIfMissing True outDir
    let good = toL4 withAsserts (parse "PrefixComparisons" prefixTable)
        bad  = replace "<= 21" "<  21" good
    bad `shouldNotBe` good
    unmutated <- runAsserts l4bin (outDir </> "prefix-good.l4") good
    mutated   <- runAsserts l4bin (outDir </> "prefix-bad.l4") bad
    fmap (\(_, _, f, _) -> f) unmutated `shouldBe` Right 0
    mutated `shouldSatisfy` either (const False) (\(_, _, f, _) -> f > 0)
  it "reports an assertion the function does not satisfy (canary)" $ withL4 $ \l4bin -> do
    createDirectoryIfMissing True outDir
    let good = toL4 withAsserts (parse "FirstGap" gapTable)
        bad  = replace "x > 10" "x > 11" good
    bad `shouldNotBe` good
    ran <- runAsserts l4bin (outDir </> "canary.l4") bad
    ran `shouldSatisfy` either (const False) (\(_, _, f, _) -> f > 0)
  where
    tally ws = [ (w, length (filter (== w) ws)) | w <- foldr (\x acc -> if x `elem` acc then acc else x : acc) [] ws ]
    replace needle new hay = case [ i | (i, t) <- zip [0 ..] (tails hay), needle `isPrefixOf` t ] of
      (i : _) -> take i hay ++ new ++ drop (i + length needle) hay
      []      -> hay

-- | Cross-check one fixture.
verdictFor :: FilePath -> String -> [Diagnostic] -> [DecisionTable] -> IO Verdict
verdictFor l4bin slug errs ts
  | not (null errs)                        = pure (NotChecked "the reader refuses it")
  | null ts                                = pure (NotChecked "it has no table")
  | any (isJust . l4Refusal) ts            = pure (NotChecked "the L4 backend refuses a table")
  | otherwise = do
      let file = outDir </> map (\c -> if c == '/' then '_' else c) slug ++ ".l4"
          plain = snd (toL4File defaultL4Opts ts)
          asserted = snd (toL4File withAsserts ts)
          expectedAsserts = length (assertsOf asserted)
      if not (plain `isPrefixOf` asserted)
        then pure (Problem "emitAsserts changed the text before the assertions")
        else do
          r <- runAsserts l4bin file asserted
          case r of
            Right (n, t, f, other)
              | n /= expectedAsserts -> pure (Problem ("l4 reported " ++ show n ++ " assertions for " ++ show expectedAsserts ++ " #ASSERT lines"))
              | f > 0 || other > 0 || t /= n ->
                  pure $ case lookup slug knownDivergences of
                    Just reason -> KnownDivergence reason
                    Nothing     -> Problem (show f ++ " of " ++ show n ++ " assertions false, " ++ show other ++ " not evaluable; see " ++ file)
              | otherwise -> pure (Checked n)
            Left why -> do
              -- Did it typecheck before? A fixture whose plain output does not
              -- typecheck is not this feature's business (the symptom cases).
              p <- runAsserts l4bin (file ++ ".plain") plain
              case (p, lookup slug knownCheckFailures) of
                (Left _, _)                 -> pure (NotChecked "its L4 does not typecheck even without assertions")
                (Right _, Just reason)      -> pure (KnownFailure reason)
                (Right _, Nothing)          -> pure (Problem why)

-- | Write a file and run @l4 check@ and @l4 run --json@ over it.
--
-- 'Left' is a file that does not typecheck. 'Right' is the number of results of
-- kind @assertion@, how many were true, how many false, and how many results
-- were anything else that is an error or an assertion with no value.
--
-- The JSON, not the text, because the text logs each satisfied assertion twice.
runAsserts :: FilePath -> FilePath -> String -> IO (Either String (Int, Int, Int, Int))
runAsserts l4bin file src = do
  writeFile file src
  (_, cOut, cErr) <- readProcessWithExitCode l4bin ["check", file] ""
  if not ("Check succeeded" `isInfixOf` (cOut ++ cErr))
    then pure (Left ("l4 check failed on " ++ file))
    else do
      (_, rOut, rErr) <- readProcessWithExitCode l4bin ["run", "--json", file] ""
      let j = rOut ++ rErr
      pure $ Right
        ( count "\"kind\":\"assertion\"" j
        , count "\"value\":true" j
        , count "\"value\":false" j
        , count "\"kind\":\"error\"" j + count "\"value\":null" j )
  where
    count needle hay = length [ () | t <- tails hay, needle `isPrefixOf` t ]
