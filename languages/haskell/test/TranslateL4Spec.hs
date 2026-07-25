{-# LANGUAGE OverloadedStrings, QuasiQuotes, LambdaCase #-}

{-| Tests for the DMN -> L4 backend ("DMN.Translate.L4").

Two layers:

* __Smoke tests__ parse small inline fixture tables and pin the emitter's
  structural shape (GIVEN/GIVETH, a first-match BRANCH closed by OTHERWISE, the
  multi-output DECLARE record + mk-constructor, OR-of-EQUALS for multi-value
  cells, arithmetic outputs, and the ditto grid).

* The __golden semantic round-trip__ (BUILD-SPEC §7.1, Option A) parses the two
  decision tables out of @test/golden/miles-card-dmn.md@, emits each with
  'milesOpts', writes the concatenation to @test/golden/.out/miles-card.l4@, and
  validates it with the real @l4@ toolchain: @l4 check@ must typecheck it and
  @l4 run@ must satisfy the golden's behavioural @#ASSERT@ block (adapted to the
  emitter's STRING-world output). This is a SEMANTIC equivalence check, not a
  byte-exact diff against the hand-written @miles-card.l4@. -}

module TranslateL4Spec (l4Spec) where

import Control.Exception (evaluate)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (isInfixOf)
import System.Directory (createDirectoryIfMissing, findExecutable)
import System.Environment (lookupEnv)
import System.Process (readProcessWithExitCode)
import Test.Hspec
import Text.RawString.QQ

import DMN.ParseTable (parseTable)
import DMN.ParsingUtils (parseOnly)
import DMN.Types (DecisionTable, DMNType(..))
import DMN.Translate.L4 (toL4, L4Opts(..), defaultL4Opts, showNumL4, type2l4, pascal)

-- | Multi-output First-hit table: exercises the record + constructor, FInRange,
-- FSection comparisons, and arithmetic outputs.
buyTable :: Text
buyTable = T.pack $ dropWhile (== '\n') [r|
| F | age : Number | mayBuy : Boolean (out) | limit : Number (out) |
|---+--------------+------------------------+----------------------|
| 1 | <18          | False                  |                    0 |
| 2 | [18..21]     | True                   |                  750 |
| 3 | [21..25]     | True                   |                 1500 |
| 4 | >25          | True                   |            age * 100 |
|]

-- | Single-output table with multi-value cells and a trailing catch-all row.
dishTable :: Text
dishTable = T.pack $ dropWhile (== '\n') [r|
| F | Season : String      | guestCount : Number  | Dish : String                | # Annotation  |
|---+----------------------+----------------------+------------------------------+---------------|
| 1 | Fall                 | <= 8                 | Spareribs                    |               |
| 2 | Spring, Summer       | [5..8]               | Steak                        | seasonal pick |
| 3 | Fall, Winter, Spring | > 8                  | Stew                         |               |
| 4 | -                    | -                    | Light Salad                  | catch all     |
|]

-- | A small all-simple-atom First-hit table used to pin the ditto grid shape
-- (BUILD-SPEC §3). Two input columns, single scalar output. Row 3 / row 5 drop
-- the @cat@ conjunct (@-@), so the ditto pass must NOT carry a caret across the
-- gap on the following arm.
tierTable :: Text
tierTable = T.pack $ dropWhile (== '\n') [r|
| F | tier : String | cat : String | mpd : Number (out) |
|---+---------------+--------------+--------------------|
| 1 | platinum      | dining       |                 10 |
| 2 | platinum      | travel       |                  8 |
| 3 | platinum      | -            |                  5 |
| 4 | gold          | dining       |                  6 |
| 5 | gold          | -            |                  3 |
|]

-- | First-hit table with NO all-wildcard catch-all row (bug 1/6). An unmatched
-- input must NOT be handed the last data row's output.
noCatchTable :: Text
noCatchTable = T.pack $ dropWhile (== '\n') [r|
| F | x : Number | label : String (out) |
|---+------------+----------------------|
| 1 | > 10       | big                  |
| 2 | < 0        | negative             |
|]

-- | Single-output NUMBER table whose trailing catch-all row has a wildcard @-@
-- OUTPUT cell (bug 2). The OTHERWISE must be a typed default (@0@), not @""@.
outWildTable :: Text
outWildTable = T.pack $ dropWhile (== '\n') [r|
| F | x : Number | n : Number (out) |
|---+------------+------------------|
| 1 | > 5        | 10               |
| 2 | -          | -                |
|]

-- | Input column whose name is an L4 reserved word (bug 3/5/8). It must be
-- backtick-quoted everywhere it appears (GIVEN, header, guard).
keywordInTable :: Text
keywordInTable = T.pack $ dropWhile (== '\n') [r|
| F | OR : String | result : String (out) |
|---+-------------+-----------------------|
| 1 | yes         | matched               |
| 2 | -           | nope                  |
|]

-- | Multi-output table whose OUTPUT (record-field) names are reserved words
-- (bug 8): the DECLARE record fields and the WITH block must be backtick-quoted.
keywordOutTable :: Text
keywordOutTable = T.pack $ dropWhile (== '\n') [r|
| F | THEN : String | x : Number | AND : String (out) | n : Number (out) |
|---+---------------+------------+--------------------+------------------|
| 1 | foo           | 5          | yes                | 3                |
| 2 | -             | -          | no                 | 9                |
|]

-- | A list-valued hit policy (OutputOrder) that must be gated, not silently
-- collapsed to a single-result BRANCH (bug 7).
outputOrderTable :: Text
outputOrderTable = T.pack $ dropWhile (== '\n') [r|
| O | age : Number | perk : String (out) |
|---+--------------+---------------------|
| 1 | >= 18        | vote                |
| 2 | >= 21        | drink               |
|]

-- | RuleOrder counterpart of 'outputOrderTable' (bug 7).
ruleOrderTable :: Text
ruleOrderTable = T.pack $ dropWhile (== '\n') [r|
| R | age : Number | perk : String (out) |
|---+--------------+---------------------|
| 1 | >= 18        | vote                |
| 2 | >= 21        | drink               |
|]

-- | A guard value containing a wide (CJK) glyph (bug 4). The ditto grid must
-- align by DISPLAY width (中 = 2 lexer columns) so the @^@ chain on arm 2 still
-- copies the q/s conjuncts; otherwise arm 2 collapses to @p EQUALS "中"@ and a
-- non-matching input wrongly returns "b" instead of the catch-all "d".
cjkTable :: Text
cjkTable = T.pack $ dropWhile (== '\n') [r|
| F | p : String | q : Number | s : String | r : String (out) |
|---+------------+------------+------------+------------------|
| 1 | 中         | 1          | k          | a                |
| 2 | 中         | 1          | k          | b                |
| 3 | -          | -          | -          | d                |
|]

-- | A Priority ('P') table whose ROW order (LOW first) differs from its OUTPUT
-- priority (the enums subhead ranks HIGH above LOW). A first-match BRANCH must
-- emit the HIGH arm FIRST, matching evalTable's outputOrder semantics, else an
-- input matching both rows wrongly resolves to "LOW".
prioTable :: Text
prioTable = T.pack $ dropWhile (== '\n') [r|
| P | x : Number | result : String (out) |
|---+------------+-----------------------|
|   |            | HIGH, LOW             |
| 1 | > 0        | LOW                   |
| 2 | > 0        | HIGH                  |
|]

parse :: String -> Text -> DecisionTable
parse name = either error id . parseOnly (parseTable name)

-- * Golden semantic round-trip (BUILD-SPEC §7.1, Option A)

-- | Emission options for the golden: ditto on, OR-of-EQUALS (no @elem@), bare
-- @OTHERWISE@ via the @mk<Name>@ constructor (no MAYBE wrapping).
milesOpts :: L4Opts
milesOpts = L4Opts
  { emitDitto     = True
  , useElem       = False
  , wrapMaybe     = False
  , defaultResult = ""
  , emitAsserts   = False
  }

-- | Locate the external @l4@ toolchain that backs the semantic gate. Honours
-- @$L4_BIN@ first (so a non-PATH build can be pointed at explicitly), else looks
-- @l4@ up on @PATH@.
findL4 :: IO (Maybe FilePath)
findL4 = lookupEnv "L4_BIN" >>= \case
  Just p  -> pure (Just p)
  Nothing -> findExecutable "l4"

-- | Run an example that needs the @l4@ toolchain, or mark it pending when the
-- binary is absent. l4 is NOT a build dependency of dmnmd (the emitter is a pure
-- @DecisionTable -> String@ function), and it does not exist on the CI runner, so
-- a missing toolchain must not be reported as an emitter regression. Everything
-- else in this module is pure and always runs.
withL4 :: (FilePath -> Expectation) -> Expectation
withL4 act = findL4 >>= \case
  Just l4bin -> act l4bin
  Nothing    -> pendingWith
    "no l4 toolchain found: set $L4_BIN or put `l4` on PATH to run the semantic gate"

goldenInput :: FilePath
goldenInput = "test/golden/miles-card-dmn.md"

outDir :: FilePath
outDir = "test/golden/.out"

outFile :: FilePath
outFile = outDir ++ "/miles-card.l4"

-- | Extract maximal runs of consecutive markdown pipe-table lines from a file.
-- The miles-card fixture has exactly two such blocks (the two decision tables);
-- surrounding prose is dropped.
pipeBlocks :: Text -> [Text]
pipeBlocks md = map T.unlines (go (T.lines md))
  where
    isPipe l = case T.uncons (T.dropWhile (== ' ') l) of
                 Just ('|', _) -> True
                 _             -> False
    go [] = []
    go ls = case break isPipe ls of
              (_, [])   -> []
              (_, rest) -> let (blk, after) = span isPipe rest
                           in blk : go after

-- | The golden's behavioural assertions, adapted to the emitter's output: the
-- @Categorize@/@CardToUse@ functions return STRING (the DMN @Category@/@Card@
-- columns are typed @String@), the multi-field result is the @mkCardToUse@
-- record whose @Card@ field is read with @'s Card@. Mirrors the @#ASSERT@ block
-- of @test/golden/miles-card.l4@ (enum literals -> string literals, accessor
-- @'s card@ -> @'s Card@).
goldenAsserts :: String
goldenAsserts = unlines
  [ ""
  , "-- behavioural assertions carried from the golden (adapted to STRING-world output)"
  , "#ASSERT (Categorize \"Cold Storage\" 5411 \"inPerson\" \"SGD\") EQUALS \"GroceriesPhysical\""
  , "#ASSERT (Categorize \"foodpanda\" 0 \"online\" \"SGD\") EQUALS \"foodpanda\""
  , "#ASSERT (Categorize \"Anyone\" 4121 \"inPerson\" \"SGD\") EQUALS \"RideHailing\""
  , "#ASSERT (Categorize \"Anyone\" 0 \"inPerson\" \"MYR\") EQUALS \"MYR\""
  , "#ASSERT (CardToUse \"GroceriesPhysical\" \"MobileWallet\" 800 500 1000)'s Card EQUALS \"DBS yuu\""
  , "#ASSERT (CardToUse \"GroceriesPhysical\" \"MobileWallet\" 0 500 1000)'s Card EQUALS \"PAssion Debit\""
  , "#ASSERT (CardToUse \"GroceriesPhysical\" \"MobileWallet\" 0 0 1000)'s Card EQUALS \"Woman's World\""
  , "#ASSERT (CardToUse \"GroceriesPhysical\" \"PhysicalContactless\" 0 0 1000)'s Card EQUALS \"PRVI\""
  , "#ASSERT (CardToUse \"ContactlessTap\" \"MobileWallet\" 0 0 1000)'s Card EQUALS \"UOB Preferred\""
  , "#ASSERT (CardToUse \"ContactlessTap\" \"PhysicalContactless\" 0 0 1000)'s Card EQUALS \"HSBC Revolution\""
  , "#ASSERT (CardToUse \"ContactlessTap\" \"Amaze\" 0 0 1000)'s Card EQUALS \"Citi Rewards\""
  , "#ASSERT (CardToUse \"Streaming\" \"Online\" 0 0 0)'s Card EQUALS \"SC Smart\""
  ]

l4Spec :: Spec
l4Spec = do
  describe "DMN.Translate.L4.type2l4 / showNumL4 / pascal" $ do
    it "maps DMN types to L4 surface types" $ do
      type2l4 (Just DMN_String)            `shouldBe` "STRING"
      type2l4 (Just DMN_Number)            `shouldBe` "NUMBER"
      type2l4 (Just DMN_Boolean)           `shouldBe` "BOOLEAN"
      type2l4 (Just (DMN_List DMN_Number)) `shouldBe` "LIST OF NUMBER"
      type2l4 Nothing                      `shouldBe` "STRING"
    it "renders integral floats without .0 but keeps real decimals" $ do
      showNumL4 9.0  `shouldBe` "9"
      showNumL4 0.0  `shouldBe` "0"
      showNumL4 0.02 `shouldBe` "0.02"
    it "preserves small-magnitude precision instead of truncating to 6 dp (bug 9)" $ do
      -- a fixed showFFloat (Just 6) collapsed these: 0.0000001 -> "0.0" (a nonzero
      -- value rendered as zero) and dropped 0.1234567's last digit.
      showNumL4 0.0000001 `shouldBe` "0.0000001"
      showNumL4 0.1234567 `shouldBe` "0.1234567"
      showNumL4 (-3.5)    `shouldBe` "-3.5"
      showNumL4 0.0000001 `shouldNotBe` "0.0"
    it "PascalCases multi-word table names" $
      pascal "card to use" `shouldBe` "CardToUse"

  describe "DMN.Translate.L4.toL4 — multi-output First-hit table" $ do
    let out = toL4 defaultL4Opts (parse "buy" buyTable)
    it "declares a result record and a mk-constructor" $ do
      out `shouldContain` "DECLARE Buy HAS"
      out `shouldContain` "mkBuy v1 v2 MEANS Buy WITH"
    it "emits a GIVEN/GIVETH header over the record type" $ do
      out `shouldContain` "GIVEN age IS A NUMBER"
      out `shouldContain` "GIVETH A Buy"
    it "emits a first-match BRANCH closed by OTHERWISE" $ do
      out `shouldContain` "BRANCH"
      out `shouldContain` "OTHERWISE mkBuy"
    it "renders FInRange as two AND-ed conjuncts" $
      out `shouldContain` "age >= 18 AND age <= 21"
    it "renders arithmetic outputs via fnf2l4" $
      out `shouldContain` "(age * 100)"

  describe "DMN.Translate.L4.toL4 — single-output table with multi-value cells" $ do
    let out = toL4 defaultL4Opts (parse "dish" dishTable)
    it "uses a scalar GIVETH for one output column" $
      out `shouldContain` "GIVETH A STRING"
    it "expands a multi-value cell to OR-of-EQUALS" $
      out `shouldContain` "(Season EQUALS \"Spring\" OR Season EQUALS \"Summer\")"
    it "lifts the trailing catch-all row into OTHERWISE" $
      out `shouldContain` "OTHERWISE \"Light Salad\""
    it "carries row comments as trailing -- comments" $
      out `shouldContain` "-- seasonal pick"

  describe "DMN.Translate.L4.toL4 — useElem option" $
    it "renders multi-value cells as elem … (LIST …) when useElem is set" $ do
      let out = toL4 defaultL4Opts { useElem = True } (parse "dish" dishTable)
      out `shouldContain` "elem Season (LIST \"Spring\", \"Summer\")"

  describe "DMN.Translate.L4.renderDittoGrid — emitDitto on vs off (BUILD-SPEC §3)" $ do
    let dt  = parse "tier" tierTable
        off = toL4 defaultL4Opts { emitDitto = False } dt
        on  = toL4 defaultL4Opts { emitDitto = True  } dt
    it "off mode spells out every guard at the same column layout (no carets)" $ do
      off `shouldNotContain` "^"
      off `shouldContain` "IF tier EQUALS \"platinum\" AND cat EQUALS \"travel\" THEN 8"
    it "on mode keeps the first arm fully spelled (nothing to copy above)" $
      on `shouldContain` "IF tier EQUALS \"platinum\" AND cat EQUALS \"dining\" THEN 10"
    it "on mode collapses repeated guard tokens to column-aligned ^" $ do
      on `shouldContain` "^"
      -- arm 2: field, op and the platinum value all match the arm above -> ^^^;
      -- only the changed cat value ("travel") is re-typed.
      on `shouldContain` "IF ^    ^      ^          ^   ^   ^      \"travel\" THEN 8"
      -- the spelled-out form of arm 2 must NOT survive (it was dittoed away).
      on `shouldNotContain` "IF tier EQUALS \"platinum\" AND cat EQUALS \"travel\""
    it "on mode re-types a changed value and never dittos across a dropped conjunct" $
      -- arm 4 changes tier to gold (re-typed), but its AND/cat/EQUALS/value are
      -- spelled out because arm 3 dropped the cat conjunct (a Nothing above is
      -- not copyable) — the load-bearing transitive-with-gap case.
      on `shouldContain` "IF ^    ^      \"gold\"     AND cat EQUALS \"dining\" THEN 6"

  describe "DMN.Translate.L4.toL4 — OTHERWISE synthesis (bug 1/6)" $ do
    let out = toL4 defaultL4Opts (parse "FirstNoCatch" noCatchTable)
    it "does NOT fabricate the last data row's output for unmatched inputs" $
      out `shouldNotContain` "OTHERWISE \"negative\""
    it "falls back to a typed default sentinel when there is no catch-all row" $
      out `shouldContain` "OTHERWISE \"\""

  describe "DMN.Translate.L4.toL4 — wildcard output cell typing (bug 2)" $ do
    let out = toL4 defaultL4Opts (parse "OutWild" outWildTable)
    it "renders a wildcard NUMBER output cell as 0, not the ill-typed \"\"" $ do
      out `shouldContain` "OTHERWISE 0"
      out `shouldNotContain` "OTHERWISE \"\""

  describe "DMN.Translate.L4.toL4 — reserved-word identifier quoting (bug 3/5/8)" $ do
    it "backtick-quotes a keyword input column in GIVEN and the guard" $ do
      let out = toL4 defaultL4Opts (parse "KeywordCol" keywordInTable)
      out `shouldContain` "GIVEN `OR` IS A STRING"
      out `shouldContain` "`OR` EQUALS \"yes\""
      out `shouldNotContain` "GIVEN OR IS"
    it "backtick-quotes keyword record fields (DECLARE + WITH) and keyword GIVEN params" $ do
      let out = toL4 defaultL4Opts (parse "Kwtest" keywordOutTable)
      out `shouldContain` "`AND` IS A STRING"
      out `shouldContain` "GIVEN `THEN` IS A STRING"
      out `shouldNotContain` "HAS\n    AND IS"

  describe "DMN.Translate.L4.toL4 — list-valued hit policies are gated (bug 7)" $ do
    it "errors on OutputOrder instead of collapsing to a scalar BRANCH" $
      evaluate (length (toL4 defaultL4Opts (parse "ro" outputOrderTable)))
        `shouldThrow` anyErrorCall
    it "errors on RuleOrder instead of collapsing to a scalar BRANCH" $
      evaluate (length (toL4 defaultL4Opts (parse "ro" ruleOrderTable)))
        `shouldThrow` anyErrorCall

  describe "DMN.Translate.L4.toL4 — Priority orders arms by output priority, not row order (HP_Priority)" $ do
    let out = toL4 defaultL4Opts (parse "prio" prioTable)
        (beforeLow, _) = T.breakOn (T.pack "THEN \"LOW\"") (T.pack out)
    it "emits both matching arms" $ do
      out `shouldContain` "THEN \"HIGH\""
      out `shouldContain` "THEN \"LOW\""
    it "emits the higher-priority HIGH arm before LOW (rows LOW-first; enums rank HIGH above LOW)" $
      -- first-match BRANCH over priority-sorted arms == evalTable's outputOrder:
      -- an input matching both rows must resolve to HIGH, so HIGH must emit first.
      (T.pack "THEN \"HIGH\"" `T.isInfixOf` beforeLow) `shouldBe` True

  describe "DMN.Translate.L4.toL4 — wide-character ditto alignment (bug 4)" $
    it "a CJK guard value keeps the ^ grid aligned so unmatched input returns the catch-all" $ withL4 $ \l4bin -> do
      let emitted = "§ `t`\n\nIMPORT prelude\n\n"
                 ++ toL4 milesOpts (parse "t" cjkTable)
                 ++ "\n#EVAL t \"\x4e2d\" 999 \"zzz\"\n"
          cjkOut = outDir ++ "/cjk.l4"
      createDirectoryIfMissing True outDir
      writeFile cjkOut emitted
      (_cc, cOut, cErr) <- readProcessWithExitCode l4bin ["check", cjkOut] ""
      (cOut ++ cErr) `shouldSatisfy` ("Check succeeded" `isInfixOf`)
      (_rc, rOut, rErr) <- readProcessWithExitCode l4bin ["run", cjkOut] ""
      let runLog = rOut ++ rErr
      runLog `shouldSatisfy` ("\"d\"" `isInfixOf`)       -- catch-all wins for non-matching input
      runLog `shouldSatisfy` (not . ("\"b\"" `isInfixOf`)) -- the miscopied arm must NOT fire

  describe "dmnmd --to=l4 golden (Option A — semantic, not byte-exact)" $
    it "miles-card emitter output typechecks and the golden #ASSERTs all pass" $ withL4 $ \l4bin -> do
      md <- TIO.readFile goldenInput
      let blocks = pipeBlocks md
      length blocks `shouldSatisfy` (>= 2)
      let dtCat  = parse "Categorize" (blocks !! 0)
          dtCard = parse "CardToUse"  (blocks !! 1)
          emitted = toL4 milesOpts dtCat
                 ++ "\n" ++ toL4 milesOpts dtCard
                 ++ goldenAsserts
      createDirectoryIfMissing True outDir
      writeFile outFile emitted

      -- structural gate: must typecheck (catches ditto/column misalignment).
      (_cc, cOut, cErr) <- readProcessWithExitCode l4bin ["check", outFile] ""
      let checkLog = cOut ++ cErr
      checkLog `shouldSatisfy` ("Check succeeded" `isInfixOf`)

      -- behavioural gate: l4 run exits 0 even on a failed assertion, so inspect
      -- stdout for the satisfied/failed markers rather than the exit code.
      (_rc, rOut, rErr) <- readProcessWithExitCode l4bin ["run", outFile] ""
      let runLog = rOut ++ rErr
      runLog `shouldSatisfy` (not . ("assertion failed" `isInfixOf`))
      runLog `shouldSatisfy` ("assertion satisfied" `isInfixOf`)
