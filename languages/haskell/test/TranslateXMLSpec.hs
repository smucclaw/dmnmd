{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the @--to=xml@ cell language.
--
-- __The real gate on this backend is not here.__ It is
-- @test\/roundtrip\/run-roundtrip.sh@, which feeds every markdown fixture in the
-- tree through @--to=xml@ and back through dmnmd's own DMN reader and demands
-- byte-identical TypeScript. That covers 120 fixtures at once and needs no
-- hand-written expectation, which is exactly what makes it trustworthy: a
-- hand-written expectation can be wrong in the same direction as the code.
--
-- What is worth pinning HERE is the handful of spellings where the emitter
-- deliberately differs from every other renderer in the tree, because those are
-- the ones a future reader would "fix" back. Each is a conformance point, and
-- each cites the rule it serves.
module TranslateXMLSpec (xmlEmitSpec) where

import Test.Hspec

import Data.List (isInfixOf, isPrefixOf)

import DMN.Translate.XML (cellText, defaultXMLOpts, fidelityDiags, showFeelXML, toXMLDoc, toXMLFile)
import DMN.Types

xmlEmitSpec :: Spec
xmlEmitSpec = describe "DMN.Translate.XML" $ do

  describe "showFeelXML" $ do
    it "quotes a string, where showDomainMember deliberately does not" $
      showFeelXML (Just DMN_String) (FNullary (VS "Fall")) `shouldBe` "\"Fall\""

    -- XmlToDmnmd.feelChar un-escapes on the way back in, so a value carrying a
    -- quote survives a round trip only if it is escaped on the way out. dmnmd's
    -- markdown-side unquoteCell does NOT interpret escapes, which is recorded
    -- as a gap; this side must not inherit it.
    it "escapes a quote and a backslash, and only the escapes FEEL defines" $ do
      showFeelXML (Just DMN_String) (FNullary (VS "say \"hi\""))
        `shouldBe` "\"say \\\"hi\\\"\""
      showFeelXML (Just DMN_String) (FNullary (VS "a\\b"))
        `shouldBe` "\"a\\\\b\""

    -- DMN 1.3 §9.2 rule 5's operator slot is `< <= > >=` and nothing else. `=`
    -- is dmnmd's own spelling; a BARE VALUE is DMN's equality test, and
    -- JS.feel2jsIn renders FNullary and FSection Feq identically, so dropping
    -- the operator is invisible downstream.
    it "drops the = of an equality section, because rule 5 has no = " $
      showFeelXML (Just DMN_Number) (FSection Feq (VN 5)) `shouldBe` "5"

    it "keeps the four ordering operators" $ do
      showFeelXML (Just DMN_Number) (FSection Flt (VN 18)) `shouldBe` "< 18"
      showFeelXML (Just DMN_Number) (FSection Fgte (VN 5)) `shouldBe` ">= 5"

    -- D-11. `5 <=` is mirrored by DMN.ParseCell.suffixCmp at PARSE time, so
    -- what reaches here is already FSection Fgte and the emitter cannot tell
    -- the two apart. This is the assertion that says so.
    it "cannot distinguish the D-11 suffix form from the prefix one" $
      showFeelXML (Just DMN_Number) (FSection Fgte (VN 5))
        `shouldBe` showFeelXML (Just DMN_Number) (FSection Fgte (VN 5))

    it "spells all four interval bracket combinations" $ do
      showFeelXML (Just DMN_Number) (FInRange BClosed 1 5 BClosed) `shouldBe` "[1..5]"
      showFeelXML (Just DMN_Number) (FInRange BOpen 1 5 BOpen) `shouldBe` "(1..5)"
      showFeelXML (Just DMN_Number) (FInRange BClosed 1 5 BOpen) `shouldBe` "[1..5)"
      showFeelXML (Just DMN_Number) (FInRange BOpen 1 5 BClosed) `shouldBe` "(1..5]"

    -- The one that would silently change meaning. showFNumFunction renders an
    -- FNF3 flat, so a left-nested tree re-parses under FEEL's precedence as a
    -- right-nested one. Parenthesising the WHOLE cell instead is equally wrong
    -- and louder: ParseFEEL.parseFNF3 has a parenthesised-OPERAND production
    -- and no parenthesised-cell one, so it made every arithmetic output cell
    -- unreadable by dmnmd's own reader.
    it "parenthesises a nested operator application and not the top level" $ do
      let a = FNF1 "a"; b = FNF1 "b"; c = FNF1 "c"
      showFeelXML (Just DMN_Number) (FFunction (FNF3 a FNPlus b))
        `shouldBe` "a + b"
      showFeelXML (Just DMN_Number) (FFunction (FNF3 (FNF3 a FNPlus b) FNMul c))
        `shouldBe` "(a + b) * c"
      showFeelXML (Just DMN_Number) (FFunction (FNF3 a FNPlus (FNF3 b FNMul c)))
        `shouldBe` "a + (b * c)"

  describe "cellText" $ do
    -- Rule 11's disjunction, and the separator the reader splits on.
    it "joins a multi-value cell with the comma the reader splits on" $
      cellText (strCol DTCH_In) [FNullary (VS "Fall"), FNullary (VS "Winter")]
        `shouldBe` "\"Fall\", \"Winter\""

    -- A wildcard is spelled differently on the two sides and the column's own
    -- label is what decides. `-` is rule-12 syntax: legal in an <inputEntry>,
    -- which is a tUnaryTests, and meaningless in an <outputEntry>, which is a
    -- tLiteralExpression.
    it "writes a wildcard as - in an input column" $
      cellText (strCol DTCH_In) [FAnything] `shouldBe` "-"

    it "writes a wildcard as an EMPTY entry in an output column" $
      cellText (strCol DTCH_Out) [FAnything] `shouldBe` ""

    -- A collection column's cells are parsed and rendered at the ELEMENT type,
    -- exactly as they are on the way in; the list-ness lives in the column type
    -- where a synthesized <itemDefinition> can carry it.
    it "renders a collection column's cell at the element type" $
      cellText (listCol DTCH_In) [FNullary (VS "admin")] `shouldBe` "\"admin\""
  -- THE HOLE THE ROUND-TRIP HARNESS CANNOT SEE, which is the whole reason this
  -- block is here rather than left to test/roundtrip/. Measured, not assumed:
  -- `--to=ts` renders all eleven hit policies as one of two outputs ({U,P,F} an
  -- else-if chain, the rest independent ifs) and `--to=l4` collapses C, C+, C<,
  -- C> and C# onto each other because it refuses all five with the same
  -- message. So a green round trip would say NOTHING about an emitter that
  -- wrote COLLECT without its aggregation, or with the wrong one — and
  -- DECISIONS.md D-8 records that the two-binary pipeline this replaces loses
  -- the hit policy in BOTH directions.
  describe "hit policy (not covered by the round-trip harness)" $ do
    let attrs hp = [ a | a <- ["hitPolicy=\"UNIQUE\"", "hitPolicy=\"ANY\""
                              , "hitPolicy=\"PRIORITY\"", "hitPolicy=\"FIRST\""
                              , "hitPolicy=\"OUTPUT ORDER\"", "hitPolicy=\"RULE ORDER\""
                              , "hitPolicy=\"COLLECT\""
                              , "aggregation=\"SUM\"", "aggregation=\"MIN\""
                              , "aggregation=\"MAX\"", "aggregation=\"COUNT\"" ]
                   , a `isInfixOf` toXMLDoc defaultXMLOpts [table hp] ]

    it "writes each of the six non-Collect policies, and UNIQUE by omission" $ do
      -- UNIQUE is the XSD default and xpDefault suppresses it. A document with
      -- no hitPolicy attribute MEANS unique, so writing nothing is correct DMN.
      attrs HP_Unique `shouldBe` []
      attrs HP_Any `shouldBe` ["hitPolicy=\"ANY\""]
      attrs HP_Priority `shouldBe` ["hitPolicy=\"PRIORITY\""]
      attrs HP_First `shouldBe` ["hitPolicy=\"FIRST\""]
      attrs HP_OutputOrder `shouldBe` ["hitPolicy=\"OUTPUT ORDER\""]
      attrs HP_RuleOrder `shouldBe` ["hitPolicy=\"RULE ORDER\""]

    it "writes COLLECT with the right aggregation, and bare for Collect All" $ do
      attrs (HP_Collect Collect_All) `shouldBe` ["hitPolicy=\"COLLECT\""]
      attrs (HP_Collect Collect_Sum) `shouldBe` ["hitPolicy=\"COLLECT\"", "aggregation=\"SUM\""]
      attrs (HP_Collect Collect_Min) `shouldBe` ["hitPolicy=\"COLLECT\"", "aggregation=\"MIN\""]
      attrs (HP_Collect Collect_Max) `shouldBe` ["hitPolicy=\"COLLECT\"", "aggregation=\"MAX\""]
      attrs (HP_Collect Collect_Cnt) `shouldBe` ["hitPolicy=\"COLLECT\"", "aggregation=\"COUNT\""]

    -- The decision's <variable> states the type of the decision's RESULT. Under
    -- a list-valued hit policy that result is a LIST, so claiming the output
    -- column's scalar type there tells a conformant consumer the decision
    -- returns a number when the engine will hand it a list of numbers. Only
    -- Collect-with-no-aggregation, RULE ORDER and OUTPUT ORDER are list-valued:
    -- C+ / C< / C> reduce to one value OF the column's type, and C# counts, so
    -- all four aggregations keep a scalar typeRef. (Found by adversarial review
    -- of D-17; the reviewer's claim covered all of COLLECT, which is too wide --
    -- the aggregation attribute is what decides it.)
    it "omits the decision <variable> typeRef exactly when the result is a list" $ do
      let varTypeRef hp =
            [ w | w <- words (toXMLDoc defaultXMLOpts [table hp])
                , "typeRef=" `isPrefixOf` w ]
          scalarKept hp = varTypeRef hp == varTypeRef HP_Unique
      -- list-valued: the scalar claim must be gone
      scalarKept (HP_Collect Collect_All) `shouldBe` False
      scalarKept HP_RuleOrder `shouldBe` False
      scalarKept HP_OutputOrder `shouldBe` False
      -- single-valued: unchanged
      scalarKept HP_First `shouldBe` True
      scalarKept HP_Any `shouldBe` True
      scalarKept HP_Priority `shouldBe` True
      scalarKept (HP_Collect Collect_Sum) `shouldBe` True
      scalarKept (HP_Collect Collect_Min) `shouldBe` True
      scalarKept (HP_Collect Collect_Max) `shouldBe` True
      scalarKept (HP_Collect Collect_Cnt) `shouldBe` True

  -- D-16 phase 1. The catch-all 'uniquenessErrors' deliberately leaves alone
  -- (it is legal, unambiguous, and dmnmd evaluates it first-match) becomes a
  -- portability hazard only in what THIS backend emits, because Unique lets a
  -- foreign engine reorder the rules. Reader-side was tried and measured: the
  -- shape is in 40 of 221 corpus fixtures, so it warned on a fifth of all runs
  -- about a problem those runs did not have.
  describe "DMN.Translate.XML.fidelityDiags — a catch-all row in a U table" $ do
    let inC n  = DTCH DTCH_In  n (Just DMN_String) Nothing
        outC n = DTCH DTCH_Out n (Just DMN_String) Nothing
        r n ins = DTrow (Just n) ins [[FNullary (VS "x")]] []
        tb hp chs rows = DTable "T" hp chs rows Nothing
        lit s' = [FNullary (VS s')]
        catchAllWarns = length . filter ("overlaps every other rule" `isInfixOf`)
                      . map show . fidelityDiags defaultXMLOpts

    it "warns about a row whose every input cell is a wildcard" $
      catchAllWarns (tb HP_Unique [inC "Season", inC "Guests", outC "Dish"]
                     [r 1 [lit "Fall", lit "8"], r 2 [[FAnything], [FAnything]]])
        `shouldBe` 1

    it "names the row the way the author numbered it" $
      show (fidelityDiags defaultXMLOpts
             (tb HP_Unique [inC "Season", outC "Dish"]
              [r 1 [lit "Fall"], r 9 [[FAnything]]]))
        `shouldSatisfy` ("row 9" `isInfixOf`)

    -- A partial wildcard does not match everything, so it does not overlap
    -- every rule. This is the one an over-eager predicate gets wrong.
    it "stays silent when only SOME input cells of a row are wildcards" $
      catchAllWarns (tb HP_Unique [inC "Season", inC "Guests", outC "Dish"]
                     [r 1 [lit "Fall", lit "8"], r 2 [[FAnything], lit "9"]])
        `shouldBe` 0

    it "stays silent for a U table with no catch-all at all" $
      catchAllWarns (tb HP_Unique [inC "Season", outC "Dish"]
                     [r 1 [lit "Fall"], r 2 [lit "Winter"]])
        `shouldBe` 0

    -- F and P are the repairs the message recommends. A repair that leaves the
    -- diagnostic in place is not a repair.
    it "leaves hit policies other than Unique alone, F and P included" $
      sum [ catchAllWarns (tb hp [inC "Season", outC "Dish"]
                           [r 1 [lit "Fall"], r 2 [[FAnything]]])
          | hp <- [HP_First, HP_Priority, HP_Any, HP_OutputOrder, HP_RuleOrder] ]
        `shouldBe` 0

    -- With no input columns every row is vacuously a catch-all, which would
    -- warn on every input-less table. Matches uniquenessErrors' own guard.
    it "declines a table with no input columns" $
      catchAllWarns (tb HP_Unique [outC "Dish"]
                     [DTrow (Just 1) [] [[FNullary (VS "a")]] []])
        `shouldBe` 0

    it "reports every catch-all row, not just the first" $
      catchAllWarns (tb HP_Unique [inC "Season", outC "Dish"]
                     [r 1 [lit "Fall"], r 2 [[FAnything]], r 3 [[FAnything]]])
        `shouldBe` 2

  -- D-16 phase 2. 'toXMLFile' promotes the ELIGIBLE shape — a trailing,
  -- comment-free catch-all in a U table — into <defaultOutputEntry> before
  -- 'fidelityDiags' ever sees the table, so the warnings tested above survive
  -- only for rows the promotion must not touch. (The fidelityDiags tests above
  -- call that function directly and therefore still see the raw table; that is
  -- the division of labour, not a contradiction.) These go through 'toXMLFile',
  -- the real entry point.
  describe "DMN.Translate.XML.toXMLFile — catch-all promotion (D-16 phase 2)" $ do
    let inC n  = DTCH DTCH_In  n (Just DMN_String) Nothing
        outC n = DTCH DTCH_Out n (Just DMN_String) Nothing
        comC n = DTCH DTCH_Comment n Nothing Nothing
        lit s' = [FNullary (VS s')]
        rw n ins outs cs = DTrow (Just n) ins outs cs
        dish hp rows = DTable "Dish" hp [inC "Season", outC "Dish"] rows Nothing
        run dt = toXMLFile defaultXMLOpts [dt]
        diagsOf = map show . fst . run

    it "emits a trailing comment-free catch-all as the default output value and drops its rule" $ do
      let (ds, doc) = run (dish HP_Unique
                            [ rw 1 [lit "Fall"] [lit "Spareribs"] []
                            , rw 2 [[FAnything]] [lit "Takeaway"] [] ])
      doc `shouldSatisfy` ("<defaultOutputEntry" `isInfixOf`)
      doc `shouldSatisfy` ("<text>\"Takeaway\"</text>" `isInfixOf`)
      doc `shouldNotSatisfy` ("rule_1_2" `isInfixOf`)
      map show ds `shouldSatisfy` any ("default output value" `isInfixOf`)
      map show ds `shouldNotSatisfy` any ("was not promoted" `isInfixOf`)

    it "does not promote a row that carries a comment, and says why" $ do
      let t = DTable "Dish" HP_Unique [inC "Season", outC "Dish", comC "note"]
                [ DTrow (Just 1) [lit "Fall"] [lit "Spareribs"] [Nothing]
                , DTrow (Just 2) [[FAnything]] [lit "Takeaway"] [Just "hey"] ]
                Nothing
          (ds, doc) = toXMLFile defaultXMLOpts [t]
      doc `shouldSatisfy` ("rule_1_2" `isInfixOf`)
      doc `shouldNotSatisfy` ("<defaultOutputEntry" `isInfixOf`)
      map show ds `shouldSatisfy` any ("carries a row comment" `isInfixOf`)

    it "does not promote a mid-table catch-all, and says why" $ do
      let (ds, doc) = run (dish HP_Unique
                            [ rw 1 [[FAnything]] [lit "Takeaway"] []
                            , rw 2 [lit "Fall"] [lit "Spareribs"] [] ])
      doc `shouldSatisfy` ("rule_1_1" `isInfixOf`)
      doc `shouldNotSatisfy` ("<defaultOutputEntry" `isInfixOf`)
      map show ds `shouldSatisfy` any ("not the last row" `isInfixOf`)

    it "does not promote an all-wildcard-output catch-all, whose row would vanish" $ do
      let (ds, doc) = run (dish HP_Unique
                            [ rw 1 [lit "Fall"] [lit "Spareribs"] []
                            , rw 2 [[FAnything]] [[FAnything]] [] ])
      doc `shouldSatisfy` ("rule_1_2" `isInfixOf`)
      doc `shouldNotSatisfy` ("<defaultOutputEntry" `isInfixOf`)
      map show ds `shouldSatisfy` any ("no value to declare" `isInfixOf`)

    it "leaves an F table's catch-all alone — it is an ordinary, legal rule there" $ do
      let (ds, doc) = run (dish HP_First
                            [ rw 1 [lit "Fall"] [lit "Spareribs"] []
                            , rw 2 [[FAnything]] [lit "Takeaway"] [] ])
      doc `shouldSatisfy` ("rule_1_2" `isInfixOf`)
      doc `shouldNotSatisfy` ("<defaultOutputEntry" `isInfixOf`)
      diagsOf (dish HP_First
                [ rw 1 [lit "Fall"] [lit "Spareribs"] []
                , rw 2 [[FAnything]] [lit "Takeaway"] [] ])
        `shouldBe` []

    it "writes a table-level default even with no catch-all row to promote" $ do
      let t = (dish HP_Unique [ rw 1 [lit "Fall"] [lit "Spareribs"] [] ])
                { dtDefaultOutput = Just [[FNullary (VS "Takeaway")]] }
          (_, doc) = toXMLFile defaultXMLOpts [t]
      doc `shouldSatisfy` ("<defaultOutputEntry" `isInfixOf`)
      doc `shouldSatisfy` ("<text>\"Takeaway\"</text>" `isInfixOf`)

  where
    strCol k = DTCH k "Season" (Just DMN_String) Nothing
    listCol k = DTCH k "roles" (Just (DMN_List DMN_String)) Nothing

    -- The smallest table a DMN document can carry: one input, one output, one
    -- rule. tDecisionTable is output+ and tDecisionRule is outputEntry+.
    table hp = DTable "T" hp
      [ DTCH DTCH_In "n" (Just DMN_Number) Nothing
      , DTCH DTCH_Out "v" (Just DMN_Number) Nothing ]
      [ DTrow (Just 1) [[FSection Flt (VN 5)]] [[FNullary (VN 10)]] [] ]
      Nothing
