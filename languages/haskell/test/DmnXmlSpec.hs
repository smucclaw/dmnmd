{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DmnXmlSpec where

import Control.Monad.IO.Class (MonadIO (liftIO))
import DMN.XML.ParseDMN
import DMN.XML.XmlToDmnmd (convertAll, Diagnostic (..), Severity (..))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.Golden
import Text.RawString.QQ
-- import qualified DMN.Types as DT
import DMN.Types as DT

-- import Text.XML.HXT.Core

xmlSpec :: Spec
xmlSpec = do
  describe "parseXml" $ do
    it "should parse a trivial xml file" $ do
      output <- parseDMN "test/simple.dmn"
      output
        `shouldBe` [ Definitions
                       { defLabel = dmnNamed' "dinnerDecisions" "Dinner Decisions",
                         defsNamespace = Namespace { namespace = "http://camunda.org/schema/1.0/dmn" },
                         -- no <itemDefinition> in this document; the field records
                         -- the NAME of each one so the converter can warn that
                         -- DMN's data model is dropped.
                         defItemDefs = [],
                         defsDescisions = [],
                         defInputData = [],
                         defDrgElems = [],
                         defDMNDI = Just DMNDI
                       }
                   ]
    it "should parse the simulation.dmn xml file from camunda" $ do
      output <- parseDMN "test/simulation.dmn"
      output `shouldBe` simulationDmn
    -- it "generates the right output with the right params" $ do
    --   output <- liftIO $ show <$> parseDMN "test/simple.dmn"
    --   defaultGolden "myFunc" output
    pure ()
  describe "convertIt" $ do
    it "should convert the standard example properly" $
      snd (convertAll simulationDmn) `shouldBe` convertedSimulation
  dmn13Spec

-- * DMN 1.3
--
-- Every other XML fixture in test/ is DMN 1.1 or 1.2, so none of them ever
-- reached the DMN 1.3 reader. These do: `baseline` is a known-good file and
-- each of the others is `baseline` plus exactly one construct that used to make
-- the whole document fail to unpickle. See test/dmn13/README.md.

dmn13 :: FilePath -> FilePath
dmn13 name = "test/dmn13/" ++ name ++ ".dmn"

-- | Parse a DMN 1.3 fixture and convert it, failing the example with the
-- reader's own diagnostic if it could not be read at all.
readDmn13 :: FilePath -> IO ([Diagnostic], [DT.DecisionTable])
readDmn13 name = do
  parsed <- parseDMNEither (dmn13 name)
  case parsed of
    Left err   -> expectationFailure err >> pure ([], [])
    Right defs -> pure (convertAll defs)

-- | Does any diagnostic of the given severity mention this text?
hasDiag :: Severity -> String -> [Diagnostic] -> Bool
hasDiag sev needle =
  any (\d -> diagSeverity d == sev && T.pack needle `T.isInfixOf` T.pack (diagMessage d))

-- | The single decision table every accepting fixture is expected to yield.
shouldBeAgeBand :: String -> [DT.DecisionTable] -> Expectation
shouldBeAgeBand outName tables = case tables of
  [t] -> do
    tableName t `shouldBe` "Band"
    hitpolicy t `shouldBe` HP_First
    map varname (header t) `shouldBe` ["age", outName]
    map vartype (header t) `shouldBe` [Just DMN_Number, Just DMN_String]
    map row_outputs (allrows t)
      `shouldBe` [ [[FNullary (VS "minor")]]
                 , [[FNullary (VS "adult")]]
                 , [[FNullary (VS "senior")]]
                 ]
  _ -> expectationFailure $ "expected exactly one decision table, got " ++ show (length tables)

dmn13Spec :: Spec
dmn13Spec = describe "DMN 1.3" $ do
  describe "accepts" $ do
    it "the known-good baseline" $ do
      (warns, tables) <- readDmn13 "baseline"
      warns `shouldBe` []
      shouldBeAgeBand "Band" tables

    it "<inputData> carrying <description> and <variable> (B1)" $ do
      (warns, tables) <- readDmn13 "inputdata-variable"
      warns `shouldBe` []
      shouldBeAgeBand "Band" tables

    it "<input>/<output> with no label= and <output> with no typeRef= (B2)" $ do
      (warns, tables) <- readDmn13 "output-without-label"
      warns `shouldBe` []
      -- with no label the output column falls back to its name= attribute,
      -- and with no typeRef its type is inferred from the cells
      case tables of
        [t] -> do
          map varname (header t) `shouldBe` ["age", "band"]
          map vartype (header t) `shouldBe` [Just DMN_Number, Just DMN_String]
        _ -> expectationFailure "expected exactly one decision table"

    it "<output> with a <defaultOutputEntry> (B3)" $ do
      (warns, tables) <- readDmn13 "default-output-entry"
      -- dmnmd's DecisionTable has nowhere to put a default output, so the value
      -- IS lost. That must be said out loud, with the value in the message —
      -- the alternative is an OTHERWISE arm that quietly contradicts the file.
      warns `shouldSatisfy` hasDiag Warning "<defaultOutputEntry> \"\\\"unknown\\\"\""
      warns `shouldNotSatisfy` any ((== Error) . diagSeverity)
      shouldBeAgeBand "Band" tables

    it "<outputValues> and <inputValues> (B3)" $ do
      (warns, tables) <- readDmn13 "output-values"
      warns `shouldBe` []
      shouldBeAgeBand "Band" tables
      -- the declared domain is not dropped: it lands in the column's enums,
      -- which is exactly what dmnmd's own subheader rows populate.
      case tables of
        [t] -> map enums (header t)
          `shouldBe` [ Just [FInRange BClosed 0 150 BClosed]
                     , Just [FNullary (VS "minor"), FNullary (VS "adult"), FNullary (VS "senior")]
                     ]
        _ -> expectationFailure "expected exactly one decision table"

    it "a file declaring only the DMN model namespace (B5)" $ do
      (warns, tables) <- readDmn13 "minimal-namespaces"
      warns `shouldBe` []
      shouldBeAgeBand "Band" tables

    it "extra namespace declarations and foreign-namespace attributes (B5)" $ do
      (warns, tables) <- readDmn13 "extra-namespace"
      warns `shouldBe` []
      shouldBeAgeBand "Band" tables

    it "typeRef=\"number\", the FEEL numeric type (B4)" $ do
      (warns, tables) <- readDmn13 "feel-number-type"
      warns `shouldBe` []
      case tables of
        [t] -> do
          map vartype (header t) `shouldBe` [Just DMN_Number, Just DMN_Number]
          map row_outputs (allrows t)
            `shouldBe` [ [[FNullary (VN 100)]], [[FNullary (VN 200)]], [[FNullary (VN 150)]] ]
        _ -> expectationFailure "expected exactly one decision table"

    it "an unrecognised typeRef, refusing the table rather than guessing (B4)" $ do
      -- Deliberately an Error, not a Warning. An earlier pass inferred the column
      -- type from the cells here, on the reasoning that an unknown typeRef might be
      -- a user-defined <itemDefinition> that happens to be a string. Adversarial
      -- review showed that is the SAME defect as the temporal case: inference can
      -- settle on String, and `mkF (Just DMN_String)` stores a guard like "< 18"
      -- verbatim, so every rule becomes an equality test against that literal and
      -- the generated code can never match — silently, at exit 0. We do not know
      -- the domain, so we refuse the table and name the type. Widening support is
      -- a matter of adding the spelling to convertType.
      (diags, tables) <- readDmn13 "unknown-type"
      diags  `shouldSatisfy` hasDiag Error "unknown typeRef"
      tables `shouldBe` []

    it "carries <annotationEntry> text into the row comments" $ do
      (_, tables) <- readDmn13 "annotations"
      case tables of
        [t] -> map row_comments (allrows t)
          `shouldBe` [ [Just "children", Just "under the age of majority"]
                     , [Nothing, Just "working age"]
                     , [Nothing, Just "retired"]
                     ]
        _ -> expectationFailure "expected exactly one decision table"

  describe "refuses the table" $ do
    it "when a column's typeRef is a FEEL temporal type" $ do
      -- degrading `date` to String would make `< date(\"2020-01-01\")` an
      -- equality test against that literal text: a table that can never fire.
      (diags, tables) <- readDmn13 "temporal-type"
      diags `shouldSatisfy` hasDiag Error "temporal type"
      tables `shouldBe` []

    it "when a rule carries more <inputEntry> elements than there are columns" $ do
      (diags, tables) <- readDmn13 "bad-rule-arity"
      diags `shouldSatisfy` hasDiag Error "<inputEntry>"
      diags `shouldSatisfy` hasDiag Error "Rule_2"
      tables `shouldBe` []

    it "when a rule carries no <outputEntry> at all" $ do
      (diags, tables) <- readDmn13 "bad-rule-no-output"
      diags `shouldSatisfy` hasDiag Error "<outputEntry>"
      tables `shouldBe` []

  describe "rejects" $ do
    let shouldReject name expected = do
          parsed <- parseDMNEither (dmn13 name)
          case parsed of
            Right _  -> expectationFailure $ dmn13 name ++ " should not have been accepted"
            Left err -> T.pack err `shouldSatisfy` T.isInfixOf (T.pack expected)

    it "a DMN 1.2 document, naming the version" $
      shouldReject "not-dmn13" "DMN 1.2"
    it "an element the schema does not allow there" $
      shouldReject "bad-unknown-element" "unprocessed XML content"
    it "an attribute in DMN's own vocabulary that the schema does not declare" $
      shouldReject "bad-unknown-attribute" "unprocessed XML attribute"
    it "children that are out of schema order" $
      shouldReject "bad-misordered-child" "unprocessed XML content"

    it "a legal <businessKnowledgeModel>, saying so rather than blaming the version" $ do
      shouldReject "unsupported-drgelement" "<businessKnowledgeModel>"
      -- the old message claimed "this is not DMN 1.3", which was simply untrue:
      -- businessKnowledgeModel is a legal drgElement substitution in the
      -- vendored XSD. We just do not model it.
      parsed <- parseDMNEither (dmn13 "unsupported-drgelement")
      case parsed of
        Right _ -> expectationFailure "should not have been accepted"
        Left e -> T.pack e `shouldNotSatisfy` T.isInfixOf "not DMN 1.3"

  describe "FEEL string quoting" $
    it "does not double-quote a FEEL string literal" $ do
      -- <text>\"minor\"</text> must land in the IR as VS \"minor\", the same as
      -- the markdown cell `minor`, so that both readers agree downstream.
      (_, tables) <- readDmn13 "baseline"
      concatMap (concatMap concat . map row_outputs . allrows) tables
        `shouldBe` [FNullary (VS "minor"), FNullary (VS "adult"), FNullary (VS "senior")]

convertedSimulation :: [DT.DecisionTable]
convertedSimulation =
  [ DTable
      { tableName = "Beverages",
        hitpolicy = HP_Collect Collect_All,
        header =
          [ DTCH
              { label = DTCH_In,
                varname = "desiredDish",
                vartype = Just DMN_String,
                enums = Nothing
              },
            DTCH
              { label = DTCH_In,
                varname = "guestsWithChildren",
                vartype = Just DMN_Boolean,
                enums = Nothing
              },
            DTCH
              { label = DTCH_Out,
                varname = "Beverages",
                vartype = Just DMN_String,
                enums = Nothing
              }
          ],
        allrows =
          [ DTrow
              { row_number = Just 1,
                row_inputs =
                  [ [FNullary (VS "Spareribs")],
                    [FNullary (VB True)]
                  ],
                row_outputs = [[FNullary (VS "Aecht Schlenkerla Rauchbier")]],
                row_comments = [Just "Tough Stuff"]
              },
            DTrow
              { row_number = Just 2,
                row_inputs =
                  [ [FNullary (VS "Stew")],
                    [FNullary (VB True)]
                  ],
                row_outputs = [[FNullary (VS "Guiness")]],
                row_comments = [Nothing]
              },
            DTrow
              { row_number = Just 3,
                row_inputs =
                  [ [FNullary (VS "Roastbeef")],
                    [FNullary (VB True)]
                  ],
                row_outputs = [[FNullary (VS "Bordeaux")]],
                row_comments = [Nothing]
              },
            DTrow
              { row_number = Just 4,
                row_inputs =
                  [ [ FNullary (VS "Steak"),
                      FNullary (VS "Dry Aged Gourmet Steak"),
                      FNullary (VS "Light Salad and a nice Steak")
                    ],
                    [FNullary (VB True)]
                  ],
                row_outputs = [[FNullary (VS "Pinot Noir")]],
                row_comments = [Nothing]
              },
            DTrow
              { row_number = Just 5,
                row_inputs =
                  [ [FAnything],
                    [FNullary (VB True)]
                  ],
                row_outputs = [[FNullary (VS "Apple Juice")]],
                row_comments = [Nothing]
              },
            DTrow
              { row_number = Just 6,
                row_inputs =
                  [ [FAnything],
                    [FNullary (VB False)]
                  ],
                row_outputs = [[FNullary (VS "Water")]],
                row_comments = [Nothing]
              }
          ]
      },
    DTable
      { tableName = "Dish",
        hitpolicy = HP_Unique,
        header =
          [ DTCH
              { label = DTCH_In,
                varname = "season",
                vartype = Just DMN_String,
                enums = Nothing
              },
            DTCH
              { label = DTCH_In,
                varname = "guestCount",
                vartype = Just DMN_Number,
                enums = Nothing
              },
            DTCH
              { label = DTCH_Out,
                varname = "Dish",
                vartype = Just DMN_String,
                enums = Nothing
              }
          ],
        allrows =
          [ DTrow
              { row_number = Just 1,
                -- ============================ KNOWN DEFECT ============================
                -- This is NOT the desired behaviour; it is the current behaviour,
                -- frozen so that a change to it is noticed.
                --
                -- The source cell reads  not("Fall", "Winter", "Spring", "Summer") .
                -- 'DMN.DecisionTable.mkFs' splits every cell on commas before
                -- anything looks at what the cell means, so this single FEEL
                -- negation is shredded into four fragments, two of which have
                -- unbalanced quotes and parentheses. Nothing downstream can
                -- reconstruct the negation.
                --
                -- Fixing that is the cell-language redesign, deliberately out of
                -- scope here. The same defect is recorded, with its full stdout /
                -- stderr / exit-status behaviour, at
                --   test/corpus/cases/symptom/xml-comma-split-negation
                -- and the expectation below is expected to change when that corpus
                -- case changes. See test/corpus/README.md for the symptom/policy
                -- distinction. What is in scope here is that dmnmd now *says so*: the
                -- reader emits a warning naming the table, column and rule and
                -- stating that the text is kept verbatim and comma-split. The
                -- quotes below are therefore intact — a previous attempt stripped
                -- them from whichever fragments happened to begin and end with
                -- one, which produced a mixture that looked half-parsed and was
                -- neither the source text nor a parse of it.
                --
                -- That warning has since been acted on rather than merely heeded.
                -- dmnmd *does* now unwrap S-FEEL string literals, so a cell
                -- reading "Fall" yields the four characters Fall — but
                -- 'DMN.DecisionTable.unquoteCell' does it __all or nothing per
                -- cell__, precisely so that this shredded cell keeps every
                -- fragment verbatim and the mixture above cannot arise. The rule
                -- is pinned from the other side by
                --   test/corpus/cases/policy/md-quoted-literal-all-or-nothing
                -- so if someone simplifies it to work per fragment, that policy
                -- case and this expectation both move together.
                -- ======================================================================
                row_inputs =
                  [ [ FNullary (VS "not(\"Fall\""),
                      FNullary (VS "\"Winter\""),
                      FNullary (VS "\"Spring\""),
                      FNullary (VS "\"Summer\")")
                    ],
                    [FSection Fgte (VN 0.0)]
                  ],
                row_outputs = [[FNullary (VS "Instant Soup")]],
                row_comments = [Just "Default value"]
              },
            DTrow
              { row_number = Just 2,
                row_inputs =
                  [ [FNullary (VS "Fall")],
                    [FSection Flte (VN 8.0)]
                  ],
                row_outputs = [[FNullary (VS "Spareribs")]],
                row_comments = [Nothing]
              },
            DTrow
              { row_number = Just 3,
                row_inputs =
                  [ [FNullary (VS "Winter")],
                    [FSection Flte (VN 8.0)]
                  ],
                row_outputs = [[FNullary (VS "Roastbeef")]],
                row_comments = [Nothing]
              },
            DTrow
              { row_number = Just 4,
                row_inputs =
                  [ [FNullary (VS "Spring")],
                    [FSection Flte (VN 4.0)]
                  ],
                row_outputs = [[FNullary (VS "Dry Aged Gourmet Steak")]],
                row_comments = [Nothing]
              },
            DTrow
              { row_number = Just 5,
                row_inputs =
                  [ [FNullary (VS "Spring")],
                    [FInRange BClosed 5.0 8.0 BClosed]
                  ],
                row_outputs = [[FNullary (VS "Steak")]],
                row_comments = [Just "Save money"]
              },
            DTrow
              { row_number = Just 6,
                row_inputs =
                  [ [ FNullary (VS "Fall"),
                      FNullary (VS "Winter"),
                      FNullary (VS "Spring")
                    ],
                    [FSection Fgt (VN 8.0)]
                  ],
                row_outputs = [[FNullary (VS "Stew")]],
                row_comments = [Just "Less effort"]
              },
            DTrow
              { row_number = Just 7,
                row_inputs =
                  [ [FNullary (VS "Summer")],
                    [FAnything]
                  ],
                row_outputs = [[FNullary (VS "Light Salad and a nice Steak")]],
                row_comments = [Just "Hey, why not?"]
              }
          ]
      }
  ]


simulationDmn :: [Definitions]
simulationDmn =
  [ Definitions
      { defLabel = dmnNamed' "dinnerDecisions" "Dinner Decisions",
        defsNamespace = Namespace {namespace = "http://camunda.org/schema/1.0/dmn"},
        defItemDefs = [],
        defInputData = [],
        defsDescisions =
          [ Decision
              { decLabel = dmnNamed' "beverages" "Beverages",
                decInfoReq =
                  [ InformationRequirement
                      { infrLabel = dmnWithId "InformationRequirement_1xvojck",
                        infrReq = RequiredInput,
                        infoHref = Href "#InputData_0pgvdj9"
                      },
                    InformationRequirement
                      { infrLabel = dmnWithId "InformationRequirement_083jsex",
                        infrReq = RequiredDecision,
                        infoHref = Href "#dish"
                      }
                  ],
                decDTable =
                  Just
                    ( ExprDTable
                        ( DecisionTable
                            { dtLabel = dmnWithId "DecisionTable_07q05jb", dtAnnotations = [],
                              dtHitPolicy = HP_Collect Collect_All,
                              dtInput =
                                [ TableInput
                                    { tinpName = dmnWithId "InputClause_1acmlkd",
                                      tinpLabel = Just ColumnLabel {columnLabel = "Dish"},
                                      tinpValues = Nothing,
                                      tinpExpr =
                                        InputExpression
                                          ( TLiteralExpression
                                              { tleExpr =
                                                  TExpr
                                                    { exprLabel = dmnWithId "LiteralExpression_0bqgrlg",
                                                      exprTypeRef = Just (TypeRef {typeRef = "string"})
                                                    },
                                                tleExpressionLanguage = Nothing,
                                                tleContent = Just (TextElement {innerText = "desiredDish"})
                                              }
                                          )
                                    },
                                  TableInput
                                    { tinpName = dmnWithId "InputClause_0bo3uen",
                                      tinpLabel =
                                        Just ColumnLabel
                                          { columnLabel = "Guests with children"
                                          },
                                      tinpValues = Nothing,
                                      tinpExpr =
                                        InputExpression
                                          ( TLiteralExpression
                                              { tleExpr =
                                                  TExpr
                                                    { exprLabel = dmnWithId "LiteralExpression_0d6l79o",
                                                      exprTypeRef = Just (TypeRef {typeRef = "boolean"})
                                                    },
                                                tleExpressionLanguage = Nothing,
                                                tleContent = Just (TextElement {innerText = "guestsWithChildren"})
                                              }
                                          )
                                    }
                                ],
                              dtOutput =
                                [ TableOutput
                                    { toutName = dmnLabeled "OuputClause_99999" "beverages",
                                      toutLabel = Just ColumnLabel {columnLabel = "Beverages"},
                                      toutTypeRef = Just TypeRef {typeRef = "string"},
                                      toutValues = Nothing,
                                      toutDefault = Nothing
                                    }
                                ],
                              dtRules =
                                [ Rule
                                    { ruleLabel = dmnWithId "row-506282952-7", ruleAnnotations = [],
                                      ruleDescription = Just (Description {description = "Tough Stuff"}),
                                      ruleInputEntry =
                                        [ InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_03g3ci0",
                                              ieText = TextElement {innerText = "\"Spareribs\""}
                                            },
                                          InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_0jb8hau",
                                              ieText = TextElement {innerText = "true"}
                                            }
                                        ],
                                      ruleOutputEntry =
                                        [ OutputEntry
                                            { outputEntryLabel = dmnWithId "LiteralExpression_1kr45vj",
                                              outputEntryText = TextElement {innerText = "\"Aecht Schlenkerla Rauchbier\""}
                                            }
                                        ]
                                    },
                                  Rule
                                    { ruleLabel = dmnWithId "row-506282952-8", ruleAnnotations = [],
                                      ruleDescription = Nothing,
                                      ruleInputEntry =
                                        [ InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_1ckv0bb",
                                              ieText = TextElement {innerText = "\"Stew\""}
                                            },
                                          InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_1joyits",
                                              ieText = TextElement {innerText = "true"}
                                            }
                                        ],
                                      ruleOutputEntry =
                                        [ OutputEntry
                                            { outputEntryLabel = dmnWithId "LiteralExpression_139n7gl",
                                              outputEntryText = TextElement {innerText = "\"Guiness\""}
                                            }
                                        ]
                                    },
                                  Rule
                                    { ruleLabel = dmnWithId "row-506282952-9", ruleAnnotations = [],
                                      ruleDescription = Nothing,
                                      ruleInputEntry =
                                        [ InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_05rspiy",
                                              ieText = TextElement {innerText = "\"Roastbeef\""}
                                            },
                                          InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_15yl6ki",
                                              ieText = TextElement {innerText = "true"}
                                            }
                                        ],
                                      ruleOutputEntry =
                                        [ OutputEntry
                                            { outputEntryLabel = dmnWithId "LiteralExpression_0bzfo47",
                                              outputEntryText = TextElement {innerText = "\"Bordeaux\""}
                                            }
                                        ]
                                    },
                                  Rule
                                    { ruleLabel = dmnWithId "row-506282952-10", ruleAnnotations = [],
                                      ruleDescription = Nothing,
                                      ruleInputEntry =
                                        [ InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_0mk75lc",
                                              ieText = TextElement {innerText = "\"Steak\",\"Dry Aged Gourmet Steak\",\"Light Salad and a nice Steak\""}
                                            },
                                          InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_18uxmko",
                                              ieText = TextElement {innerText = "true"}
                                            }
                                        ],
                                      ruleOutputEntry =
                                        [ OutputEntry
                                            { outputEntryLabel = dmnWithId "LiteralExpression_00nwn3e",
                                              outputEntryText = TextElement {innerText = "\"Pinot Noir\""}
                                            }
                                        ]
                                    },
                                  Rule
                                    { ruleLabel = dmnWithId "row-506282952-11", ruleAnnotations = [],
                                      ruleDescription = Nothing,
                                      ruleInputEntry =
                                        [ InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_11o8pqj",
                                              ieText = TextElement {innerText = ""}
                                            },
                                          InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_1298ow3",
                                              ieText = TextElement {innerText = "true"}
                                            }
                                        ],
                                      ruleOutputEntry =
                                        [ OutputEntry
                                            { outputEntryLabel = dmnWithId "LiteralExpression_0z18erz",
                                              outputEntryText = TextElement {innerText = "\"Apple Juice\""}
                                            }
                                        ]
                                    },
                                  Rule
                                    { ruleLabel = dmnWithId "row-506282952-12", ruleAnnotations = [],
                                      ruleDescription = Nothing,
                                      ruleInputEntry =
                                        [ InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_0shocr0",
                                              ieText = TextElement {innerText = ""}
                                            },
                                          InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_0nblyrk",
                                              ieText = TextElement {innerText = "false"}
                                            }
                                        ],
                                      ruleOutputEntry =
                                        [ OutputEntry
                                            { outputEntryLabel = dmnWithId "LiteralExpression_0s2fq8r",
                                              outputEntryText = TextElement {innerText = "\"Water\""}
                                            }
                                        ]
                                    }
                                ]
                            }
                        )
                    )
              },
            Decision
              { decLabel = dmnNamed' "dish" "Dish",
                decInfoReq =
                  [ InformationRequirement
                      { infrLabel = dmnWithId "InformationRequirement_0xbr982",
                        infrReq = RequiredInput,
                        infoHref = Href "#InputData_0rin549"
                      },
                    InformationRequirement
                      { infrLabel = dmnWithId "InformationRequirement_0s36klr",
                        infrReq = RequiredInput,
                        infoHref = Href "#InputData_1axnom3"
                      }
                  ],
                decDTable =
                  Just
                    ( ExprDTable
                        ( DecisionTable
                            { dtLabel = dmnWithId "DecisionTable_040j91i", dtAnnotations = [],
                              dtHitPolicy = HP_Unique,
                              dtInput =
                                [ TableInput
                                    { tinpName = dmnWithId "InputClause_0bbq1z8",
                                      tinpLabel = Just ColumnLabel {columnLabel = "Season"},
                                      tinpValues = Nothing,
                                      tinpExpr =
                                        InputExpression
                                          ( TLiteralExpression
                                              { tleExpr =
                                                  TExpr
                                                    { exprLabel = dmnWithId "LiteralExpression_1iwaqcz",
                                                      exprTypeRef = Just (TypeRef {typeRef = "string"})
                                                    },
                                                tleExpressionLanguage = Nothing,
                                                tleContent = Just (TextElement {innerText = "season"})
                                              }
                                          )
                                    },
                                  TableInput
                                    { tinpName = dmnWithId "InputClause_0pcbpc9",
                                      tinpLabel = Just ColumnLabel {columnLabel = "How many guests"},
                                      tinpValues = Nothing,
                                      tinpExpr =
                                        InputExpression
                                          ( TLiteralExpression
                                              { tleExpr =
                                                  TExpr
                                                    { exprLabel = dmnWithId "LiteralExpression_1uu3xe6",
                                                      exprTypeRef = Just (TypeRef {typeRef = "integer"})
                                                    },
                                                tleExpressionLanguage = Nothing,
                                                tleContent = Just (TextElement {innerText = "guestCount"})
                                              }
                                          )
                                    }
                                ],
                              dtOutput =
                                [ TableOutput
                                    { toutName = dmnLabeled "OutputClause_0lfar1z" "desiredDish",
                                      toutLabel = Just ColumnLabel {columnLabel = "Dish"},
                                      toutTypeRef = Just TypeRef {typeRef = "string"},
                                      toutValues = Nothing,
                                      toutDefault = Nothing
                                    }
                                ],
                              dtRules =
                                [ Rule
                                    { ruleLabel = dmnWithId "row-884555325-1", ruleAnnotations = [],
                                      ruleDescription = Just (Description {description = "Default value"}),
                                      ruleInputEntry =
                                        [ InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_0cy7usy",
                                              ieText = TextElement {innerText = "not(\"Fall\", \"Winter\", \"Spring\", \"Summer\")"}
                                            },
                                          InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_0ww352f",
                                              ieText = TextElement {innerText = ">= 0"}
                                            }
                                        ],
                                      ruleOutputEntry =
                                        [ OutputEntry
                                            { outputEntryLabel = dmnWithId "LiteralExpression_07xyqqp",
                                              outputEntryText = TextElement {innerText = "\"Instant Soup\""}
                                            }
                                        ]
                                    },
                                  Rule
                                    { ruleLabel = dmnWithId "row-506282952-1", ruleAnnotations = [],
                                      ruleDescription = Nothing,
                                      ruleInputEntry =
                                        [ InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_06z2ju4",
                                              ieText = TextElement {innerText = "\"Fall\""}
                                            },
                                          InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_0ph5qbt",
                                              ieText = TextElement {innerText = "<= 8"}
                                            }
                                        ],
                                      ruleOutputEntry =
                                        [ OutputEntry
                                            { outputEntryLabel = dmnWithId "LiteralExpression_0sntjhd",
                                              outputEntryText = TextElement {innerText = "\"Spareribs\""}
                                            }
                                        ]
                                    },
                                  Rule
                                    { ruleLabel = dmnWithId "row-506282952-2", ruleAnnotations = [],
                                      ruleDescription = Nothing,
                                      ruleInputEntry =
                                        [ InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_0o5o0mm",
                                              ieText = TextElement {innerText = "\"Winter\""}
                                            },
                                          InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_0rtyr8x",
                                              ieText = TextElement {innerText = "<= 8"}
                                            }
                                        ],
                                      ruleOutputEntry =
                                        [ OutputEntry
                                            { outputEntryLabel = dmnWithId "LiteralExpression_15nybba",
                                              outputEntryText = TextElement {innerText = "\"Roastbeef\""}
                                            }
                                        ]
                                    },
                                  Rule
                                    { ruleLabel = dmnWithId "row-506282952-3", ruleAnnotations = [],
                                      ruleDescription = Nothing,
                                      ruleInputEntry =
                                        [ InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_1f00omt",
                                              ieText = TextElement {innerText = "\"Spring\""}
                                            },
                                          InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_03yxt5d",
                                              ieText = TextElement {innerText = "<= 4"}
                                            }
                                        ],
                                      ruleOutputEntry =
                                        [ OutputEntry
                                            { outputEntryLabel = dmnWithId "LiteralExpression_1ki86jo",
                                              outputEntryText = TextElement {innerText = "\"Dry Aged Gourmet Steak\""}
                                            }
                                        ]
                                    },
                                  Rule
                                    { ruleLabel = dmnWithId "row-506282952-4", ruleAnnotations = [],
                                      ruleDescription = Just (Description {description = "Save money"}),
                                      ruleInputEntry =
                                        [ InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_18csep1",
                                              ieText = TextElement {innerText = "\"Spring\""}
                                            },
                                          InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_1lt4o3q",
                                              ieText = TextElement {innerText = "[5..8]"}
                                            }
                                        ],
                                      ruleOutputEntry =
                                        [ OutputEntry
                                            { outputEntryLabel = dmnWithId "LiteralExpression_1h969t1",
                                              outputEntryText = TextElement {innerText = "\"Steak\""}
                                            }
                                        ]
                                    },
                                  Rule
                                    { ruleLabel = dmnWithId "row-506282952-5", ruleAnnotations = [],
                                      ruleDescription = Just (Description {description = "Less effort"}),
                                      ruleInputEntry =
                                        [ InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_0cp9scy",
                                              ieText = TextElement {innerText = "\"Fall\",\"Winter\",\"Spring\""}
                                            },
                                          InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_1to1xxg",
                                              ieText = TextElement {innerText = "> 8"}
                                            }
                                        ],
                                      ruleOutputEntry =
                                        [ OutputEntry
                                            { outputEntryLabel = dmnWithId "LiteralExpression_0fjt4uo",
                                              outputEntryText = TextElement {innerText = "\"Stew\""}
                                            }
                                        ]
                                    },
                                  Rule
                                    { ruleLabel = dmnWithId "row-506282952-6", ruleAnnotations = [],
                                      ruleDescription = Just (Description {description = "Hey, why not?"}),
                                      ruleInputEntry =
                                        [ InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_0s5azk4",
                                              ieText = TextElement {innerText = "\"Summer\""}
                                            },
                                          InputEntry
                                            { ieLabel = dmnWithId "UnaryTests_1nuzyri",
                                              ieText = TextElement {innerText = ""}
                                            }
                                        ],
                                      ruleOutputEntry =
                                        [ OutputEntry
                                            { outputEntryLabel = dmnWithId "LiteralExpression_0nspzk1",
                                              outputEntryText = TextElement {innerText = "\"Light Salad and a nice Steak\""}
                                            }
                                        ]
                                    }
                                ]
                            }
                        )
                    )
              }
          ],
        defDrgElems =
          [ DrgInpData
              (InputData {inpLabel = dmnNamed' "InputData_0rin549" "Season", inpVariable = Nothing}),
            DrgInpData
              ( InputData {inpLabel = dmnNamed' "InputData_1axnom3" "Number of Guests", inpVariable = Nothing}
              ),
            DrgInpData
              ( InputData {inpLabel = dmnNamed' "InputData_0pgvdj9" "Guests with children?", inpVariable = Nothing}
              ),
            DrgKS
              ( KnowledgeSource
                  { knsLabel = dmnNamed' "KnowledgeSource_0b8hnqo" "Men's Cookbook"
                  }
              )
          ],
        defDMNDI = Just DMNDI
      }
  ]







-- spec1 :: Spec
-- spec1 = do
--   describe "parseHelloWorld" $ do
--     it "should parse the phrase 'Hello World!'" $
--       parseHelloWorld `shouldSucceedOn` ("Hello World!" :: Text)

ex1 :: Text
ex1 =
  T.pack $
    dropWhile
      (== '\n')
      [r|
<?xml version="1.0" encoding="UTF-8"?>
<definitions xmlns="https://www.omg.org/spec/DMN/20191111/MODEL/" xmlns:dmndi="https://www.omg.org/spec/DMN/20191111/DMNDI/" xmlns:dc="http://www.omg.org/spec/DMN/20180521/DC/" xmlns:di="http://www.omg.org/spec/DMN/20180521/DI/" xmlns:camunda="http://camunda.org/schema/1.0/dmn" id="dinnerDecisions" name="Dinner Decisions" namespace="http://camunda.org/schema/1.0/dmn">
</definitions>
|]
