{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DmnXmlSpec where

import Control.Monad.IO.Class (MonadIO (liftIO))
import DMN.XML.ParseDMN
import DMN.XML.XmlToDmnmd (convertAll)
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
                       { defLabel = dmnNamed "dinnerDecisions" "Dinner Decisions",
                         defsNamespace = Namespace { namespace = "http://camunda.org/schema/1.0/dmn" },
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
      convertAll simulationDmn `shouldBe` convertedSimulation

convertedSimulation :: [DT.DecisionTable]
convertedSimulation =
  [ DTable
      { tableName = "Unknown", -- TODO: We should have a real value here
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
                  [ [FNullary (VS "\"Spareribs\"")],
                    [FNullary (VS "true")]
                  ],
                row_outputs = [[FNullary (VS "\"Aecht Schlenkerla Rauchbier\"")]],
                row_comments = [Just "Tough Stuff"]
              },
            DTrow
              { row_number = Just 2,
                row_inputs =
                  [ [FNullary (VS "\"Stew\"")],
                    [FNullary (VS "true")]
                  ],
                row_outputs = [[FNullary (VS "\"Guiness\"")]],
                row_comments = [Nothing]
              },
            DTrow
              { row_number = Just 3,
                row_inputs =
                  [ [FNullary (VS "\"Roastbeef\"")],
                    [FNullary (VS "true")]
                  ],
                row_outputs = [[FNullary (VS "\"Bordeaux\"")]],
                row_comments = [Nothing]
              },
            DTrow
              { row_number = Just 4,
                row_inputs =
                  [ [ FNullary (VS "\"Steak\""),
                      FNullary (VS "\"Dry Aged Gourmet Steak\""),
                      FNullary (VS "\"Light Salad and a nice Steak\"")
                    ],
                    [FNullary (VS "true")]
                  ],
                row_outputs = [[FNullary (VS "\"Pinot Noir\"")]],
                row_comments = [Nothing]
              },
            DTrow
              { row_number = Just 5,
                row_inputs =
                  [ [FAnything],
                    [FNullary (VS "true")]
                  ],
                row_outputs = [[FNullary (VS "\"Apple Juice\"")]],
                row_comments = [Nothing]
              },
            DTrow
              { row_number = Just 6,
                row_inputs =
                  [ [FAnything],
                    [FNullary (VS "false")]
                  ],
                row_outputs = [[FNullary (VS "\"Water\"")]],
                row_comments = [Nothing]
              }
          ]
      },
    DTable
      { tableName = "Unknown",
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
                row_inputs =
                  [ [ FNullary (VS "not(\"Fall\""),
                      FNullary (VS "\"Winter\""),
                      FNullary (VS "\"Spring\""),
                      FNullary (VS "\"Summer\")")
                    ],
                    [FNullary (VS ">= 0")]
                  ],
                row_outputs = [[FNullary (VS "\"Instant Soup\"")]],
                row_comments = [Just "Default value"]
              },
            DTrow
              { row_number = Just 2,
                row_inputs =
                  [ [FNullary (VS "\"Fall\"")],
                    [FNullary (VS "<= 8")]
                  ],
                row_outputs = [[FNullary (VS "\"Spareribs\"")]],
                row_comments = [Nothing]
              },
            DTrow
              { row_number = Just 3,
                row_inputs =
                  [ [FNullary (VS "\"Winter\"")],
                    [FNullary (VS "<= 8")]
                  ],
                row_outputs = [[FNullary (VS "\"Roastbeef\"")]],
                row_comments = [Nothing]
              },
            DTrow
              { row_number = Just 4,
                row_inputs =
                  [ [FNullary (VS "\"Spring\"")],
                    [FNullary (VS "<= 4")]
                  ],
                row_outputs = [[FNullary (VS "\"Dry Aged Gourmet Steak\"")]],
                row_comments = [Nothing]
              },
            DTrow
              { row_number = Just 5,
                row_inputs =
                  [ [FNullary (VS "\"Spring\"")],
                    [FNullary (VS "[5..8]")]
                  ],
                row_outputs = [[FNullary (VS "\"Steak\"")]],
                row_comments = [Just "Save money"]
              },
            DTrow
              { row_number = Just 6,
                row_inputs =
                  [ [ FNullary (VS "\"Fall\""),
                      FNullary (VS "\"Winter\""),
                      FNullary (VS "\"Spring\"")
                    ],
                    [FNullary (VS "> 8")]
                  ],
                row_outputs = [[FNullary (VS "\"Stew\"")]],
                row_comments = [Just "Less effort"]
              },
            DTrow
              { row_number = Just 7,
                row_inputs =
                  [ [FNullary (VS "\"Summer\"")],
                    [FAnything]
                  ],
                row_outputs = [[FNullary (VS "\"Light Salad and a nice Steak\"")]],
                row_comments = [Just "Hey, why not?"]
              }
          ]
      }
  ]


simulationDmn :: [Definitions]
simulationDmn =
  [ Definitions
    { defLabel = dmnNamed "dinnerDecisions" "Dinner Decisions"
    , defsNamespace = Namespace { namespace = "http://camunda.org/schema/1.0/dmn" }
    , defsDescisions =
        [ Decision
            { decLabel = dmnNamed' "beverages" "Beverages"
            , decInfoReq =
                [ InformationRequirement
                    { infrLabel = dmnWithId "InformationRequirement_1xvojck"
                    , infrReq = RequiredInput
                    , infoHref = Href "#InputData_0pgvdj9"
                    }
                , InformationRequirement
                    { infrLabel = dmnWithId "InformationRequirement_083jsex"
                    , infrReq = RequiredDecision
                    , infoHref = Href "#dish"
                    }
                ]
            , decDTable = Just
                ( DecisionTable
                    { dtLabel = dmnWithId "DecisionTable_07q05jb"
                    , dtHitPolicy = HP_Collect Collect_All
                    , dtInput =
                        [ TableInput
                            { tinpName = dmnWithId "InputClause_1acmlkd"
                            , tinpLabel = ColumnLabel { columnLabel = "Dish" }
                            , tinpExpr = InputExpression
                                { inputExprLabel = dmnWithId "LiteralExpression_0bqgrlg"
                                , inputExprTypeRef = TypeRef { typeRef = "string" }
                                , inputExprText = TextElement { innerText = "desiredDish" }
                                }
                            }
                        , TableInput
                            { tinpName = dmnWithId "InputClause_0bo3uen"
                            , tinpLabel = ColumnLabel { columnLabel = "Guests with children" }
                            , tinpExpr = InputExpression
                                { inputExprLabel = dmnWithId "LiteralExpression_0d6l79o"
                                , inputExprTypeRef = TypeRef { typeRef = "boolean" }
                                , inputExprText = TextElement { innerText = "guestsWithChildren" }
                                }
                            }
                        ]
                    , dtOutput =
                        [ TableOutput
                            { toutName = dmnNamed "OuputClause_99999" "beverages"
                            , toutLabel = ColumnLabel { columnLabel = "Beverages" }
                            , toutTypeRef = TypeRef { typeRef = "string" }
                            }
                        ]
                    , dtRules =
                        [ Rule
                            { ruleLabel = dmnWithId "row-506282952-7"
                            , ruleDescription = Just (Description { description = "Tough Stuff" })
                            , ruleInputEntry =
                                [ InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_03g3ci0"
                                    , ieText = TextElement { innerText = "\"Spareribs\"" }
                                    }
                                , InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_0jb8hau"
                                    , ieText = TextElement { innerText = "true" }
                                    }
                                ]
                            , ruleOutputEntry =
                                [ OutputEntry
                                    { outputEntryLabel = dmnWithId "LiteralExpression_1kr45vj"
                                    , outputEntryText = TextElement { innerText = "\"Aecht Schlenkerla Rauchbier\"" }
                                    }
                                ]
                            }
                        , Rule
                            { ruleLabel = dmnWithId "row-506282952-8"
                            , ruleDescription = Nothing
                            , ruleInputEntry =
                                [ InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_1ckv0bb"
                                    , ieText = TextElement { innerText = "\"Stew\"" }
                                    }
                                , InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_1joyits"
                                    , ieText = TextElement { innerText = "true" }
                                    }
                                ]
                            , ruleOutputEntry =
                                [ OutputEntry
                                    { outputEntryLabel = dmnWithId "LiteralExpression_139n7gl"
                                    , outputEntryText = TextElement { innerText = "\"Guiness\"" }
                                    }
                                ]
                            }
                        , Rule
                            { ruleLabel = dmnWithId "row-506282952-9"
                            , ruleDescription = Nothing
                            , ruleInputEntry =
                                [ InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_05rspiy"
                                    , ieText = TextElement { innerText = "\"Roastbeef\"" }
                                    }
                                , InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_15yl6ki"
                                    , ieText = TextElement { innerText = "true" }
                                    }
                                ]
                            , ruleOutputEntry =
                                [ OutputEntry
                                    { outputEntryLabel = dmnWithId "LiteralExpression_0bzfo47"
                                    , outputEntryText = TextElement { innerText = "\"Bordeaux\"" }
                                    }
                                ]
                            }
                        , Rule
                            { ruleLabel = dmnWithId "row-506282952-10"
                            , ruleDescription = Nothing
                            , ruleInputEntry =
                                [ InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_0mk75lc"
                                    , ieText = TextElement { innerText = "\"Steak\",\"Dry Aged Gourmet Steak\",\"Light Salad and a nice Steak\"" }
                                    }
                                , InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_18uxmko"
                                    , ieText = TextElement { innerText = "true" }
                                    }
                                ]
                            , ruleOutputEntry =
                                [ OutputEntry
                                    { outputEntryLabel = dmnWithId "LiteralExpression_00nwn3e"
                                    , outputEntryText = TextElement { innerText = "\"Pinot Noir\"" }
                                    }
                                ]
                            }
                        , Rule
                            { ruleLabel = dmnWithId "row-506282952-11"
                            , ruleDescription = Nothing
                            , ruleInputEntry =
                                [ InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_11o8pqj"
                                    , ieText = TextElement { innerText = "" }
                                    }
                                , InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_1298ow3"
                                    , ieText = TextElement { innerText = "true" }
                                    }
                                ]
                            , ruleOutputEntry =
                                [ OutputEntry
                                    { outputEntryLabel = dmnWithId "LiteralExpression_0z18erz"
                                    , outputEntryText = TextElement { innerText = "\"Apple Juice\"" }
                                    }
                                ]
                            }
                        , Rule
                            { ruleLabel = dmnWithId "row-506282952-12"
                            , ruleDescription = Nothing
                            , ruleInputEntry =
                                [ InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_0shocr0"
                                    , ieText = TextElement { innerText = "" }
                                    }
                                , InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_0nblyrk"
                                    , ieText = TextElement { innerText = "false" }
                                    }
                                ]
                            , ruleOutputEntry =
                                [ OutputEntry
                                    { outputEntryLabel = dmnWithId "LiteralExpression_0s2fq8r"
                                    , outputEntryText = TextElement { innerText = "\"Water\"" }
                                    }
                                ]
                            }
                        ]
                    }
                )
            }
        , Decision
            { decLabel = dmnNamed' "dish" "Dish"
            , decInfoReq =
                [ InformationRequirement
                    { infrLabel = dmnWithId "InformationRequirement_0xbr982"
                    , infrReq = RequiredInput
                    , infoHref = Href "#InputData_0rin549"
                    }
                , InformationRequirement
                    { infrLabel = dmnWithId "InformationRequirement_0s36klr"
                    , infrReq = RequiredInput
                    , infoHref = Href "#InputData_1axnom3"
                    }
                ]
            , decDTable = Just
                ( DecisionTable
                    { dtLabel = dmnWithId "DecisionTable_040j91i"
                    , dtHitPolicy = HP_Unique
                    , dtInput =
                        [ TableInput
                            { tinpName = dmnWithId "InputClause_0bbq1z8"
                            , tinpLabel = ColumnLabel { columnLabel = "Season" }
                            , tinpExpr = InputExpression
                                { inputExprLabel = dmnWithId "LiteralExpression_1iwaqcz"
                                , inputExprTypeRef = TypeRef { typeRef = "string" }
                                , inputExprText = TextElement { innerText = "season" }
                                }
                            }
                        , TableInput
                            { tinpName = dmnWithId "InputClause_0pcbpc9"
                            , tinpLabel = ColumnLabel { columnLabel = "How many guests" }
                            , tinpExpr = InputExpression
                                { inputExprLabel = dmnWithId "LiteralExpression_1uu3xe6"
                                , inputExprTypeRef = TypeRef { typeRef = "integer" }
                                , inputExprText = TextElement { innerText = "guestCount" }
                                }
                            }
                        ]
                    , dtOutput =
                        [ TableOutput
                            { toutName = dmnNamed "OutputClause_0lfar1z" "desiredDish"
                            , toutLabel = ColumnLabel { columnLabel = "Dish" }
                            , toutTypeRef = TypeRef { typeRef = "string" }
                            }
                        ]
                    , dtRules =
                        [ Rule
                            { ruleLabel = dmnWithId "row-884555325-1"
                            , ruleDescription = Just (Description { description = "Default value" })
                            , ruleInputEntry =
                                [ InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_0cy7usy"
                                    , ieText = TextElement { innerText = "not(\"Fall\", \"Winter\", \"Spring\", \"Summer\")" }
                                    }
                                , InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_0ww352f"
                                    , ieText = TextElement { innerText = ">= 0" }
                                    }
                                ]
                            , ruleOutputEntry =
                                [ OutputEntry
                                    { outputEntryLabel = dmnWithId "LiteralExpression_07xyqqp"
                                    , outputEntryText = TextElement { innerText = "\"Instant Soup\"" }
                                    }
                                ]
                            }
                        , Rule
                            { ruleLabel = dmnWithId "row-506282952-1"
                            , ruleDescription = Nothing
                            , ruleInputEntry =
                                [ InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_06z2ju4"
                                    , ieText = TextElement { innerText = "\"Fall\"" }
                                    }
                                , InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_0ph5qbt"
                                    , ieText = TextElement { innerText = "<= 8" }
                                    }
                                ]
                            , ruleOutputEntry =
                                [ OutputEntry
                                    { outputEntryLabel = dmnWithId "LiteralExpression_0sntjhd"
                                    , outputEntryText = TextElement { innerText = "\"Spareribs\"" }
                                    }
                                ]
                            }
                        , Rule
                            { ruleLabel = dmnWithId "row-506282952-2"
                            , ruleDescription = Nothing
                            , ruleInputEntry =
                                [ InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_0o5o0mm"
                                    , ieText = TextElement { innerText = "\"Winter\"" }
                                    }
                                , InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_0rtyr8x"
                                    , ieText = TextElement { innerText = "<= 8" }
                                    }
                                ]
                            , ruleOutputEntry =
                                [ OutputEntry
                                    { outputEntryLabel = dmnWithId "LiteralExpression_15nybba"
                                    , outputEntryText = TextElement { innerText = "\"Roastbeef\"" }
                                    }
                                ]
                            }
                        , Rule
                            { ruleLabel = dmnWithId "row-506282952-3"
                            , ruleDescription = Nothing
                            , ruleInputEntry =
                                [ InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_1f00omt"
                                    , ieText = TextElement { innerText = "\"Spring\"" }
                                    }
                                , InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_03yxt5d"
                                    , ieText = TextElement { innerText = "<= 4" }
                                    }
                                ]
                            , ruleOutputEntry =
                                [ OutputEntry
                                    { outputEntryLabel = dmnWithId "LiteralExpression_1ki86jo"
                                    , outputEntryText = TextElement { innerText = "\"Dry Aged Gourmet Steak\"" }
                                    }
                                ]
                            }
                        , Rule
                            { ruleLabel = dmnWithId "row-506282952-4"
                            , ruleDescription = Just (Description { description = "Save money" })
                            , ruleInputEntry =
                                [ InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_18csep1"
                                    , ieText = TextElement { innerText = "\"Spring\"" }
                                    }
                                , InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_1lt4o3q"
                                    , ieText = TextElement { innerText = "[5..8]" }
                                    }
                                ]
                            , ruleOutputEntry =
                                [ OutputEntry
                                    { outputEntryLabel = dmnWithId "LiteralExpression_1h969t1"
                                    , outputEntryText = TextElement { innerText = "\"Steak\"" }
                                    }
                                ]
                            }
                        , Rule
                            { ruleLabel = dmnWithId "row-506282952-5"
                            , ruleDescription = Just (Description { description = "Less effort" })
                            , ruleInputEntry =
                                [ InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_0cp9scy"
                                    , ieText = TextElement { innerText = "\"Fall\",\"Winter\",\"Spring\"" }
                                    }
                                , InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_1to1xxg"
                                    , ieText = TextElement { innerText = "> 8" }
                                    }
                                ]
                            , ruleOutputEntry =
                                [ OutputEntry
                                    { outputEntryLabel = dmnWithId "LiteralExpression_0fjt4uo"
                                    , outputEntryText = TextElement { innerText = "\"Stew\"" }
                                    }
                                ]
                            }
                        , Rule
                            { ruleLabel = dmnWithId "row-506282952-6"
                            , ruleDescription = Just (Description { description = "Hey, why not?" })
                            , ruleInputEntry =
                                [ InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_0s5azk4"
                                    , ieText = TextElement { innerText = "\"Summer\"" }
                                    }
                                , InputEntry
                                    { ieLabel = dmnWithId "UnaryTests_1nuzyri"
                                    , ieText = TextElement { innerText = "" }
                                    }
                                ]
                            , ruleOutputEntry =
                                [ OutputEntry
                                    { outputEntryLabel = dmnWithId "LiteralExpression_0nspzk1"
                                    , outputEntryText = TextElement { innerText = "\"Light Salad and a nice Steak\"" }
                                    }
                                ]
                            }
                        ]
                    }
                )
            }
        ]
    , defInputData =
        [ InputData { inpLabel = dmnNamed' "InputData_0rin549" "Season" }
        , InputData { inpLabel = dmnNamed' "InputData_1axnom3" "Number of Guests" }
        , InputData { inpLabel = dmnNamed' "InputData_0pgvdj9" "Guests with children?" }
        ]
    , defDrgElems =
        [ KnowledgeSource { knsLabel = dmnNamed' "KnowledgeSource_0b8hnqo" "Men's Cookbook" } ]
    , defDMNDI = Just DMNDI
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
