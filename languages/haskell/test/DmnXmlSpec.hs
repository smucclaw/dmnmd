{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DmnXmlSpec where

import Control.Monad.IO.Class (MonadIO (liftIO))
import DMN.XML.ParseDMN
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.Golden
import Text.RawString.QQ
-- import qualified DMN.Types as DT
import DMN.Types (CollectOperator(..), HitPolicy(..))

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
                         descisionsDiagrams = [],
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


simulationDmn :: [Definitions]
simulationDmn =
  [ Definitions
    { defLabel = dmnNamed "dinnerDecisions" "Dinner Decisions"
    , defsNamespace = Namespace { namespace = "http://camunda.org/schema/1.0/dmn" }
    , descisionsDiagrams =
        [ Decision
            { decLabel = dmnNamed "beverages" "Beverages"
            , decInfoReq =
                [ InformationRequirement
                    { infrLabel = DmnCommon ( Just "InformationRequirement_1xvojck" ) Nothing
                    , infrReq = RequiredInput
                    , infoHref = Href "#InputData_0pgvdj9"
                    }
                , InformationRequirement
                    { infrLabel = DmnCommon ( Just "InformationRequirement_083jsex" ) Nothing
                    , infrReq = RequiredDecision
                    , infoHref = Href "#dish"
                    }
                ]
            , decDTable = Just
                ( DecisionTable
                    { dtLabel = DmnCommon ( Just "DecisionTable_07q05jb" ) Nothing
                    , dtHitPolicy = HP_Collect Collect_All
                    , dtInput =
                        [ TableInput
                            { tinpName = DmnCommon ( Just "InputClause_1acmlkd" ) Nothing
                            , tinpLabel = ColumnLabel { columnLabel = "Dish" }
                            , tinpExpr = InputExpression
                                { inputExprLabel = DmnCommon ( Just "LiteralExpression_0bqgrlg" ) Nothing
                                , inputExprTypeRef = TypeRef { typeRef = "string" }
                                , inputExprText = TextElement { innerText = "desiredDish" }
                                }
                            }
                        , TableInput
                            { tinpName = DmnCommon ( Just "InputClause_0bo3uen" ) Nothing
                            , tinpLabel = ColumnLabel { columnLabel = "Guests with children" }
                            , tinpExpr = InputExpression
                                { inputExprLabel = DmnCommon ( Just "LiteralExpression_0d6l79o" ) Nothing
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
                            { ruleLabel = DmnCommon ( Just "row-506282952-7" ) Nothing }
                        , Rule
                            { ruleLabel = DmnCommon ( Just "row-506282952-8" ) Nothing }
                        , Rule
                            { ruleLabel = DmnCommon ( Just "row-506282952-9" ) Nothing }
                        , Rule
                            { ruleLabel = DmnCommon ( Just "row-506282952-10" ) Nothing }
                        , Rule
                            { ruleLabel = DmnCommon ( Just "row-506282952-11" ) Nothing }
                        , Rule
                            { ruleLabel = DmnCommon ( Just "row-506282952-12" ) Nothing }
                        ]
                    }
                )
            }
        , Decision
            { decLabel = dmnNamed "dish" "Dish"
            , decInfoReq =
                [ InformationRequirement
                    { infrLabel = DmnCommon ( Just "InformationRequirement_0xbr982" ) Nothing
                    , infrReq = RequiredInput
                    , infoHref = Href "#InputData_0rin549"
                    }
                , InformationRequirement
                    { infrLabel = DmnCommon ( Just "InformationRequirement_0s36klr" ) Nothing
                    , infrReq = RequiredInput
                    , infoHref = Href "#InputData_1axnom3"
                    }
                ]
            , decDTable = Just
                ( DecisionTable
                    { dtLabel = DmnCommon ( Just "DecisionTable_040j91i" ) Nothing
                    , dtHitPolicy = HP_Unique
                    , dtInput =
                        [ TableInput
                            { tinpName = DmnCommon ( Just "InputClause_0bbq1z8" ) Nothing
                            , tinpLabel = ColumnLabel { columnLabel = "Season" }
                            , tinpExpr = InputExpression
                                { inputExprLabel = DmnCommon ( Just "LiteralExpression_1iwaqcz" ) Nothing
                                , inputExprTypeRef = TypeRef { typeRef = "string" }
                                , inputExprText = TextElement { innerText = "season" }
                                }
                            }
                        , TableInput
                            { tinpName = DmnCommon ( Just "InputClause_0pcbpc9" ) Nothing
                            , tinpLabel = ColumnLabel { columnLabel = "How many guests" }
                            , tinpExpr = InputExpression
                                { inputExprLabel = DmnCommon ( Just "LiteralExpression_1uu3xe6" ) Nothing
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
                            { ruleLabel = DmnCommon ( Just "row-884555325-1" ) Nothing }
                        , Rule
                            { ruleLabel = DmnCommon ( Just "row-506282952-1" ) Nothing }
                        , Rule
                            { ruleLabel = DmnCommon ( Just "row-506282952-2" ) Nothing }
                        , Rule
                            { ruleLabel = DmnCommon ( Just "row-506282952-3" ) Nothing }
                        , Rule
                            { ruleLabel = DmnCommon ( Just "row-506282952-4" ) Nothing }
                        , Rule
                            { ruleLabel = DmnCommon ( Just "row-506282952-5" ) Nothing }
                        , Rule
                            { ruleLabel = DmnCommon ( Just "row-506282952-6" ) Nothing }
                        ]
                    }
                )
            }
        ]
    , defInputData =
        [ InputData { inpLabel = dmnNamed "InputData_0rin549" "Season" }
        , InputData { inpLabel = dmnNamed "InputData_1axnom3" "Number of Guests" }
        , InputData { inpLabel = dmnNamed "InputData_0pgvdj9" "Guests with children?" }
        ]
    , defDrgElems =
        [ KnowledgeSource { knsLabel = dmnNamed "KnowledgeSource_0b8hnqo" "Men's Cookbook" } ]
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
