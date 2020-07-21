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

-- import Text.XML.HXT.Core

xmlSpec :: Spec
xmlSpec = do
  describe "parseXml" $ do
    it "should parse a trivial xml file" $ do
      output <- parseDMN "test/simple.dmn"
      output
        `shouldBe` [ Definitions
                       { defLabel = DmnCommon {dmnId = "dinnerDecisions", dmnName = "Dinner Decisions"},
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
simulationDmn = [
  Definitions
    { defLabel = DmnCommon "dinnerDecisions" "Dinner Decisions"
    , descisionsDiagrams =
        [ Decision
            { decLabel = DmnCommon "beverages" "Beverages"
            }
        , Decision
            { decLabel = DmnCommon "dish" "Dish"
            }
        ]
    , defInputData =
        [ InputData
            { inpLabel = DmnCommon "InputData_0rin549" "Season"
            }
        , InputData
            { inpLabel = DmnCommon "InputData_1axnom3" "Number of Guests"
            }
        , InputData
            { inpLabel = DmnCommon "InputData_0pgvdj9" "Guests with children?"
            }
        ]
    , defDrgElems 
      =
        [ KnowledgeSource
            { knsLabel = DmnCommon "KnowledgeSource_0b8hnqo" "Men's Cookbook"
            }
        ]
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
