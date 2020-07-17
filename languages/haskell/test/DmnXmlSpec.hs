{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, QuasiQuotes #-}

module DmnXmlSpec where

-- import Control.Monad
import Text.RawString.QQ
-- import DMN.DecisionTable
-- import DMN.Types
-- import DMN.ParseTable
-- import DMN.ParseFEEL
import Test.Hspec
-- import Test.Hspec.Attoparsec
-- import Data.Either (fromRight)
-- import Control.Applicative 
-- import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

-- import Text.XML.HXT.Core

xmlSpec :: Spec
xmlSpec = do
  describe "parseXml" $ do
      it "should parse a trivial xml file" $ pendingWith "Not yet"



-- spec1 :: Spec
-- spec1 = do
--   describe "parseHelloWorld" $ do
--     it "should parse the phrase 'Hello World!'" $
--       parseHelloWorld `shouldSucceedOn` ("Hello World!" :: Text)

ex1 :: Text
ex1 = T.pack $ dropWhile (=='\n') [r|
<?xml version="1.0" encoding="UTF-8"?>
<definitions xmlns="https://www.omg.org/spec/DMN/20191111/MODEL/" xmlns:dmndi="https://www.omg.org/spec/DMN/20191111/DMNDI/" xmlns:dc="http://www.omg.org/spec/DMN/20180521/DC/" xmlns:di="http://www.omg.org/spec/DMN/20180521/DI/" xmlns:camunda="http://camunda.org/schema/1.0/dmn" id="dinnerDecisions" name="Dinner Decisions" namespace="http://camunda.org/schema/1.0/dmn">
</definitions>
|]