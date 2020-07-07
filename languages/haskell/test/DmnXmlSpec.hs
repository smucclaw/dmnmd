{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, QuasiQuotes #-}

module DmnXmlSpec where

import Control.Monad
import Text.RawString.QQ
import DMN.DecisionTable
import DMN.Types
import DMN.ParseTable
import DMN.ParseFEEL
import Test.Hspec
import Test.Hspec.Attoparsec
import Data.Either (fromRight)
import Control.Applicative 
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

xmlSpec :: Spec
xmlSpec = do
  describe "parseXml" $ do
      it "should parse a trivial xml file" $ pending

-- spec1 :: Spec
-- spec1 = do
--   describe "parseHelloWorld" $ do
--     it "should parse the phrase 'Hello World!'" $
--       parseHelloWorld `shouldSucceedOn` ("Hello World!" :: Text)