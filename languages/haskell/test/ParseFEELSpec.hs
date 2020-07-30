-- {-# LANGUAGE OverloadedStrings, DuplicateRecordFields, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module ParseFEELSpec where

-- import Control.Monad
-- import Text.RawString.QQ
-- import DMN.DecisionTable
import DMN.Types
-- import DMN.ParseTable
import DMN.ParseFEEL
import Test.Hspec
-- import Control.Applicative 
-- import Data.Text (Text)
-- import qualified Data.Text as T
import ParserSpecHelpers
import Text.Megaparsec

feelSpec :: Spec
feelSpec = do
  describe "parseFEEL" $ do
      it "should parse correct simple expressions" $ 
        "age * 100" ~> parseFNumFunction `shouldParse` FNF3 (FNF1 "age") FNMul (FNF0 (VN 100.0))
      it "should fail to parse incorrect expressions" $ 
          (parseFNumFunction <* eof) `shouldFailOn` "age * 100.1.1"