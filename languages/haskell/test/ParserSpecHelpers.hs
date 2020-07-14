
{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}

module ParserSpecHelpers (
    module ParserSpecHelpers
    , shouldParse

) where

import Test.Hspec
import Data.Text (Text)
import Text.Megaparsec hiding (label)
import qualified Test.Hspec.Megaparsec as HM
import Test.Hspec.Megaparsec (shouldParse)
import DMN.ParsingUtils
import Data.Void (Void)

-- * Helper functions

-- shouldParse :: Parser a -> a -> Expectation
-- shouldParse = undefined -- HM.shouldParse . parse ""

-- shouldParse = HM.shouldParse

(~>) :: Text -> Parser a -> Either (ParseErrorBundle Text Void) a
t ~> p = parse (p <* eof) "" t

shouldSucceedOn :: Show a => Parser a -> Text -> Expectation
shouldSucceedOn p = HM.shouldSucceedOn $ parse p ""

shouldFailOn :: Show a => Parser a -> Text -> Expectation
shouldFailOn p = HM.shouldFailOn $ parse p ""

throwOnLeft :: HasCallStack => Either String a -> a
throwOnLeft = either error id