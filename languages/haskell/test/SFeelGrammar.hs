-- {-# LANGUAGE OverloadedStrings, DuplicateRecordFields, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module SFeelGrammar where

import DMN.Types
import DMN.ParseFEEL
import Test.Hspec
import DMN.SFeelGrammar
import ParserSpecHelpers
import Text.Megaparsec
import Data.Text (Text)

sfeelSpec :: Spec
sfeelSpec = do
  describe "SFeelGrammar's stringEscapeSequence should..." $ do
      it "parse an escaped backslash"       $ "\\\\"    ~> stringEscapeSequence `shouldParse` "\\\\"
      it "parse an escaped quote"           $ "\\\""    ~> stringEscapeSequence `shouldParse` "\\\""
      it "parse an escaped unicode"         $ "\\uaaaa" ~> stringEscapeSequence `shouldParse` "\\uaaaa"
      it "parse an escaped tab"             $ "\\t"     ~> stringEscapeSequence `shouldParse` "\\t"
      it "parse an escaped newline"         $ "\\n"     ~> stringEscapeSequence `shouldParse` "\\n"
      it "parse an escaped carriage return" $ "\\r"     ~> stringEscapeSequence `shouldParse` "\\r"
      it "parse an escaped UNICODE"         $ "\\uAAAA" ~> stringEscapeSequence `shouldParse` "\\uAAAA"
      it "fail to parse a bad hex-string"   $              stringEscapeSequence `shouldFailOn` "\\uaaza"
      it "parse something containing a couple of escape sequences" $
         "\"foo \\\\ bar \\t baz\""    ~> stringLiteral `shouldParse` "foo \\\\ bar \\t baz"
--      it "should fail to parse incorrect expressions" $ 
--          (parseFNumFunction <* eof) `shouldFailOn` "age * 100.1.1"

--  describe "SFeelGrammar's name token parser should..." $ do
--      it "parse Name Token" $ "Name    Token" ~>  `shouldParse` NameToken "Name Token"

  describe "SFeelGrammar's arithExpr parser should..." $ do
     it "parse a simple addition" $
        "1+2" ~> arithmeticExpression `shouldParse` Expr (Add (SimpleValue . SimpleLiteral $ NumericLiteral 1)
                                                              (SimpleValue . SimpleLiteral $ NumericLiteral 2))
     it "parse a simple addition with spaces" $
        "1 + 2" ~> arithmeticExpression `shouldParse` Expr (Add (SimpleValue . SimpleLiteral $ NumericLiteral 1)
                                                                (SimpleValue . SimpleLiteral $ NumericLiteral 2))
     it "parse a simple addition with multiple spaces" $
        "1  +  2" ~> arithmeticExpression `shouldParse` Expr (Add (SimpleValue . SimpleLiteral $ NumericLiteral 1)
                                                                  (SimpleValue . SimpleLiteral $ NumericLiteral 2))
