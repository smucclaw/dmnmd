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
import Data.Scientific

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
      it "fail to parse a bad hex-string"   $              stringLiteral        `shouldFailOn` "\"\\uaaza\""
      it "fail to parse newline"            $              stringLiteral        `shouldFailOn` "\"\\uaaza\""
      it "parse something containing a couple of escape sequences" $
         "\"foo \\\\ bar \\t baz\""    ~> stringLiteral `shouldParse` "foo \\\\ bar \\t baz"
--      it "should fail to parse incorrect expressions" $ 
--          (parseFNumFunction <* eof) `shouldFailOn` "age * 100.1.1"

--  describe "SFeelGrammar's name token parser should..." $ do
--      it "parse Name Token" $ "Name    Token" ~>  `shouldParse` NameToken "Name Token"

  describe "SFeelGrammar's arithExpr parser should..." $ do
     it "parse a simple addition" $
        "1+2" ~> arithmeticExpression `shouldParse` onePlusTwo
     it "parse a simple addition with spaces" $
        "1 + 2" ~> arithmeticExpression `shouldParse` onePlusTwo
     it "parse a simple addition with multiple spaces" $
        "1 \t +  2" ~> arithmeticExpression `shouldParse` onePlusTwo
     it "parse a simple addition with funky whitespace" $
        "1\160+\160\&2" ~> arithmeticExpression `shouldParse` onePlusTwo

  describe "SFeelGrammar's number parser should..." $ do
     it "Allow '.5' as an alias for 0.5" $
        ".5" ~> numericLiteral `shouldParse` NumericLiteral 0.5

  describe "SFeelGrammar's number parser should..." $ do
     it "Allow '.5' as an alias for 0.5" $
        ".5" ~> numericLiteral `shouldParse` NumericLiteral 0.5

  describe "SFeelGrammar's comparison parser should..." $ do
     it "handle unary comparison <" $
        "< 18"     ~> simpleExpression `shouldParse` Comparison (UnaryTest Lt (SimpleLiteral (NumericLiteral (scientific 18 0))))
     it "handle unary comparison <=" $
        "<= 18"     ~> simpleExpression `shouldParse` Comparison (UnaryTest Le (SimpleLiteral (NumericLiteral (scientific 18 0))))
     it "handle unary comparison >=" $
        ">= 18"     ~> simpleExpression `shouldParse` Comparison (UnaryTest Ge (SimpleLiteral (NumericLiteral (scientific 18 0))))
     it "handle unary comparison >" $
        "> 18"     ~> simpleExpression `shouldParse` Comparison (UnaryTest Gt (SimpleLiteral (NumericLiteral (scientific 18 0))))
     it "handle interval comparison" $
        "[18..21]" ~> simpleExpression `shouldParse` Comparison (Interval
                                                                  Closed (SimpleLiteral (NumericLiteral (scientific 18 0)))
                                                                  (SimpleLiteral (NumericLiteral (scientific 21 0))) Closed
                                                                )
     it "handle interval comparison involving negative ints" $
       "[18..-21]" ~> simpleExpression `shouldParse` Comparison (Interval
                                                                  Closed (SimpleLiteral (NumericLiteral (scientific 18 0)))
                                                                  (SimpleLiteral (NumericLiteral (scientific (-21) 0))) Closed
                                                                )

     it "handle interval comparison involving negative ints" $
       "[-1..21]" ~> simpleExpression `shouldParse` Comparison (Interval
                                                                  Closed (SimpleLiteral (NumericLiteral (scientific (-1) 0)))
                                                                  (SimpleLiteral (NumericLiteral (scientific 21 0))) Closed
                                                                )

onePlusTwo = Expr (Add (SimpleValue . SimpleLiteral $ NumericLiteral 1)
                       (SimpleValue . SimpleLiteral $ NumericLiteral 2))


