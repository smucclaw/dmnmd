{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module DMN.SFeelGrammar where

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Options.Applicative (Alternative((<|>)))
import DMN.Types
import DMN.ParsingUtils
import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.Fixed (Fixed)

-- 9.2 S-FEEL syntax
-- The syntax for the S-FEEL expressions used in this section is specified in the EBNF below: it is a subset of the FEEL
-- syntax specified in clause 10.3.1.2.
-- Grammar rules:

-- 1. expression = simple expression ;

expression :: Parser SimpleExpression
expression = simpleExpression

-- 2. arithmetic expression =
-- 2.a addition | subtraction |
-- 2.b multiplication | division |
-- 2.c exponentiation |
-- 2.d arithmetic negation ;

-- TODO: This should have precidences
-- arithmeticExpression =
--     addition <|> subtraction <|>
--     multiplication <|> division <|>
--     exponentiation <|>
--     arithmeticNegation

arithmeticExpression = makeExprParser term table <?> "arithmeticExpression"

-- expr = makeExprParser term table <?> "expression"

term = undefined
-- term = parens expr <|> integer <?> "term"

table :: [[Operator Parser Expression]]
table = [ [ Prefix arithmeticNegation ]
        , [ InfixL exponentiation ]
        , [ InfixL multiplication
          , InfixL division  ]
        , [ InfixL addition
          , InfixL subtraction  ] ]


-- TODO
symbol = id

{-

expr = makeExprParser term table <?> "expression"

term = parens expr <|> integer <?> "term"

table = [ [ prefix  "-"  negate
          , prefix  "+"  id ]
        , [ postfix "++" (+1) ]
        , [ binary  "*"  (*)
          , binary  "/"  div  ]
        , [ binary  "+"  (+)
          , binary  "-"  (-)  ] ]

binary  name f = InfixL  (f <$ symbol name)
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

-}



-- 3 simple expression = arithmetic expression | simple value | comparison ;

data SimpleExpression = Expr Expression

simpleExpression :: Parser SimpleExpression
simpleExpression = Expr <$> arithmeticExpression <|> _ <$> simpleValue <|> _ <$> comparison

-- 4 simple expressions = simple expression , { "," , simple expression } ;

simpleExpressions :: Parser [SimpleExpression]
simpleExpressions = sepBy simpleExpression ","

-- 5 simple positive unary test =
-- 5.a [ "<" | "<=" | ">" | ">=" ] , endpoint |
-- 5.b interval ;

type UnaryTest = ()

simplePositiveUnaryTest :: Parser UnaryTest
simplePositiveUnaryTest = do
    op <- ( "<" <|> "<=" <|> ">" <|> ">=" ) 
    endpt <- endpoint
    pure $ () -- UnaryTest op endpt
  <|> interval

-- 6 interval = ( open interval start | closed interval start ) 
--            , endpoint , ".." , endpoint 
--            , ( open interval end | closed interval end ) ;

interval :: Parser UnaryTest
interval = do
    (openIntervalStart <|> closedIntervalStart)
    endpoint 
    ".."
    endpoint 
    (openIntervalEnd <|> closedIntervalEnd)
    pure ()


-- 7 open interval start = "(" | "]" ;
openIntervalStart :: Parser Text
openIntervalStart = "(" <|> "]"

-- 8 closed interval start = "[" ;

closedIntervalStart :: Parser Text
closedIntervalStart = "]"

-- 9 open interval end = ")" | "[" ;

openIntervalEnd :: Parser Text
openIntervalEnd = ")" <|> "["

-- 10 closed interval end = "]" ;

closedIntervalEnd :: Parser Text
closedIntervalEnd = "]"

-- 11 simple positive unary tests = simple positive unary test , { "," , simple positive unary test } ;

simplePositiveUnaryTests :: Parser [UnaryTest]
simplePositiveUnaryTests = sepBy simplePositiveUnaryTest ","

-- 12 simple unary tests =
-- 12.a simple positive unary tests |
-- 12.b "not", "(", simple positive unary tests, ")" |
-- 12.c "-";

simpleUnaryTests :: Parser [UnaryTest]
simpleUnaryTests = simplePositiveUnaryTests 
                <|> "not" *> "(" *> simplePositiveUnaryTests <* ")"
                <|> [] <$ "-"

-- 13 endpoint = simple value ;

endpoint :: Parser SimpleValue
endpoint = simpleValue

-- 14 simple value = qualified name | simple literal ;

data SimpleValue = QName [String] | SimpleLiteral SimpleLiteral

simpleValue :: Parser SimpleValue
simpleValue = qualifiedName <|> SimpleLiteral <$> simpleLiteral

-- 15 qualified name = name , { "." , name } ;

qualifiedName :: Parser SimpleValue
qualifiedName = QName <$> sepBy1 name "."

type BinOp a = a -> a -> a

data Expression 
    = Add Expression Expression
    | Sub Expression Expression
    | Mul Expression Expression
    | Div Expression Expression
    | Exp Expression Expression
    | Neg Expression

-- 16 addition = expression , "+" , expression ;

addition :: Parser (BinOp Expression)
addition = Add <$ "+"

-- 17 subtraction = expression , "-" , expression ;

subtraction :: Parser (BinOp Expression)
subtraction = Sub <$ "-"

-- 18 multiplication = expression , "*" , expression ;

multiplication :: Parser (BinOp Expression)
multiplication = Mul <$ "*"

-- 19 division = expression , "/" , expression ;

division :: Parser (BinOp Expression)
division = Div <$ "/"

-- 20 exponentiation = expression, "**", expression ;

exponentiation :: Parser (BinOp Expression)
exponentiation = Exp <$ "**"

-- 21 arithmetic negation = "-" , expression ;

arithmeticNegation :: Parser (Expression -> Expression)
arithmeticNegation = Neg <$ "-"

-- 22 name = name start , { name part | additional name symbols } ;

name :: Parser String
name = do
    x <- nameStart
    xs <- many (namePart <|> additionalNameSymbols)
    return $ x : xs

-- 23 name start = name start char, { name part char } ;

nameStart :: Parser Char
nameStart = satisfy nameStartChar -- The above is nonsense

-- 24 name part = name part char , { name part char } ;

namePart :: Parser Char
namePart = satisfy namePartChar -- TODO: Better errors

-- 25 name start char = "?" | [A-Z] | "_" | [a-z] | [\uC0-\uD6] | [\uD8-\uF6] | [\uF8-\u2FF] | [\u370-\u37D] | [\u37F-\u1FFF]
-- | [\u200C-\u200D] | [\u2070-\u218F] | [\u2C00-\u2FEF] | [\u3001-\uD7FF] | [\uF900-\uFDCF] | [\uFDF0-\uFFFD] |
-- [\u10000-\uEFFFF] ;

-- name_start_char :: String
-- name_start_char = "?" ++ ['A'..'Z'] ++ "_" ++ ['a'..'z'] ++ ['\xC0'..'\xD6'] ++ ['\xD8'..'\xF6'] ++ ['\xF8'..'\x2FF'] ++ ['\x370'..'\x37D'] ++ ['\x37F'..'\x1FFF']
--   ++ ['\x200C'..'\x200D'] ++ ['\x2070'..'\x218F'] ++ ['\x2C00'..'\x2FEF'] ++ ['\x3001'..'\xD7FF'] ++ ['\xF900'..'\xFDCF'] ++ ['\xFDF0'..'\xFFFD'] ++
--   ['\x10000'..'\xEFFFF'] 

-- parseClass (a:'-':b:xs) = [a..b] ++ parseClass xs; parseClass (x:xs) = x:parseClass xs; parseClass [] = []

nameStartChar :: Char -> Bool
nameStartChar = inClass "?A-Z_a-z\xC0-\xD6\xD8-\xF6\xF8-\x2FF\x370-\x37D\x37F-\x1FFF"

-- 26 name part char = name start char | digit | \uB7 | [\u0300-\u036F] | [\u203F-\u2040] ;

namePartChar :: Char -> Bool
namePartChar c = nameStartChar c || inClass "0-9\xB7\x0300-\x036F\x203F-\x2040" c

-- 27 additional name symbols = "." | "/" | "-" | "’" | "+" | "*" ;

-- TODO: Improve this. Also, it'll probably cause a ton of trouble. Why do we allow this?
additionalNameSymbols :: Parser Char
additionalNameSymbols = char '.' <|> char '/' <|> char '-' <|> char '’' <|> char '+' <|> char '*'

-- 28 simple literal = numeric literal | string literal | boolean literal | date time literal ;

data SimpleLiteral = NumericLiteral Scientific | StringLiteral String 
                   | BooleanLiteral Bool | DateLiteral Date

simpleLiteral :: Parser SimpleLiteral
simpleLiteral = numericLiteral <|> stringLiteral <|> booleanLiteral <|> dateTimeLiteral

-- 29 string literal = """, { character – (""" | vertical space) | string escape sequence}, """ ;

stringLiteral :: Parser SimpleLiteral
stringLiteral = StringLiteral <$> do
    "\""
    x <- many (satisfy (\c -> c /= '\"' && not (isVerticalSpace c)) <?> "stringChar")
    "\""
    pure x


-- 30 boolean literal = "true" | "false" ;

booleanLiteral :: Parser SimpleLiteral
booleanLiteral = BooleanLiteral <$> (True <$ "true" <|> False <$ "false")

-- 31 numeric literal = [ "-" ] , ( digits , [ ".", digits ] | "." , digits ) ;

-- TODO: This is an ugly hack
numericLiteral :: Parser SimpleLiteral
-- numericLiteral = NumericLiteral <$> L.scientific
numericLiteral = NumericLiteral <$> do
    sign <- negate <$ "-" <|> pure id
    (intPart, fracPart) <-
            (,) <$> some digit <*> option "0" ("," *> some digit)
        <|> (,) "0" <$> some digit

    pure . sign $ read $ intPart ++ "." ++ fracPart

-- 32 digit = [0-9] ;
-- 33 digits = digit , {digit} ;
-- 34 date time literal = ("date" | "time" | "duration" ) , "(" , string literal , ")" ;

data Date = Date Text SimpleLiteral

dateTimeLiteral :: Parser SimpleLiteral
dateTimeLiteral = do
    timeType <- "date" <|> "time" <|> "duration"
    "("
    str <- stringLiteral
    ")"
    pure . DateLiteral $ Date timeType str

-- 35 comparison = expression , ( "=" | "!=" | "<" | "<=" | ">" | ">=" ) , expression ;

comparison = undefined
-- 36 white space = vertical space | \u0009 | \u0020 | \u0085 | \u00A0 | \u1680 | \u180E | [\u2000-\u200B] | \u2028 | \u2029
-- | \u202F | \u205F | \u3000 | \uFEFF ;

whitespace = undefined

-- 37 vertical space = [\u000A-\u000D];

isVerticalSpace = undefined

-- 38 string escape sequence = "\'" | "\"" | "\\" | "\n" | "\r" | "\t" | "\u", hex digit, hex digit, hex digit, hex digit;

stringEscapeSequence = undefined

