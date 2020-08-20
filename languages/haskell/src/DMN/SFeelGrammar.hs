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
import qualified Data.Text as T

-- 9.2 S-FEEL syntax
-- The syntax for the S-FEEL expressions used in this section is specified in the EBNF below: it is a subset of the FEEL
-- syntax specified in clause 10.3.1.2.
-- Grammar rules:

-- 1. expression = simple expression ;

type Expression = SimpleExpression

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

arithmeticExpression :: Parser Expression
arithmeticExpression = makeExprParser term table <?> "arithmeticExpression"

-- expr = makeExprParser term table <?> "expression"

term :: Parser SimpleExpression
term = SimpleValue <$> simpleValue
-- term = parens expr <|> integer <?> "term"

table :: [[Operator Parser Expression]]
table = [ [ Prefix arithmeticNegation ]
        , [ InfixL exponentiation ]
        , [ InfixL multiplication
          , InfixL division  ]
        , [ InfixL addition
          , InfixL subtraction  ] ]

-- binary  name f = InfixL  (f <$ symbol name)
-- prefix  name f = Prefix  (f <$ symbol name)
-- postfix name f = Postfix (f <$ symbol name)

-- TODO
-- symbol :: (Tokens s0 -> m0 (Tokens s0))
symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer 

spaceConsumer :: Parser ()
spaceConsumer = space

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

data SimpleExpression
  = Expr (ArithmeticExpression Expression)
  | SimpleValue SimpleValue
  | Comparison Comparison
  deriving (Show, Eq)

simpleExpression :: Parser SimpleExpression
simpleExpression = arithmeticExpression 
               <|> term
               <|> Comparison <$> comparison

-- 4 simple expressions = simple expression , { "," , simple expression } ;

simpleExpressions :: Parser [SimpleExpression]
simpleExpressions = sepBy simpleExpression ","

-- 5 simple positive unary test =
-- 5.a [ "<" | "<=" | ">" | ">=" ] , endpoint |
-- 5.b interval ;

data UTComparison = Lt | Le | Gt | Ge

-- TODO
data UnaryTest = UnaryTest UTComparison Endpoint | Interval IsOpen Endpoint Endpoint IsOpen


simplePositiveUnaryTest :: Parser UnaryTest
simplePositiveUnaryTest = do
    op <-  Lt <$ "<" <|> Le <$ "<=" <|> Gt <$ ">" <|> Ge <$ ">="  
    endpt <- endpoint
    pure $ UnaryTest op endpt
  <|> interval

-- 6 interval = ( open interval start | closed interval start ) 
--            , endpoint , ".." , endpoint 
--            , ( open interval end | closed interval end ) ;

data IsOpen = Open | Closed
    deriving (Eq, Show)

interval :: Parser UnaryTest
interval = do
    leftOpen <- (openIntervalStart <|> closedIntervalStart)
    leftEndpoint <- endpoint 
    ".."
    rightEndpoint <- endpoint 
    rightOpen <- (openIntervalEnd <|> closedIntervalEnd)
    pure $ Interval leftOpen leftEndpoint rightEndpoint rightOpen

-- 7 open interval start = "(" | "]" ;
-- 8 closed interval start = "[" ;
-- 9 open interval end = ")" | "[" ;
-- 10 closed interval end = "]" ;
openIntervalStart, closedIntervalStart, openIntervalEnd, closedIntervalEnd :: Parser IsOpen
openIntervalStart   = Open   <$ ("(" <|> "]")
closedIntervalStart = Closed <$ "]"
openIntervalEnd     = Open   <$ (")" <|> "[")
closedIntervalEnd   = Closed <$ "]"

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

type Endpoint = SimpleValue

endpoint :: Parser Endpoint
endpoint = simpleValue

-- 14 simple value = qualified name | simple literal ;

data SimpleValue = QName [String] | SimpleLiteral SimpleLiteral
  deriving (Show, Eq)

simpleValue :: Parser SimpleValue
simpleValue = qualifiedName <|> SimpleLiteral <$> simpleLiteral

-- 15 qualified name = name , { "." , name } ;

qualifiedName :: Parser SimpleValue
qualifiedName = QName <$> sepBy1 name "."

type BinOp a = a -> a -> a

data ArithmeticExpression expr
    = Add expr expr
    | Sub expr expr
    | Mul expr expr
    | Div expr expr
    | Exp expr expr
    | Neg expr
  deriving (Show, Eq)

-- 16 addition = expression , "+" , expression ;
-- 17 subtraction = expression , "-" , expression ;
-- 18 multiplication = expression , "*" , expression ;
-- 19 division = expression , "/" , expression ;
-- 20 exponentiation = expression, "**", expression ;

addition, subtraction, multiplication, division, exponentiation :: Parser (BinOp Expression)
addition       = wrapExpr Add "+"
subtraction    = wrapExpr Sub "-"
multiplication = wrapExpr Mul "*"
division       = wrapExpr Div "/"
exponentiation = wrapExpr Exp "**"

wrapExpr :: (a1 -> a2 -> ArithmeticExpression Expression) -> Text -> Parser (a1 -> a2 -> SimpleExpression)
wrapExpr op str = (Expr .) . op <$ symbol str

-- 21 arithmetic negation = "-" , expression ;

arithmeticNegation :: Parser (Expression -> Expression)
arithmeticNegation = Expr . Neg <$ "-"

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

data SimpleLiteral = NumericLiteral Scientific | StringLiteral Text
                   | BooleanLiteral Bool | DateLiteral Date
  deriving (Show, Eq)

simpleLiteral :: Parser SimpleLiteral
simpleLiteral = numericLiteral <|> StringLiteral <$> stringLiteral <|> booleanLiteral <|> dateTimeLiteral

-- 29 string literal = """, { character – (""" | vertical space) | string escape sequence}, """ ;

-- Shouldn't it be?
-- 29 string literal = """, { character – (""" | vertical space | "\") | string escape sequence}, """ ;

stringLiteral :: Parser Text
stringLiteral =  do
    "\""
    x <- T.concat <$> many (nonEscapeChar <|> stringEscapeSequence)
    "\""
    pure x

-- TODO: This could be faster, by parsing a long sequence at a time
nonEscapeChar :: Parser Text
-- nonEscapeChar = charToText <$> satisfy (\c -> c /= '\"' && not (isVerticalSpace c)) <?> "stringChar" 
nonEscapeChar = charToText <$> noneOf ('"' : '\\' : ['\n' .. '\r'])

charToText :: Char -> Text
charToText = T.singleton


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
            (,) <$> some digit <*> option "0" ("." *> some digit)
        <|> (,) "0" <$ "." <*> some digit

    spaceConsumer
    pure . sign $ read $ intPart ++ "." ++ fracPart

-- 32 digit = [0-9] ;
-- 33 digits = digit , {digit} ;
-- 34 date time literal = ("date" | "time" | "duration" ) , "(" , string literal , ")" ;

data TimeType = Date | Time | Duration
  deriving (Show, Eq)

data Date = DateThing TimeType Text
  deriving (Show, Eq)

dateTimeLiteral :: Parser SimpleLiteral
dateTimeLiteral = do
    timeType <- Date <$ "date" <|> Time <$ "time" <|> Duration <$ "duration"
    "("
    str <- stringLiteral
    ")"
    pure . DateLiteral $ DateThing timeType str

-- 35 comparison = expression , ( "=" | "!=" | "<" | "<=" | ">" | ">=" ) , expression ;

type Comparison = ()

comparison :: Parser Comparison
comparison = undefined

-- 36 white space = vertical space | \u0009 | \u0020 | \u0085 | \u00A0 | \u1680 | \u180E | [\u2000-\u200B] | \u2028 | \u2029
-- | \u202F | \u205F | \u3000 | \uFEFF ;

whiteSpace :: Char -> Bool
whiteSpace x = isVerticalSpace x || horizontalSpace x

horizontalSpace :: Char -> Bool
horizontalSpace = inClass "\x0009\x0020\x0085\x00A0\x1680\x180E\x2000-\x200B\x2028\x2029\x202F\x205F\x3000\xFEFF"

-- whitespace = undefined

-- 37 vertical space = [\u000A-\u000D];

isVerticalSpace :: Char -> Bool
isVerticalSpace = inClass "\x000A-\x000D"

-- 38 string escape sequence = "\'" | "\"" | "\\" | "\n" | "\r" | "\t" | "\u", hex digit, hex digit, hex digit, hex digit;

stringEscapeSequence :: Parser Text
stringEscapeSequence = do
    "\\'"
        <|> "\\\""
        <|> "\\\\"
        <|> "\\n"
        <|> "\\r"
        <|> "\\t"
        <|> listOfText ["\\u", hexDigit, hexDigit, hexDigit, hexDigit]

listOfText :: [Parser Text] -> Parser Text
listOfText = fmap T.concat . sequence

hexDigit :: Parser Text
hexDigit = charToText <$> satisfy (inClass "0-9a-fA-F") <?> "Hex digit"

-- stringEscapeSequence = do
--     "\\"
--     "'"
--         <|> "\""
--         <|> "\\"
--         <|> "\n" <$ "n"
--         <|> "\r" <$ "r"
--         <|> "\t" <$ "t"
--         -- <|> "u", hex digit, hex digit, hex digit, hex digit