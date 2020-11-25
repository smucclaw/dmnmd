{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module DMN.ParseFEEL where

import Prelude hiding (takeWhile)
import Data.Maybe (fromJust)
import Control.Applicative hiding (many, some) 
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import Data.Char (isPrint)
import Data.List (nub)
import qualified Data.Text as T
import DMN.Types
import DMN.ParsingUtils
import qualified Data.Void
import qualified Data.Functor.Identity
import Debug.Trace

-- see Text.Megaparsec.Char.Lexer documentation
symbol :: Tokens Text -> ParsecT Data.Void.Void Text Data.Functor.Identity.Identity (Tokens Text)
symbol    = L.symbol space

brackets, parens :: ParsecT Data.Void.Void Text Data.Functor.Identity.Identity a -> ParsecT Data.Void.Void Text Data.Functor.Identity.Identity a
parens    = between (symbol "(") (symbol ")")
brackets  = between (symbol "[") (symbol "]")

lexeme :: ParsecT Data.Void.Void Text Data.Functor.Identity.Identity a -> ParsecT Data.Void.Void Text Data.Functor.Identity.Identity a
lexeme    = L.lexeme hspace

-- parser for low-level expressions, e.g. FEEL, common to all DMN syntaxes.

-- let's allow spaces in variable names. what could possibly go wrong?
parseVarname :: Parser Text
parseVarname = try $ do
  firstLetter <- letterChar
  -- remainder <- takeWhileP Nothing (\c -> c /= ':' && c /= '|' && c /= '(' ) -- inClass "a-zA-Z0-9_"
  remainder <- takeWhileP Nothing (inClass "a-zA-Z0-9_ ")
  -- remainder <- takeWhileP Nothing (inClass $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_ ")
  return $ T.strip $ T.append (T.singleton firstLetter) remainder


-- for input cells
parseDataCell :: Maybe DMNType -> Parser [FEELexp]
parseDataCell dmntype = ([] <$ (lexeme eof)) <|>
                        do
  fexp  <- parseFEELexp dmntype
  fexps <- many $ symbol "," *> parseFEELexp dmntype
  _ <- eof
  return $  fexp : fexps


-- output cells have a slightly wider range of recognized expressions -- we allow FFunctions (which are still in FEELexp)
parseOutCell :: Maybe DMNType -> Parser [FEELexp]
parseOutCell dmntype = try $ do
  fexp  <- parseFEELext dmntype
  fexps <- many $ symbol "," *> parseFEELext dmntype
  _ <- eof
  return $  fexp : fexps

-- note -- "exp" for expression, "ext" for extended
parseFEELext :: Maybe DMNType -> Parser FEELexp
parseFEELext mdt = try (parseFEELexp mdt) <|> (FFunction <$> parseFNumFunction mdt)

mkF :: Maybe DMNType -> String -> FEELexp
mkF maybetype myinput =
  either (error . errorBundlePretty) id (runParser (parseFEELexp maybetype) "(2nd parse mkF)" (T.pack myinput))

parseFEELexp :: Maybe DMNType -> Parser FEELexp
parseFEELexp mdt = (try ((char '_' <|> char '-') >> eof >> return FAnything)
                    <|> parseFEELexp' mdt <*  (lookAhead (char ',' >> return ()) <|> eof))

parseFEELexp' :: Maybe DMNType -> Parser FEELexp
parseFEELexp' (Just (DMN_List x)) = parseFEELexp' (Just x)

-- if we're given "Nothing" and "DMN_String" hints, we always return a string -- FNullary VS.
-- a subsequent reprocessing stage will perform type inference and retype the column and data cells.

parseFEELexp' Nothing = do
  inner <- (hspace *> manyTill anyChar eof)
  return $ FNullary (VS inner)
-- strings are tricky because they could be FEEL expression variable names like "Dish Name"
-- or just literal strings like "Lentil Soup"

-- string expressions can be `like this` or `"quoted"`.
parseFEELexp' (Just DMN_String)  = (FNullary . VS <$> parseBareString)

parseFEELexp' (Just DMN_Boolean) = do
  choice [ try $ FNullary (VB True)  <$ (tryChoice (symbol <$> T.words "True true yes t y positive"))
         , try $ FNullary (VB False) <$ (tryChoice (symbol <$> T.words "False false no f n negative"))
         ] <* (lookAhead (char ',' >> return ()) <|> eof)

parseFEELexp' (Just DMN_Number) = do
  choice [ try $ FSection Flte . VN <$> (symbol "<=" *> someNum)
         , try $ FSection Flt  . VN <$> (symbol "<"  *> someNum)
         , try $ FSection Fgte . VN <$> (symbol ">=" *> someNum)
         , try $ FSection Fgt  . VN <$> (symbol ">"  *> someNum)
         , try $ FSection Fgt  . VN <$> (someNum <* symbol "<=")
         , try $ FSection Fgte . VN <$> (someNum <* symbol "<" )
         , try $ FSection Flt  . VN <$> (someNum <* symbol ">=")
         , try $ FSection Flte . VN <$> (someNum <* symbol ">" )
         , try $ FSection Feq  . VN <$> (someNum <* symbol "=" )
         , try $ FSection Feq  . VN <$> (symbol "=" *> someNum )
         , try $ do -- Yoda would be pissed
             lower <- symbol "[" *> lexeme L.decimal
             upper <- symbol ".." *> lexeme L.decimal <* symbol "]"
             return $ FInRange lower upper
         , try $ FNullary      . VN <$> (someNum)
         ]


parseBareString :: Parser String
parseBareString = (try nocommas <|> stringLiteral)

nocommas :: Parser String
nocommas = unwords <$> some ( lexeme ( some $ satisfy (\c -> isPrint c && c /= ',' ) ) )

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

-- for type inference, in the 2nd step; we can assume we are working on input columns only
-- so we can blithely split on commas.
-- ideally we would be able to parse     true,false  using only Megaparsec
-- but the backtracking doesn't seem to work that way -- instead   true,false  seems to parse as a string?

-- TODO: remove duplication vs parseDataCell; it makes no sense to duplicate the parsing logic. Instead we should just call parseDataCell and throw away the content, preserving only the type.
parseDMNType :: Parser (Maybe DMNType)
parseDMNType = try (Nothing <$ (choice ( symbol <$> (T.words "- _")) ) <* eof )
               <|> (Nothing <$ (hspace >> eof))
               <|> do
  first <- innerParse <* hspace
  after <- many (symbol "," *> innerParse)
  eof
  --Debug.Trace.trace ("parseDMNType: returning " ++ show(first:after)) $
  return $ flattenTypes (first:after)
  where
    innerParse :: Parser (Maybe DMNType)
    innerParse =
      tryChoice [ Just DMN_Number  <$  (choice ( symbol <$> T.words "<= < >= > ==" ) *> someNum)
                , Just DMN_Number  <$  (someNum <* choice ( symbol <$> T.words "<= < >= > ==" ))
                , Just DMN_Number  <$  (lexeme (isNum <|> isVar_name)
                                         >> lexeme (choice [ char x | x <- "+-*/" ])
                                         >> lexeme (isNum <|> isVar_name))
                  -- TODO: a full language would allow "celsius * 9/5 + 32"
                , Just DMN_Number  <$  (brackets (someNum >> symbol ".." >> someNum))
                , Just DMN_Number  <$  (someNum)
                , Just DMN_Boolean <$  (tryChoice (symbol <$> T.words "True true yes t y positive")  <* (lookAhead (char ',' >> return ()) <|> eof))
                , Just DMN_Boolean <$  (tryChoice (symbol <$> T.words "False false no f n negative")  <* (lookAhead (char ',' >> return ()) <|> eof))
                , Just . DMN_List  <$> (brackets (many anyChar) >> (fromJust <$> parseDMNType)) -- the first element is dispositive
                , Just DMN_String  <$  parseBareString
                ]
    flattenTypes :: [Maybe DMNType] -> Maybe DMNType
    flattenTypes xs
      | (Just DMN_String) `elem` xs = Just DMN_String
      | length (nub xs) == 1        = head xs
      | otherwise                   = Just DMN_String

-- additional special function-like expressions found only in output columns
parseFNumFunction :: Maybe DMNType -> Parser FNumFunction
parseFNumFunction mdt =
  tryChoice [ parseFNFf mdt, parseFNF3 mdt, parseFNF1 mdt, parseFNF0 mdt ]
-- max(21, age * 2) -- FNFf (FNF1 "max") [ FNF1 "age" FNMul (FNF0 $ VN 2.0) ]
-- age * 2  -- FNF3 (FNF1 "age") FNMul (FNF0 $ VN 2.0)
-- age      -- FNF1 "age"
-- "age"    -- FNF0 (VS "age")

parseFNFmax :: Maybe DMNType -> Parser FNFF -- recognized functions
parseFNFmax mdt = symbol "max" >> return FNFmax

parseFNFmin :: Maybe DMNType -> Parser FNFF -- recognized functions
parseFNFmin mdt = symbol "min" >> return FNFmin

parseFNFf :: Maybe DMNType -> Parser FNumFunction -- function call
parseFNFf mdt = try $ do
  fName <- choice [ parseFNFmax mdt, parseFNFmin mdt ]
  args  <- parens $ many ( parseFNumFunction mdt <* optional (symbol ",") )
  return $ FNFf fName args

parseFNF1 :: Maybe DMNType -> Parser FNumFunction -- unquoted variable which should appear in the symbol table
parseFNF1 mdt = Debug.Trace.trace ("parseFNF1: called with " ++ show mdt ++ "...") $
                case mdt of
                  (Just DMN_String)  -> (FNF1 <$> parseVar_name) <|> (feel2fnf <$> parseFEELexp' mdt)
                  (Just DMN_Number)  -> (FNF1 <$> parseVar_name) <|> (feel2fnf <$> parseFEELexp' mdt)
                  (Just DMN_Boolean) -> (FNF1 <$> parseVar_name) <|> (feel2fnf <$> parseFEELexp' mdt)
                  (Just (DMN_List _))-> error $ "list type not supported in output columns"
                  Nothing            -> feel2fnf <$> parseFEELexp' (Just DMN_String)

feel2fnf :: FEELexp -> FNumFunction
feel2fnf (FNullary x)   = FNF0 x
feel2fnf (FSection c x) = error $ "declining to construct lambda result in FNumFunction"
feel2fnf (FAnything)    = FNF0 (VB True)
feel2fnf (FInRange x y) = error $ "declining to construct range result in FNumFunction"
feel2fnf (FFunction x)  = x

parseFNF3 :: Maybe DMNType -> Parser FNumFunction -- complex function of multiple sub functions
parseFNF3 mdt = try $ do
  let complex = ( ( parens $ parseFNumFunction mdt )
                  <|> parseFNF0 mdt
                  <|> parseFNF1 mdt )
  fnfa  <- lexeme complex
  fnfop <- lexeme parseFNOp2
  fnfb  <- lexeme complex
  return $ FNF3 fnfa fnfop fnfb

parseFNF0 :: Maybe DMNType -> Parser FNumFunction -- double-quoted string literal
parseFNF0 mdt = FNF0 . VS <$> stringLiteral

parseFNOp2 :: Parser FNOp2
parseFNOp2 =
  choice [ symbol "**" >> return FNExp
         , symbol "*"  >> return FNMul
         , symbol "/"  >> return FNDiv
         , symbol "-"  >> return FNMinus
         , symbol "+"  >> return FNPlus
         ]
    
escape :: Parser String
escape = try $ do
    d <- char '\\'
    c <- oneOf ['\\', '\"', '0', 'n', 'r', 'v', 't', 'b', 'f']
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf ['\\', '\"', '\0', '\n', '\r', '\v', '\t', '\b', '\f']

