{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module DMN.ParseFEEL where

import Prelude hiding (takeWhile)
import Control.Monad (guard)
import Data.Maybe (fromJust, isJust)
import Control.Applicative hiding (many, some) 
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T
import DMN.Types
import DMN.ParsingUtils
import qualified Data.Void
import qualified Data.Functor.Identity

-- see Text.Megaparsec.Char.Lexer documentation
symbol :: Tokens Text -> ParsecT Data.Void.Void Text Data.Functor.Identity.Identity (Tokens Text)
symbol    = L.symbol hspace

parens :: ParsecT Data.Void.Void Text Data.Functor.Identity.Identity a -> ParsecT Data.Void.Void Text Data.Functor.Identity.Identity a
parens    = between (symbol "(") (symbol ")")

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



parseDataCell :: Maybe DMNType -> Parser [FEELexp]
parseDataCell dmntype = try $ do
  fexp  <- optional $ parseFEELexp dmntype
  guard $ isJust fexp
  fexps <- many $ symbol "," *> parseFEELexp dmntype
  _ <- eof
  return $ fromJust fexp : fexps

mkF :: Maybe DMNType -> String -> FEELexp
mkF maybetype myinput =
  either (error . show) id (runParser (parseFEELexp maybetype) "(2nd parse mkF)" (T.pack myinput))

parseFEELexp :: Maybe DMNType -> Parser FEELexp
parseFEELexp mdt = try ((char '_' <|> char '-') >> eof >> return FAnything)
                   <|> parseFEELexp' mdt

parseFEELexp' :: Maybe DMNType -> Parser FEELexp
parseFEELexp' (Just (DMN_List x)) = parseFEELexp' (Just x)
parseFEELexp' Nothing = do
  inner <- (hspace *> manyTill anyChar eof)
  return $ FNullary (VS inner)
-- strings are tricky because they could be FEEL expression variable names like "Dish Name"
-- or just literal strings like "Lentil Soup"

-- let's have a rule that a string expression can be bare, but no commas -- if there are commas, the string has to be quoted.
parseFEELexp' (Just DMN_String)  = FNullary . VS   <$> (nocommas <|> stringLiteral)
  where
    stringLiteral :: Parser String
    stringLiteral = char '"' >> manyTill L.charLiteral (char '"')
    nocommas :: Parser String
    nocommas = many $ satisfy (/= ',')


parseFEELexp' (Just DMN_Boolean) = do
  FNullary (VB True)  <$ (choice (string <$> T.words "True true yes t y positive"))
  <|>
  FNullary (VB False) <$ (choice (string <$> T.words "False false no f n negative"))
parseFEELexp' (Just DMN_Number) = do
  FFunction <$> parseFNumFunction -- only number columns get function types for now
  <|> FSection Flte . VN <$> (symbol "<=" *> lexeme L.float)
  <|> FSection Flt  . VN <$> (symbol "<"  *> lexeme L.float)
  <|> FSection Fgte . VN <$> (symbol ">=" *> lexeme L.float)
  <|> FSection Fgt  . VN <$> (symbol ">"  *> lexeme L.float)
  <|> FSection Fgt  . VN <$> (lexeme L.float <* symbol "<=")
  <|> FSection Fgte . VN <$> (lexeme L.float <* symbol "<" )
  <|> FSection Flt  . VN <$> (lexeme L.float <* symbol ">=")
  <|> FSection Flte . VN <$> (lexeme L.float <* symbol ">" )
  <|> FSection Feq  . VN <$> (lexeme L.float <* symbol "=" )
  <|> FSection Feq  . VN <$> (symbol "=" *> lexeme L.float )
  <|> do
    lower <- symbol "[" *> lexeme L.decimal
    upper <- symbol ".." *> lexeme L.decimal <* symbol "]"
    return $ FInRange lower upper
  <|> FNullary      . VN <$> (lexeme L.float)

-- TODO
-- there seems to be some code duplication here, between parseFEELexp and the parseFNF* below.
-- figure out the control flow involved in the type inferencing, and see how the second pass deals with strings containing ", " -- we don't seem to re-process the "," giving it an opportunity to split.

parseFNumFunction :: Parser FNumFunction
parseFNumFunction =
  choice [ parseFNFf, parseFNF3, parseFNF0, parseFNF1 ]
-- max(21, age * 2) -- FNFf (FNF1 "max") [ FNF1 "age" FNMul (FNF0 $ VN 2.0) ]
-- age * 2  -- FNF3 (FNF1 "age") FNMul (FNF0 $ VN 2.0)
-- age      -- FNF1 "age"
-- "age"    -- FNF0 (VS "age")

parseFNFmax :: Parser FNFF -- recognized functions
parseFNFmax = string "max" >> return FNFmax

parseFNFmin :: Parser FNFF -- recognized functions
parseFNFmin = string "min" >> return FNFmin

parseFNFf :: Parser FNumFunction -- function call
parseFNFf = try $ do
  fName <- hspace *> (parseFNFmax <|> parseFNFmin) <* hspace
  args  <- parens $ many ( hspace *> parseFNumFunction <* ( hspace >> optional (char ',' >> hspace ) ) )
  return $ FNFf fName args

parseFNF1 :: Parser FNumFunction -- variable which should appear in the symbol table
parseFNF1 = FNF1 . T.unpack <$> parseVarname

parseFNF3 :: Parser FNumFunction -- complex function of multiple sub functions
parseFNF3 = try $ do
  let complex = ( ( "(" *> hspace *> parseFNumFunction <* hspace <* ")" )
                  <|> parseFNF0
                  <|> parseFNF1 )
  fnfa  <- complex
  hspace
  fnfop <- parseFNOp2
  hspace
  fnfb  <- complex
  return $ FNF3 fnfa fnfop fnfb

parseFNF0 :: Parser FNumFunction -- double-quoted string literal
parseFNF0 =
  let inner = fmap return (try nonEscape) <|> escape
  in ( do _ <- char '"'
          strings <- many inner
          _ <- char '"'
          return $ FNF0 $ VS $ concat strings )
     <|> (FNF0 . VN . realToFrac <$> double)
     <|> (("yes" <|> "true"  <|> "True"  <|> "t" <|> "y") >> return ( FNF0 $ VB True))
     <|> (("no"  <|> "false" <|> "False" <|> "f" <|> "n") >> return ( FNF0 $ VB False))
      
      
parseFNOp2 :: Parser FNOp2
parseFNOp2 =
  choice [ "**" >> return FNExp
         , "*"  >> return FNMul
         , "/"  >> return FNDiv
         , "-"  >> return FNMinus
         , "+"  >> return FNPlus
         ]
    
escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf ['\\', '\"', '0', 'n', 'r', 'v', 't', 'b', 'f']
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf ['\\', '\"', '\0', '\n', '\r', '\v', '\t', '\b', '\f']

