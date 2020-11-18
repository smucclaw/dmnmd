{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module DMN.ParseFEEL where

import Prelude hiding (takeWhile)
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
parseVarname = do
  firstLetter <- letterChar
  -- remainder <- takeWhileP Nothing (\c -> c /= ':' && c /= '|' && c /= '(' ) -- inClass "a-zA-Z0-9_"
  remainder <- takeWhileP Nothing (inClass "a-zA-Z0-9_ ")
  -- remainder <- takeWhileP Nothing (inClass $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_ ")
  return $ T.strip $ T.append (T.singleton firstLetter) remainder



parseDataCell :: Maybe DMNType -> Parser [FEELexp]
parseDataCell dmntype = do
  fexps <- many ( parseFEELexp dmntype <* symbol "," )
  return fexps

parseFEELexp :: Maybe DMNType -> Parser FEELexp
parseFEELexp mdt = try $ optional (char '_' <|> char '-') >> endOfLine >> return FAnything
                   <|> parseFEELexp' mdt

parseFEELexp' :: Maybe DMNType -> Parser FEELexp
parseFEELexp' (Just (DMN_List x)) = parseFEELexp' (Just x)
parseFEELexp' Nothing = do
  inner <- (hspace *> manyTill anyChar eof)
  return $ FNullary (VS inner)
-- strings are tricky because they could be FEEL expression variable names like "Dish Name"
-- or just literal strings like "Lentil Soup"
parseFEELexp' (Just DMN_String)  = FNullary . VS   <$> (hspace *> many anyChar <* hspace <* eof)
parseFEELexp' (Just DMN_Boolean) = do
  FNullary (VB True)  <$ (choice (string <$> T.words "true yes t y positive") <* eof)
  <|>
  FNullary (VB False) <$ (choice (string <$> T.words "false no f n negative") <* eof)
parseFEELexp' (Just DMN_Number) = do
  FFunction <$> parseFNumFunction
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

parseFNumFunction :: Parser FNumFunction
parseFNumFunction =
  choice $ fmap try [ parseFNFf, parseFNF3, parseFNF0, parseFNF1 ]
-- max(21, age * 2) -- FNFf (FNF1 "max") [ FNF1 "age" FNMul (FNF0 $ VN 2.0) ]
-- age * 2  -- FNF3 (FNF1 "age") FNMul (FNF0 $ VN 2.0)
-- age      -- FNF1 "age"
-- "age"    -- FNF0 (VS "age")

parseFNFmax :: Parser FNFF -- recognized functions
parseFNFmax = string "max" >> return FNFmax

parseFNFmin :: Parser FNFF -- recognized functions
parseFNFmin = string "min" >> return FNFmin

parseFNFf :: Parser FNumFunction -- function call
parseFNFf = do
  fName <- hspace *> (parseFNFmax <|> parseFNFmin) <* hspace
  args  <- parens $ many ( hspace *> parseFNumFunction <* ( hspace >> optional (char ',' >> hspace ) ) )
  return $ FNFf fName args

parseFNF1 :: Parser FNumFunction -- variable which should appear in the symbol table
parseFNF1 = FNF1 . T.unpack <$> parseVarname

parseFNF3 :: Parser FNumFunction -- complex function of multiple sub functions
parseFNF3 = do
  let complex = ( ( "(" *> hspace *> parseFNumFunction <* hspace <* ")" )
                  <|> parseFNF0
                  <|> parseFNF1 )
  fnfa  <- complex
  hspace
  fnfop <- parseFNOp2
  hspace
  fnfb  <- complex
  return $ FNF3 fnfa fnfop fnfb

-- maybe we should punt to DecisionTable's mkF to handle double-quoted strings.
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

