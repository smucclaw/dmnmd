{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module DMN.ParseFEEL where

import Prelude hiding (takeWhile)
import Control.Applicative hiding (many, some) 
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import qualified Data.Text as T
import DMN.Types
import DMN.ParsingUtils


-- parser for low-level expressions, e.g. FEEL, common to all DMN syntaxes.

-- let's allow spaces in variable names. what could possibly go wrong?
parseVarname :: Parser Text
parseVarname = do
  firstLetter <- letterChar
  -- remainder <- takeWhileP Nothing (\c -> c /= ':' && c /= '|' && c /= '(' ) -- inClass "a-zA-Z0-9_"
  remainder <- takeWhileP Nothing (inClass "a-zA-Z0-9_ ")
  -- remainder <- takeWhileP Nothing (inClass $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_ ")
  return $ T.strip $ T.append (T.singleton firstLetter) remainder


parseFNumFunction :: Parser FNumFunction
parseFNumFunction =
  choice $ fmap try [ parseFNF3, parseFNF0, parseFNF1 ]
-- age * 2  -- FNF3 (FNF1 "age") FNMul (FNF0 $ VN 2.0)
-- age      -- FNF1 "age"
-- "age"    -- FNF0 (VS "age")

parseFNF1 :: Parser FNumFunction -- variable which should appear in the symbol table
parseFNF1 = FNF1 . T.unpack <$> parseVarname

parseFNF3 :: Parser FNumFunction -- complex function of multiple sub functions
parseFNF3 = do
  let complex = ( ( "(" *> skipHorizontalSpace *> parseFNumFunction <* skipHorizontalSpace <* ")" )
                  <|> parseFNF0
                  <|> parseFNF1 )
  fnfa  <- complex
  skipHorizontalSpace
  fnfop <- parseFNOp2
  skipHorizontalSpace
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

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipWhile "Horizontal space" isHorizontalSpace <?> "Horizontal whitespace"
-- ^ Maybe add try here?

