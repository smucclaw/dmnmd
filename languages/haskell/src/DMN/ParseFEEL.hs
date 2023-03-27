{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module DMN.ParseFEEL where

import Prelude hiding (takeWhile)
import Control.Applicative hiding (many, some) 
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Data.Char (isAlphaNum)
import qualified Data.Text as T
import DMN.Types
import DMN.ParsingUtils
import Debug.Trace

-- parser for low-level expressions, e.g. FEEL, common to all DMN syntaxes.

-- let's allow spaces in variable names. what could possibly go wrong?
parseVarname :: Parser Text
parseVarname = do
  -- input <- T.take 40 <$> lookAhead takeRest; traceM $ "parseVarname: input: " ++ T.unpack input
  firstLetter <- letterChar
  remainder <- takeWhileP (Just "legal identifier character, including spaces")
               (\c -> isAlphaNum c    || c == '\t'
                      || c == ' '     || c == '\x00A0' -- nonbreaking space
                      || c == '_'     || c == '_'      )

  -- we should now have run up against either eol, |, (, or :
  return $ T.strip $ T.cons firstLetter remainder


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
skipHorizontalSpace = skipWhile "Horizontal space" isHorizontalSpace
-- ^ Maybe add try here?

