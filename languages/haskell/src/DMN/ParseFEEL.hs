{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module DMN.ParseFEEL where

import Prelude hiding (takeWhile)
import Control.Applicative hiding (many, some) 
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Data.Char (isAlphaNum)
import Data.Scientific (Scientific)
import qualified Data.Text as T
import DMN.Types
import DMN.ParsingUtils
import DMN.Number (spellable, maxBase10Exponent)
import Text.Megaparsec.Char.Lexer (scientific)
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
     -- Numeric literal inside an arithmetic cell (@Age * 0.1@). This is
     -- megaparsec's own 'Text.Megaparsec.Char.Lexer.scientific', taken
     -- directly: it is what 'DMN.ParsingUtils.double' already wrapped, so the
     -- accepted language is unchanged (exponents included), and the two
     -- @realToFrac@s that stood between it and 'VN' are gone.
     --
     -- Note the deliberate asymmetry with 'DMN.ParseCell.numericLiteral', which
     -- is hand-rolled to DMN 1.3 §9.2 rule 31 and refuses @1e5@. A cell is
     -- S-FEEL; the inside of an arithmetic expression is not. Do not unify them.
     --
     -- 'spellable' is the magnitude guard 'Float' used to provide by rounding to
     -- @Infinity@: @1e1000000@ parses fine and every route back out of it is
     -- proportional to the spelled value.
     <|> (FNF0 . VN <$> guardedScientific)
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


-- | 'Text.Megaparsec.Char.Lexer.scientific', refusing a value dmnmd cannot
-- spell back out. See 'DMN.Number.maxBase10Exponent'.
guardedScientific :: Parser Scientific
guardedScientific = do
  n <- scientific
  if spellable n
    then pure n
    else fail ("number is too large to represent: its decimal exponent exceeds "
               ++ show maxBase10Exponent)
