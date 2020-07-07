{-# LANGUAGE OverloadedStrings #-}
{- | A compatability module for moving from Attoparsec to Megaparsec

-}
module DMN.ParsingUtils
  where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Data.Attoparsec.Text as Atto
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import GHC.Stack (HasCallStack)

type Parser = Parsec Void Text


inClass :: [Char] -> Char -> Bool
inClass = Atto.inClass
-- inClass cs = (`elem` cs)

notInClass :: [Char] -> Char -> Bool
notInClass s = not . inClass s
  
skipWhile :: String -> (Char -> Bool) -> Parser ()
skipWhile tokenLabel p = () <$ takeWhileP (Just tokenLabel) p

-- | The parser @skip p@ succeeds for any character for which the
-- predicate @p@ returns 'True'.
--
-- >skipDigit = skip isDigit
-- >    where isDigit c = c >= '0' && c <= '9'
skip :: (Char -> Bool) -> Parser ()
skip p = () <$ satisfy p
-- skip :: String -> (Char -> Bool) -> Parser ()
-- skip name p = () <$ satisfy p <?> name

-- | A predicate that matches either a space @\' \'@ or horizontal tab
-- @\'\\t\'@ character.
isHorizontalSpace :: Char -> Bool
isHorizontalSpace c = c == ' ' || c == '\t'
{-# INLINE isHorizontalSpace #-}

-- Note: According to the docs of Scientific, we should use Data.Scientifc.toRealFloat instead of realToFrac
double :: Parser Double
double = realToFrac <$> scientific
-- double = read <$> many1 digit

-- | Parse a single digit, as recognised by 'isDigit'.
digit :: Parser Char
digit = satisfy isDigit <?> "digit"
{-# INLINE digit #-}

parseOnly :: HasCallStack => Parser a -> Text -> Either String a
parseOnly p = first errorBundlePretty . parse (p <* eof) ""

-- | Match either a single newline character @\'\\n\'@, or a carriage
-- return followed by a newline character @\"\\r\\n\"@.
endOfLine :: Parser ()
endOfLine = (() <$ char '\n') <|> (() <$ string "\r\n") <?> "End of line"

endOfInput :: Parser ()
endOfInput = eof

-- attoparsec calls 'some' 'many1'
many1 :: Parser a -> Parser [a]
many1 = some

skipMany1 :: Parser a -> Parser () 
skipMany1 p = p *> skipMany p

anyChar :: Parser Char
anyChar = anySingle

notChar :: Char -> Parser Char
notChar = anySingleBut