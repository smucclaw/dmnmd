{-# LANGUAGE OverloadedStrings #-}

module DMN.ParsingUtils
  where

import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec.Char ( char, string )
import Text.Megaparsec
    ( (<|>),
      (<?>),
      anySingle,
      anySingleBut,
      parse,
      satisfy,
      errorBundlePretty,
      skipMany,
      some,
      Parsec,
      MonadParsec(eof, takeWhileP) )

import GHC.Stack (HasCallStack)

type Parser = Parsec Void Text


inClass :: [Char] -> Char -> Bool
inClass cs = (`elem` cs)

notInClass :: [Char] -> Char -> Bool
notInClass s = not . inClass s
  
skipWhile :: String -> (Char -> Bool) -> Parser ()
skipWhile tokenLabel p = () <$ takeWhileP (Just tokenLabel) p

lexeme :: Parser a -> Parser a
lexeme x = x <* skipHorizontalSpace

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

-- @double :: Parser Double@ used to live here, defined as
-- @realToFrac \<$\> scientific@, with a note above it saying the conversion was
-- the wrong one. Its sole caller ('DMN.ParseFEEL.parseFNF0') then applied a
-- /second/ @realToFrac@ to reach 'Float', so an arithmetic literal that
-- megaparsec had already handed back exactly went Scientific -> Double -> Float
-- before anyone looked at it.
--
-- With @VN@ now a 'Scientific' (@DECISIONS.md@ D-1) the fix is a deletion, not
-- an addition: 'DMN.ParseFEEL' calls 'Text.Megaparsec.Char.Lexer.scientific'
-- directly. Keeping @double@ would have been actively dangerous — a single
-- @realToFrac \@Double \@Scientific@ left behind is /exact/ and therefore
-- catastrophic, turning @Age * 0.1@ into a 55-digit TypeScript literal, and
-- nothing in the test suite or the corpus would have caught it because every
-- arithmetic literal in the tree happens to be dyadic.

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

-- [TODO] why don't we just replace these remnants of the Atto -> Mega transition, with things from Text.Parser.Combinators

-- attoparsec calls 'some' 'many1'
many1 :: Parser a -> Parser [a]
many1 = some

skipMany1 :: Parser a -> Parser () 
skipMany1 p = p *> skipMany p

anyChar :: Parser Char
anyChar = anySingle

notChar :: Char -> Parser Char
notChar = anySingleBut

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipWhile "Horizontal space" isHorizontalSpace
-- ^ Maybe add try here?

