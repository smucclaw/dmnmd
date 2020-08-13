{-# LANGUAGE NoMonomorphismRestriction, MultiWayIf, OverloadedStrings, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}

module ParseMarkdown (parseMarkdown) where

import System.IO
import Control.Monad
import Data.List (takeWhile)
import Data.Maybe

-- import Debug.Trace

import DMN.Types
import DMN.ParseTable
import DMN.ParseFEEL

import Text.Megaparsec hiding (label)
import Text.Megaparsec.Char
import DMN.ParsingUtils
import qualified Data.Text as T

import Options

-- concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
-- concatMapM f xs = fmap concat $ mapM f xs

parseMarkdown :: Options.ArgOptions -> IO [DecisionTable]
parseMarkdown opts1 = do
  let opts = opts1
  let infiles = if null (input opts) then ["-"] else input opts
  mydtchunks <- mapM (fileChunks opts) (zip [1..] infiles)
  mydtables1 <- mapM
    (\mychunk ->
        either
        (\myPTfail -> do myerr opts $ "** failed to parse table " ++ chunkName mychunk ++ "   :ERROR:\nat " ++ myPTfail
                         return Nothing)
        (return . Just)
        (parseOnly (parseTable (chunkName mychunk) <?> "parseTable") $ T.pack $ unlines $ chunkLines mychunk)
    ) (concat mydtchunks)
  let mydtables = catMaybes mydtables1 -- all this really wants to be an ArrowList

  return mydtables

  where
    fileChunks :: Options.ArgOptions -> (Int, FilePath) -> IO InputChunks
    fileChunks opts (inum,infile) = do
      mylog opts $ "* opening file: " ++ infile
      inlines <- if infile == "-" then getContents else readFile infile
      let rawchunksEither = parseOnly (grepMarkdown ("f"++show inum) <?> "grepMarkdown") (T.pack inlines)
      either
        (\errstr -> myerr opts ("** parser failure in grepMarkdown: " ++ errstr) >> return [])
        return
        rawchunksEither
    myerr _ = hPutStrLn stderr
    mylog opts msg = when (verbose opts) $ myerr opts msg


--  putStrLn $ toJS (fromRight (error "parse error") (parseOnly (parseTable "mydmn1") dmn2))

type InputChunks = [InputChunk]

data InputChunk = InputChunk
  { chunkName  :: String
  , chunkLines :: [String]
  }
  deriving (Show, Eq)
-- in future, consider grabbing the tables out of Pandoc -- maybe this would be better off as a JSON filter?

grepMarkdown :: String -> Parser InputChunks
grepMarkdown defaultName = do
  mytables <- many1 (try (grepTable defaultName) <?> "grepTable")
  (many irrelevantLine >> eof)
  return $ catMaybes mytables

grepTable :: String -> Parser (Maybe InputChunk)
grepTable defaultName = do
  mHeader <- maybeHeaderLines <?> "maybeHeaderLines"
  tablelines <- many1 (getTableLine <?> "getTableLine")
  return (Just (InputChunk (fromMaybe defaultName mHeader) tablelines))

getTableLine :: Parser String
getTableLine = do
  pipe <- char '|'
  therest <- manyTill anyChar endOfLine
  return $ pipe : therest

irrelevantLine :: Parser (Maybe String)
irrelevantLine = do
  endOfLine <|> (notChar '|' >> skipWhile "character" (/= '\n') >> endOfLine)
  return Nothing

maybeHeaderLines :: Parser (Maybe String)
maybeHeaderLines = do
  gotHeaders <- catMaybes <$> many maybeHeaderLine
  return (if not (null gotHeaders) then Just (last gotHeaders) else Nothing)

maybeHeaderLine :: Parser (Maybe String)
maybeHeaderLine = do
  foundLine <- try orgNameLine <|> try (headerLine <?> "header line") <|> (irrelevantLine <?> "irrelevant line")
  return $ Data.List.takeWhile (/=':') <$> foundLine

headerLine :: Parser (Maybe String)
headerLine = do
  skipMany1 (satisfy (\c -> c == '#' || c == '*'))
  skipHorizontalSpace
  content <- manyTill anyChar endOfLine
  return (Just content)

orgNameLine :: Parser (Maybe String)
orgNameLine = do
  "#+NAME:" >> skipHorizontalSpace
  content <- manyTill anyChar endOfLine
  return (Just content)


