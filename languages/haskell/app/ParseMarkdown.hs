{-# LANGUAGE NoMonomorphismRestriction, MultiWayIf, OverloadedStrings, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}

module ParseMarkdown (parseMarkdown) where

import System.IO ( stderr, hPutStrLn )
import Control.Monad ( when )
import Data.List (takeWhile)
import Data.Maybe ( catMaybes, fromMaybe )

-- import Debug.Trace

import DMN.Types ( DecisionTable )
import DMN.ParseTable ( parseTable )

import Text.Megaparsec
    ( MonadParsec(try, eof), satisfy, manyTill, many, (<?>), (<|>) )
import Text.Megaparsec.Char ( char )
import DMN.ParsingUtils
    ( Parser,
      skipMany1,
      skipWhile,
      notChar,
      endOfLine,
      anyChar,
      many1,
      parseOnly,
      skipHorizontalSpace
    )
import qualified Data.Text as T

import Options ( ArgOptions(input, verbose) )
import Data.Foldable (Foldable(toList))

-- | monadic concatMap.
-- TODO: Use ListT or this thing
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = fmap concat $ mapM f xs

-- | parse input markdown file.
parseMarkdown :: ArgOptions -> IO [DecisionTable]
parseMarkdown opts1 = do
  let infiles = input opts1
  mydtchunks <- concatMapM (fileChunks opts1) (zip [1..] infiles)
  mydtables <- concatMapM (parseChunk opts1) mydtchunks
  return mydtables

  where
    fileChunks :: ArgOptions -> (Int, FilePath) -> IO InputChunks
    fileChunks opts (inum,infile) = do
      mylog opts $ "* opening file: " ++ infile
      -- NOTE: Lazy IO
      -- TODO: Extract this logic to a common function
      inlines <- if infile == "-" then getContents else readFile infile
      let rawchunksEither = parseOnly (grepMarkdown ("f"++show inum) <?> "grepMarkdown") (T.pack inlines)

      whenLeft rawchunksEither 
        (\errstr -> myerr opts $ "** parser failure in grepMarkdown: " ++ errstr)
      return $ either (const []) id rawchunksEither

    myerr :: ArgOptions -> String -> IO ()
    myerr _ = hPutStrLn stderr

    mylog :: ArgOptions -> String -> IO ()
    mylog opts msg = when (verbose opts) $ myerr opts msg

    parseChunk :: ArgOptions -> InputChunk -> IO [DecisionTable]
    parseChunk opts mychunk
     | chunkLines mychunk == ["|]"] = pure [] -- special case, sometimes |] closes a quasiquotation block
     | otherwise = do
      let parseResult = parseOnly (parseTable (chunkName mychunk) <?> "parseTable")
            $ T.pack $ unlines $ chunkLines mychunk
      let printParseError myPTfail =
            myerr opts $
              "** failed to parse table " ++ chunkName mychunk ++ "   :ERROR:\nat " ++ myPTfail

      whenLeft parseResult printParseError
      pure $ toList parseResult

whenLeft :: Monad m => Either a b -> (a -> m ()) -> m ()
whenLeft val onLeft =
      case val of
        Left e -> onLeft e
        Right _ -> return ()

-- * parsing support
-- | the markdown parser deals with InputChunks, which tracks the chunk name and lines
type InputChunks = [InputChunk]

data InputChunk = InputChunk
  { chunkName  :: String
  , chunkLines :: [String]
  }
  deriving (Show, Eq)
-- in future, consider grabbing the tables out of Pandoc -- maybe this would be better off as a JSON filter?

-- | look for relevant table sections in the markdown file
grepMarkdown :: String -> Parser InputChunks
grepMarkdown defaultName = do
  mytables <- many1 (try (grepTable defaultName) <?> "grepTable")
  (many irrelevantLine >> eof)
  return $ catMaybes mytables

-- | start to break down a table
grepTable :: String -> Parser (Maybe InputChunk)
grepTable defaultName = do
  mHeader <- maybeHeaderLines <?> "maybeHeaderLines"
  tablelines <- many1 (getTableLine <?> "getTableLine")
  return (Just (InputChunk (fromMaybe defaultName mHeader) tablelines))

-- | a table line begins with a pipe
getTableLine :: Parser String
getTableLine = do
  pipe <- char '|'
  therest <- manyTill anyChar endOfLine
  return $ pipe : therest

-- | ignore irrelevant lines in the markdown file
irrelevantLine :: Parser (Maybe String)
irrelevantLine = do
  endOfLine <|> (notChar '|' >> skipWhile "character" (/= '\n') >> endOfLine)
  return Nothing

-- | have we got multiple header lines?
maybeHeaderLines :: Parser (Maybe String)
maybeHeaderLines = do
  gotHeaders <- catMaybes <$> many maybeHeaderLine
  return (if not (null gotHeaders) then Just (last gotHeaders) else Nothing)

-- | have we got a header line?
maybeHeaderLine :: Parser (Maybe String)
maybeHeaderLine = do
  foundLine <- try orgNameLine <|> try (headerLine <?> "header line") <|> (irrelevantLine <?> "irrelevant line")
  return $ Data.List.takeWhile (/=':') <$> foundLine

-- | deal with a header line
headerLine :: Parser (Maybe String)
headerLine = do
  skipMany1 (satisfy (\c -> c == '#' || c == '*'))
  skipHorizontalSpace
  content <- manyTill anyChar endOfLine
  return (Just content)

-- | record the name of the chunk from the org file saying @#+NAME@
orgNameLine :: Parser (Maybe String)
orgNameLine = do
  "#+NAME:" >> skipHorizontalSpace
  content <- manyTill anyChar endOfLine
  return (Just content)


