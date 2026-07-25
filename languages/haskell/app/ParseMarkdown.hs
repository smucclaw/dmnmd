{-# LANGUAGE NoMonomorphismRestriction, MultiWayIf, OverloadedStrings, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}

module ParseMarkdown (parseMarkdown) where

import System.IO ( stderr, hPutStrLn )
import Control.Monad ( when )
import Data.List (takeWhile)
import Data.Maybe ( catMaybes, fromMaybe )

-- import Debug.Trace

import Data.Either (isRight)

import DMN.Types ( DecisionTable )
import DMN.ParseTable ( parseTable, parseHitPolicy, pipeSeparator )

import Text.Megaparsec
    ( MonadParsec(try, eof), takeRest, satisfy, manyTill, many, (<?>), (<|>) )
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

-- | parse input markdown file.
--
-- Returns the parse errors alongside the tables. They used to go to stderr and
-- nowhere else, so a file that failed to parse was indistinguishable from a file
-- with nothing in it, and @dmnmd@ exited 0 either way. Printing them is now the
-- caller's job — this function printing them too is what made every failure
-- appear twice.
--
-- A returned error means a decision table did not parse. It does not mean "no
-- decision tables here": a prose document, with or without pipe tables in it,
-- is a perfectly good input that happens to contain nothing to transpile, and
-- comes back as @([], [])@.
parseMarkdown :: ArgOptions -> IO ([String], [DecisionTable])
parseMarkdown opts1 = do
  let infiles = input opts1
  chunkResults <- mapM (fileChunks opts1) (zip [1..] infiles)
  let (chunkErrs, mydtchunks) = (concatMap fst chunkResults, concatMap snd chunkResults)
  tableResults <- mapM (parseChunk opts1) mydtchunks
  let (tableErrs, mydtables) = (concatMap fst tableResults, concatMap snd tableResults)
  return (chunkErrs ++ tableErrs, mydtables)

  where
    fileChunks :: ArgOptions -> (Int, FilePath) -> IO ([String], [(FilePath, InputChunk)])
    fileChunks opts (inum,infile) = do
      mylog opts $ "* opening file: " ++ infile
      -- NOTE: Lazy IO
      -- TODO: Extract this logic to a common function
      inlines <- if infile == "-" then getContents else readFile infile
      let rawchunksEither = parseOnly (grepMarkdown ("f"++show inum) <?> "grepMarkdown") (T.pack inlines)

      case rawchunksEither of
        Left errstr ->
          pure ([infile ++ ": parser failure in grepMarkdown: " ++ errstr], [])
        Right chunks -> pure ([], [(infile, c) | c <- chunks])

    myerr :: ArgOptions -> String -> IO ()
    myerr _ = hPutStrLn stderr

    mylog :: ArgOptions -> String -> IO ()
    mylog opts msg = when (verbose opts) $ myerr opts msg

    parseChunk :: ArgOptions -> (FilePath, InputChunk) -> IO ([String], [DecisionTable])
    parseChunk opts (infile, mychunk)
     | chunkLines mychunk == ["|]"] = pure ([], []) -- special case, sometimes |] closes a quasiquotation block
     -- A pipe table whose top-left cell is not a hit policy is not a decision
     -- table; markdown files are full of ordinary prose tables. Skipping one is
     -- not an error — but it is not silent either, or a typo'd hit policy would
     -- make a real table vanish without a word.
     | not (isDecisionTable mychunk) = do
         myerr opts $ "note: " ++ infile ++ ": skipping the pipe table under "
           ++ show (chunkName mychunk)
           ++ ": its top-left cell is not a DMN hit policy (one of U A P F O R C),"
           ++ " so this is prose rather than a decision table."
         pure ([], [])
     | otherwise = do
      let parseResult = parseOnly (parseTable (chunkName mychunk) <?> "parseTable")
            $ T.pack $ unlines $ chunkLines mychunk
      case parseResult of
        Left myPTfail ->
          pure ([infile ++ ": failed to parse table " ++ chunkName mychunk ++ " at " ++ myPTfail], [])
        Right t -> pure ([], [t])

-- | Does this chunk claim to be a decision table?
--
-- Asked of the real parser, not of a lookalike: the top-left cell of a dmnmd
-- decision table is its hit policy, so we run 'parseHitPolicy' at exactly the
-- position 'parseHeaderRow' would. Nothing here inspects characters by hand.
-- | Is this pipe-table chunk a decision table, or prose that happens to be tabular?
--
-- The test is the FIRST TWO TOKENS OF 'parseHeaderRow' ITSELF — @pipeSeparator@,
-- @parseHitPolicy@, @pipeSeparator@ — so a chunk is classified by the real grammar
-- rather than by a proxy for it. The closing 'pipeSeparator' is the load-bearing
-- part: 'parseHitPolicy' consumes a SINGLE character, so without it the test
-- accepts any first cell that merely STARTS with a hit-policy letter, and
-- @| file | role | provenance |@ (a documentation table in test/golden/README.md)
-- is read as an @F@ table on the \"f\" of \"file\".
isDecisionTable :: InputChunk -> Bool
isDecisionTable mychunk = case chunkLines mychunk of
  (firstline : _) ->
    isRight $ parseOnly (pipeSeparator *> parseHitPolicy <* pipeSeparator <* takeRest)
                        (T.pack firstline)
  [] -> False

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
--
-- @many@, not @many1@: a markdown file with no pipe tables in it at all is a
-- well-formed document that happens to contain nothing for us, not a parse
-- failure. Requiring at least one table is what made every prose file in this
-- repo exit 1.
grepMarkdown :: String -> Parser InputChunks
grepMarkdown defaultName = do
  mytables <- many (try (grepTable defaultName) <?> "grepTable")
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
  return $ cleanTableName <$> foundLine

-- | Reduce a markdown heading to a clean table name. Headings in the wild look
-- like @`Categorize` — hit policy `F`@; the table name is the first backticked
-- token (@Categorize@). Falls back to the text before the first @:@ (org-style
-- @#+NAME:@ payloads and plain headings) when there are no backticks.
cleanTableName :: String -> String
cleanTableName s =
  case betweenBackticks s of
    Just name -> name
    Nothing   -> trimSpaces (Data.List.takeWhile (/= ':') s)
  where
    betweenBackticks str = case dropWhile (/= '`') str of
      ('`':rest) -> case span (/= '`') rest of
                      (name, '`':_) | not (null (trimSpaces name)) -> Just (trimSpaces name)
                      _                                            -> Nothing
      _ -> Nothing
    trimSpaces = f . f where f = reverse . dropWhile (`elem` (" \t" :: String))

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


