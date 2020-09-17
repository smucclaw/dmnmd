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
import Data.Foldable (Foldable(toList))

-- TODO: Use ListT or this thing
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = fmap concat $ mapM f xs

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
    parseChunk opts mychunk = do
      let parseResult = parseOnly (parseTable (chunkName mychunk) <?> "parseTable")
            $ T.pack $ unlines $ chunkLines mychunk
      let printParseError myPTfail =
            myerr opts $
              "** failed to parse table " ++ chunkName mychunk ++ "   :ERROR:\nat " ++ myPTfail

      whenLeft parseResult printParseError
      pure $ toList parseResult

-- -- | Convert an Either to a Maybe and run an action if it was left.
-- consumeLeft :: Monad m => Either a b -> (a -> m ()) -> m (Maybe b)
-- consumeLeft val onLeft = do
--   whenLeft val onLeft
--   pure $ eitherToMaybe val

whenLeft :: Monad m => Either a b -> (a -> m ()) -> m ()
whenLeft val onLeft =
      case val of
        Left e -> onLeft e
        Right _ -> return ()

-- eitherToMaybe :: Either a b -> Maybe b
-- eitherToMaybe = either (const Nothing) Just
-- -- eitherToMaybe val =
-- --       case val of
-- --         Left _ -> Nothing
-- --         Right x -> Just x

{-
parseChunk :: ArgOptions -> InputChunk -> IO (Maybe DecisionTable)
parseChunk opts mychunk = do
  let parseResult = parseOnly (parseTable (chunkName mychunk) <?> "parseTable")
        $ T.pack $ unlines $ chunkLines mychunk

  whenLeft parseResult $ \myPTfail ->
        myerr opts $
          "** failed to parse table " ++ chunkName mychunk ++ "   :ERROR:\nat " ++ myPTfail

  pure $ eitherToMaybe parseResult

parseChunk :: ArgOptions -> InputChunk -> IO (Maybe DecisionTable)
parseChunk opts mychunk = do
  let parseResult = parseOnly (parseTable (chunkName mychunk) <?> "parseTable")
        $ T.pack $ unlines $ chunkLines mychunk

  case parseResult of
    Left myPTfail -> do
        myerr opts $
          "** failed to parse table " ++ chunkName mychunk ++ "   :ERROR:\nat " ++ myPTfail
        return Nothing
    Right x -> return . Just $ x

mapLeft onLeft val =
      case val of
        Left e -> do
            onLeft e
            return Nothing
        Right x -> return . Just $ x

parseChunk :: ArgOptions -> InputChunk -> IO (Maybe DecisionTable)
parseChunk opts mychunk = do
  let parseResult = (parseOnly (parseTable (chunkName mychunk) <?> "parseTable") 
        $ T.pack $ unlines $ chunkLines mychunk)

  either
    (\myPTfail -> do
        myerr opts $ 
          "** failed to parse table " ++ chunkName mychunk ++ "   :ERROR:\nat " ++ myPTfail
        return Nothing)
    (return . Just)
    parseResult
-}

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


