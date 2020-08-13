{-# LANGUAGE NoMonomorphismRestriction, MultiWayIf, OverloadedStrings, DuplicateRecordFields #-}

module Main
where

import System.IO
import System.FilePath
import Control.Monad
import Data.List.Split (splitOn)
import Data.List (takeWhile, isSuffixOf, intercalate, nub)
import Data.Maybe

import System.Console.Haskeline
-- import Debug.Trace

import DMN.Types
import DMN.ParseTable
import DMN.DecisionTable
import DMN.Translate.JS
import DMN.ParseFEEL

import Text.Megaparsec hiding (label)
import Text.Megaparsec.Char
import DMN.ParsingUtils
import qualified Data.Text as T
import qualified Options.Applicative as OA
import Options.Applicative (long, short, help, helper, fullDesc, progDesc, strOption, switch, value, info, metavar, str, argument)
import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdOutput)

-- let's do getopt properly

data ArgOptions = ArgOptions
  { verbose  :: Bool
  , query    :: Bool
  , propstyle :: Bool
  , informat  :: FileFormat
  , outformat :: FileFormat
  , out      :: String
  , pick     :: String
  , input    :: [String]
  }
  deriving (Show, Eq)

argOptions :: OA.Parser ArgOptions
argOptions = ArgOptions
  <$> switch    (long "verbose"    <> short 'v'                                          <> help "more verbosity" )
  <*> switch    (long "query"      <> short 'q'                                          <> help "evaluate interactively" )
  <*> switch    (long "props"      <> short 'r'                                          <> help "JS functions use props style" )
  <*> strOption (long "from"       <> short 'f' <> metavar "InputFormat"  <> value ""    <> help "input format" )
  <*> strOption (long "to"         <> short 't' <> metavar "OutputFormat" <> value ""    <> help "output format" )
  <*> strOption (long "out"        <> short 'o' <> metavar "FILE"         <> value "-"   <> help "output file" )
  <*> strOption (long "pick"       <> short 'p' <> metavar "TABLE,..."    <> value ""    <> help "name of desired decision table" )
  <*> OA.many ( argument str (metavar "FILES..."))

type FileFormat = String

fileExtensionMappings :: [(String, FileFormat)]
fileExtensionMappings =
  [ ("ts", "ts")
  , ("js", "js")
  , ("dmn", "xml")
  , ("md", "md")
  ]

extensionToFileFormat :: String -> Maybe FileFormat
extensionToFileFormat ext = lookup ext fileExtensionMappings

detectFormat :: [FilePath] -> FileFormat -> FileFormat
detectFormat files "" = fromMaybe "" $ detectFormat' files
detectFormat _ origFormat = origFormat

detectFormat' :: [FilePath] -> Maybe FileFormat
detectFormat' files = do
  let exts = map takeExtension files
  ext <- getSame exts
  extensionToFileFormat ext

-- | Checks if all elements in a list are equal, and if so, returns that element.
getSame :: Eq a => [a] -> Maybe a
getSame (x : xs) | all (== x) xs = Just x
getSame _ = Nothing

detectOutformat :: ArgOptions -> ArgOptions
detectOutformat opts = opts { outformat = detectFormat [out opts] (outformat opts)}

detectInformat :: ArgOptions -> ArgOptions
detectInformat opts = opts { informat = detectFormat (input opts) (informat opts)}

main :: IO ()
main = do
  opts1 <- OA.execParser $ info (argOptions OA.<**> helper) (fullDesc <> progDesc "DMN CLI interpreter and converter" <> OA.header "dmnmd")
  let opts = detectOutformat . detectInformat $ opts1
  myouthandle <- myOutHandle $ out opts
  let infiles = if null (input opts) then ["-"] else input opts
  mydtchunks <- mapM (fileChunks opts) (zip [1..] infiles)
  mydtables1 <- mapM (\mychunk ->
                           either
                           (\myPTfail -> do myerr opts $ "** failed to parse table " ++ chunkName mychunk ++ "   :ERROR:\nat " ++ myPTfail
                                            return Nothing)
                           (return . Just)
                           (parseOnly (parseTable (chunkName mychunk) <?> "parseTable") $ T.pack $ unlines $ chunkLines mychunk)
                       ) (concat mydtchunks)
  let mydtables = catMaybes mydtables1 -- all this really wants to be an ArrowList
  mylog opts $ "* imported " ++ show (length mydtables) ++ " tables."
  mylog opts $ "pick = " ++ pick opts
  let pickedTables = if not (null (pick opts)) then filter (\dt -> tableName dt `elem` (trim <$> splitOn "," (pick opts))) mydtables else mydtables
  mylog opts $ "* picked " ++ show (length pickedTables) ++ " tables from " ++ show (trim <$> splitOn "," (pick opts))
  when (null pickedTables) $ mylog opts $ "available tablenames were " ++ show (tableName <$> mydtables)
  mylog opts "shall we output them or go interactive?"

    -- are we talking to console or receiving input from STDIN?
    -- is the input coming in JSON format?
    -- which tables shall we run eval against? maybe the user gave a --pick. Maybe they didn't. if they didn't, run against all tables.
    -- if the tables have different input types, die. because our plan is to run the same input against all the different tables.

  istty <- queryTerminal stdOutput
  if | not $ query opts              -> mapM_ (outputTo myouthandle (outformat opts) opts) pickedTables
     | differentlyTyped pickedTables -> fail $ "tables " ++ show (tableName <$> pickedTables) ++ " have different types; can't query. use --pick to choose one"
     | query opts && istty           -> runInputT defaultSettings (loop opts pickedTables)
     | query opts && not istty       -> mylog opts "expecting eval term input on STDIN."
  hClose myouthandle

  where
    loop :: ArgOptions -> [DecisionTable] -> InputT IO ()
    loop opts dtables = do
      minput <- getInputLine (intercalate ", " (tableName <$> dtables) ++ "> ")
      let expecting = head (getVarTypes dtables)
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          let splitInput = trim <$> splitOn "," input
          if length splitInput /= length expecting
            then outputStrLn ("error: expected " ++ show (length expecting) ++ " arguments, got " ++ show (length splitInput) ++
                              "; arguments should be " ++ show expecting)
            else
            mapM_ (
                \dtable -> do
                  when (verbose opts) $ outputStrLn $ "** evaluating " ++ input ++ " against table " ++ tableName dtable
                  either
                    (\errstr -> outputStrLn $ "problem running " ++ input ++ " against table " ++ tableName dtable ++ ": " ++ errstr)
                    (outputStr . unlines . map (\resultrow ->
                                                   tableName dtable ++ ": " ++ intercalate ", " (showToJSON dtable resultrow)))
                    (evalTable dtable (zipWith mkF expecting splitInput))
                ) dtables
          outputStrLn ""
          loop opts dtables

    -- in future, allow decision tables to curry: partial application returns a partial decision table.
    -- completed evaluation returns the matching result columns, passed through an aggregation hit policy if necessary.

    -- TODO: output in the form of a decision table, showing all the matching rows (verbose) or only the result columns and annotations (normal)

    -- TODO: validation via incompleteness and conflict detection, under different hit policies

    fileChunks :: ArgOptions -> (Int, FilePath) -> IO InputChunks
    fileChunks opts (inum,infile) = do
      mylog opts $ "* opening file: " ++ infile
      inlines <- if infile == "-" then getContents else readFile infile
      let rawchunksEither = parseOnly (grepMarkdown ("f"++show inum) <?> "grepMarkdown") (T.pack inlines)
      either
        (\errstr -> myerr opts ("** parser failure in grepMarkdown: " ++ errstr) >> return [])
        return
        rawchunksEither
    myerr opts = hPutStrLn stderr
    mylog opts msg = when (verbose opts) $ myerr opts msg

    differentlyTyped :: [DecisionTable] -> Bool
    differentlyTyped dts = length (getVarTypes dts) /= 1

    getVarTypes :: [DecisionTable] -> [[Maybe DMNType]]
    getVarTypes dts = nub (((vartype <$>) . getInputHeaders) . header <$> dts)

-- not quite finished; in future refactor this over to JS.hs
showToJSON :: DecisionTable -> [[FEELexp]] -> [String]
showToJSON dtable cols = if not (null cols) then zipWith showFeels ((getOutputHeaders . header) dtable) cols else []

outputTo :: Handle -> String -> ArgOptions -> DecisionTable -> IO ()
outputTo h "js" opts dtable = hPutStrLn h $ toJS (JSOpts (Main.propstyle opts) (outformat opts == "ts")) dtable
outputTo h "ts" opts dtable = hPutStrLn h $ toJS (JSOpts (Main.propstyle opts) (outformat opts == "ts")) dtable

myOutHandle :: FilePath -> IO Handle
myOutHandle h = if h == "-" then return stdout else openFile h WriteMode

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
  (many irrelevantLine >> eof >> return Nothing)
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

