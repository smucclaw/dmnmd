{-# LANGUAGE NoMonomorphismRestriction, MultiWayIf, OverloadedStrings, DuplicateRecordFields #-}

module Main (main) where

import System.IO
    ( stderr,
      hPutStrLn,
      Handle,
      hClose,
      openFile,
      stdout,
      IOMode(WriteMode) )
import Control.Monad ( when )
import Data.List.Split (splitOn)
import Data.List (intercalate, nub)

import System.Console.Haskeline
    ( defaultSettings,
      getInputLine,
      outputStr,
      outputStrLn,
      runInputT,
      InputT )
-- import Debug.Trace

import DMN.Types
    ( DecisionTable(header, tableName),
      FEELexp,
      DMNType,
      ColHeader(vartype) )
import DMN.DecisionTable
    ( trim, getOutputHeaders, getInputHeaders, evalTable, mkF )
import DMN.Translate.JS ( toJS, JSOpts(JSOpts) )
import DMN.Translate.PY ( toPY, PYOpts(PYOpts) )
import DMN.Translate.FEELhelpers ( showFeels )
import DMN.XML.ParseDMN (parseDMN)
import DMN.XML.XmlToDmnmd (convertAll)

import Options
    ( ArgOptions(propstyle, verbose, out, pick, query, informat, input,
                 outformat),
      parseOptions,
      FileFormat(Py, Md, Xml, Js, Ts) )
import ParseMarkdown (parseMarkdown)

-- | read DMN ASCII tables out of a Markdown file, and do useful things with it:
-- transpile to operational languages, evaluate expressions against the table, and so on.
main :: IO ()
main = do
  opts <- parseOptions
  mylog opts $ "Options: " ++ show opts
  myouthandle <- myOutHandle $ out opts

  -- a markdown file could contain multiple tables, so give the user the option of choosing one.
  mydtables <- parseTables opts -- TODO: Don't hardcode markdown here
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

  if | not $ query opts              -> mapM_ (outputTo myouthandle (outformat opts) opts) pickedTables
     | differentlyTyped pickedTables -> fail $ "tables " ++ show (tableName <$> pickedTables) ++ " have different types; can't query. use --pick to choose one"
     | query opts                    -> runInputT defaultSettings (loop opts pickedTables)
  hClose myouthandle

  where
    loop :: ArgOptions -> [DecisionTable] -> InputT IO ()
    loop opts dtables = do
      minput <- getInputLine (intercalate ", " (tableName <$> dtables) ++ "> ")
      let expecting = head (getVarTypes dtables)
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just inputCmd -> do
          let splitInput = trim <$> splitOn "," inputCmd
          if length splitInput /= length expecting
            then outputStrLn ("error: expected " ++ show (length expecting) ++ " arguments, got " ++ show (length splitInput) ++
                              "; arguments should be " ++ show expecting)
            else
            mapM_ (
                \dtable -> do
                  when (verbose opts) $ outputStrLn $ "** evaluating " ++ inputCmd ++ " against table " ++ tableName dtable
                  either
                    (\errstr -> outputStrLn $ "problem running " ++ inputCmd ++ " against table " ++ tableName dtable ++ ": " ++ errstr)
                    (outputStr . unlines . map (\resultrow ->
                                                   tableName dtable ++ ": " ++ intercalate ", " (showToJSON (outformat opts) dtable resultrow)))
                    (evalTable dtable (zipWith mkF expecting splitInput))
                ) dtables
          outputStrLn ""
          loop opts dtables

    -- in future, allow decision tables to curry: partial application returns a partial decision table.
    -- completed evaluation returns the matching result columns, passed through an aggregation hit policy if necessary.

    -- TODO: output in the form of a decision table, showing all the matching rows (verbose) or only the result columns and annotations (normal)

    -- TODO: validation via incompleteness and conflict detection, under different hit policies

    myerr _opts = hPutStrLn stderr
    mylog opts msg = when (verbose opts) $ myerr opts msg

    differentlyTyped :: [DecisionTable] -> Bool
    differentlyTyped dts = length (getVarTypes dts) /= 1

    getVarTypes :: [DecisionTable] -> [[Maybe DMNType]]
    getVarTypes dts = nub (((vartype <$>) . getInputHeaders) . header <$> dts)

-- | Abort the program with an error message
crash :: String -> a
crash = errorWithoutStackTrace

-- | initial parse of input tables. at present the emphasis is on markdown.
parseTables :: ArgOptions -> IO [DecisionTable]
parseTables opts = case informat opts of
  Md -> parseMarkdown opts
  Xml -> parseDmnXml opts
  x -> crash $ "Unsupported input format: " ++ show x
             ++ ".\nSupported formats are: 'md' and 'xml'"

-- | Not sure if this is actually functional.
parseDmnXml :: ArgOptions -> IO [DecisionTable]
parseDmnXml opts = do
  fileName <- case input opts of
    [fn] -> pure fn
    _ -> crash "Xml currently only supports a single file"

  convertAll <$> parseDMN fileName

-- | Transpile decision table to outpu tformats.
-- This is not quite finished; in future refactor this over to JS.hs
-- It would be nice to have a consistent pretty-printing library that we can use for both DMN and Natural4.
-- There are probably already packages on Hackage that represent these languages for output purposes.
showToJSON :: FileFormat -> DecisionTable -> [[FEELexp]] -> [String]
showToJSON Js dtable cols' = if not (null cols') then zipWith (showFeels "js") ((getOutputHeaders . header) dtable) cols' else []
showToJSON Ts dtable cols' = if not (null cols') then zipWith (showFeels "ts") ((getOutputHeaders . header) dtable) cols' else []
showToJSON Py dtable cols' = if not (null cols') then zipWith (showFeels "py") ((getOutputHeaders . header) dtable) cols' else []
-- NOTE: Probably equivalent to:
-- showToJSON dtable cols' = zipWith showFeels ((getOutputHeaders . header) dtable) cols'

-- | print to a file handle
outputTo :: Handle -> FileFormat -> ArgOptions -> DecisionTable -> IO ()
outputTo h Js opts dtable = hPutStrLn h $ toJS (JSOpts (Options.propstyle opts) (outformat opts == Ts)) dtable
outputTo h Ts opts dtable = hPutStrLn h $ toJS (JSOpts (Options.propstyle opts) (outformat opts == Ts)) dtable
outputTo h Py opts dtable = hPutStrLn h $ toPY (PYOpts (Options.propstyle opts))  dtable
outputTo _ filetype _ _   = crash $ "outputTo: Unsupported file type: " ++ show filetype 
                                   ++ ".\nSupported output formats are 'ts', 'js' and 'py'"

myOutHandle :: FilePath -> IO Handle
myOutHandle h = if h == "-" then return stdout else openFile h WriteMode

--  putStrLn $ toJS (fromRight (error "parse error") (parseOnly (parseTable "mydmn1") dmn2))
