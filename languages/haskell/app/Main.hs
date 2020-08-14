{-# LANGUAGE NoMonomorphismRestriction, MultiWayIf, OverloadedStrings, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main)
where

import System.IO
import Control.Monad
import Data.List.Split (splitOn)
import Data.List (intercalate, nub)

import System.Console.Haskeline
-- import Debug.Trace

import DMN.Types
import DMN.DecisionTable
import DMN.Translate.JS
import DMN.XML.ParseDMN (parseDMN)
import DMN.XML.XmlToDmnmd (convertAll)

import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdOutput)

import Options
import ParseMarkdown (parseMarkdown)


main :: IO ()
main = do
  opts <- parseOptions
  print opts
  myouthandle <- myOutHandle $ out opts

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

  istty <- queryTerminal stdOutput
  if | not $ query opts              -> mapM_ (outputTo myouthandle (outformat opts) opts) pickedTables
     | differentlyTyped pickedTables -> fail $ "tables " ++ show (tableName <$> pickedTables) ++ " have different types; can't query. use --pick to choose one"
     | query opts && istty           -> runInputT defaultSettings (loop opts pickedTables)
     | query opts && not istty       -> mylog opts "expecting eval term input on STDIN."
     | otherwise                     -> error "This should be impossible" -- Haskell can't figure out that the cases above are exhaustive
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
                                                   tableName dtable ++ ": " ++ intercalate ", " (showToJSON dtable resultrow)))
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

parseTables :: ArgOptions -> IO [DecisionTable]
parseTables opts = case informat opts of
  Md -> parseMarkdown opts
  Xml -> parseDmnXml opts
  x -> crash $ "Unsupported input format: " ++ show x
             ++ ".\nSupported formats are: 'md' and 'xml'"

parseDmnXml :: ArgOptions -> IO [DecisionTable]
parseDmnXml opts = do
  fileName <- case input opts of
    [fn] -> pure fn
    _ -> crash "Xml currently only supports a single file"

  convertAll <$> parseDMN fileName

-- not quite finished; in future refactor this over to JS.hs
showToJSON :: DecisionTable -> [[FEELexp]] -> [String]
showToJSON dtable cols' = if not (null cols') then zipWith showFeels ((getOutputHeaders . header) dtable) cols' else []
-- NOTE: Probably equivalent to:
-- showToJSON dtable cols' = zipWith showFeels ((getOutputHeaders . header) dtable) cols'

outputTo :: Handle -> FileFormat -> ArgOptions -> DecisionTable -> IO ()
outputTo h Js opts dtable = hPutStrLn h $ toJS (JSOpts (Options.propstyle opts) (outformat opts == Ts)) dtable
outputTo h Ts opts dtable = hPutStrLn h $ toJS (JSOpts (Options.propstyle opts) (outformat opts == Ts)) dtable
outputTo _ filetype _ _   = crash $ "outputTo: Unsupported file type: " ++ show filetype 
                                   ++ ".\nSupported output formats are 'ts' and 'js'"

myOutHandle :: FilePath -> IO Handle
myOutHandle h = if h == "-" then return stdout else openFile h WriteMode

--  putStrLn $ toJS (fromRight (error "parse error") (parseOnly (parseTable "mydmn1") dmn2))
