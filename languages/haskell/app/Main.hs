{-# LANGUAGE NoMonomorphismRestriction, MultiWayIf, OverloadedStrings, DuplicateRecordFields #-}

module Main (main) where

import System.IO
    ( stderr,
      hPutStr,
      hPutStrLn,
      Handle,
      hClose,
      openFile,
      stdout,
      IOMode(WriteMode) )
import Control.Monad ( when, unless, forM_, zipWithM )
import System.Exit ( exitFailure )
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
    ( trim, getOutputHeaders, getInputHeaders, evalTable, mkInputValue, splitArgs,
      tableWarnings )
import DMN.Translate.JS ( toJS, JSOpts(JSOpts) )
import DMN.Translate.PY ( toPY, PYOpts(PYOpts) )
import DMN.Translate.L4 ( toL4File, L4Opts(..), defaultL4Opts )
import DMN.Translate.FEELhelpers ( showFeels )
import DMN.XML.ParseDMN (parseDMNEither)
import DMN.XML.XmlToDmnmd (convertAll, renderDiagnostic, isError)

import Options
    ( ArgOptions(propstyle, verbose, out, pick, query, informat, input,
                 outformat),
      parseOptions,
      FileFormat(Py, Md, Xml, Js, Ts, L4) )
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

  -- Things worth saying out loud that are not grounds for refusal. Exit status
  -- is untouched: it answers only "did something we were asked to read fail to
  -- read?". The XML reader routes the same list through 'warnAt'.
  forM_ pickedTables $ \dt ->
    mapM_ (hPutStrLn stderr . (("warning: table " ++ show (tableName dt) ++ ": ") ++))
          (tableWarnings dt)

    -- are we talking to console or receiving input from STDIN?
    -- is the input coming in JSON format?
    -- which tables shall we run eval against? maybe the user gave a --pick. Maybe they didn't. if they didn't, run against all tables.
    -- if the tables have different input types, die. because our plan is to run the same input against all the different tables.

  if | not $ query opts              -> outputToAll myouthandle (outformat opts) opts pickedTables
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
          -- Bracket-aware, so `[1,2,3]` stays ONE argument. A plain
          -- splitOn "," predates types and shredded it into three.
          let splitInput = trim <$> splitArgs inputCmd
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
                    (evalTable dtable =<< zipWithM mkInputValue expecting splitInput)
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
--
-- The exit status answers exactly one question: did something we were asked to
-- read fail to read?
--
-- * a decision table that did not parse, or a DMN document that did not
--   unpickle, or a table we refused to convert — nonzero, including when other
--   tables in the same file were fine. A partial answer presented as a whole
--   one is the failure mode this is here to prevent.
-- * a well-formed file that simply contains no decision tables — zero. Prose
--   markdown, and DMN with no @<decision>@, are legitimate inputs.
parseTables :: ArgOptions -> IO [DecisionTable]
parseTables opts = case informat opts of
  Md -> do
    (errs, tables) <- parseMarkdown opts
    unless (null errs) $ do
      mapM_ (hPutStrLn stderr . ("error: " ++)) errs
      hPutStrLn stderr $
        "dmnmd: " ++ show (length errs) ++ " decision table(s) in "
          ++ intercalate ", " (input opts) ++ " could not be read"
          ++ (if null tables then "" else "; refusing to emit the "
                ++ show (length tables) ++ " that could, because partial output is"
                ++ " indistinguishable from complete output.")
      exitFailure
    pure tables
  Xml -> parseDmnXml opts
  x -> crash $ "Unsupported input format: " ++ show x
             ++ ".\nSupported formats are: 'md' and 'xml'"

-- | Read DMN XML. An unreadable document is fatal, not an empty table list.
parseDmnXml :: ArgOptions -> IO [DecisionTable]
parseDmnXml opts = do
  fileName <- case input opts of
    [fn] -> pure fn
    _ -> crash "Xml currently only supports a single file"

  parsed <- parseDMNEither fileName
  case parsed of
    Left err -> crash err
    Right defs -> do
      let (diags, tables) = convertAll defs
      mapM_ (hPutStrLn stderr . (\d -> fileName ++ ": " ++ renderDiagnostic d)) diags
      when (any isError diags) $ do
        hPutStrLn stderr $
          "dmnmd: " ++ fileName ++ ": one or more decision tables were refused"
            ++ " (see the errors above); nothing was emitted for them."
        exitFailure
      pure tables

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

-- | Emit every picked table to one handle.
--
-- L4 goes through 'toL4File' rather than table-by-table, because L4's top-level
-- scope is the whole file: a @DECLARE@ one table emits collides with an
-- identical one from the next (@symptom\/l4-duplicate-declare-across-tables@).
-- No other backend has file-level scope — a JS/PY/TS file is a sequence of
-- independent function definitions — so they stay per-table.
--
-- Byte-identical to the previous @mapM_ (outputTo …)@ for every format,
-- including L4: 'toL4File' currently just concatenates.
outputToAll :: Handle -> FileFormat -> ArgOptions -> [DecisionTable] -> IO ()
outputToAll h L4 _opts dtables = do
  let (diags, src) = toL4File defaultL4Opts dtables
  mapM_ (hPutStrLn stderr . ("dmnmd: " ++) . renderDiagnostic) diags
  when (any isError diags) $ do
    hPutStrLn stderr $
      "dmnmd: nothing was emitted for any table, because a partial L4 file is"
        ++ " indistinguishable from a complete one."
    exitFailure
  hPutStr h src
outputToAll h fmt opts dtables = mapM_ (outputTo h fmt opts) dtables

-- | print to a file handle
outputTo :: Handle -> FileFormat -> ArgOptions -> DecisionTable -> IO ()
outputTo h Js opts dtable = hPutStrLn h $ toJS (JSOpts (Options.propstyle opts) (outformat opts == Ts)) dtable
outputTo h Ts opts dtable = hPutStrLn h $ toJS (JSOpts (Options.propstyle opts) (outformat opts == Ts)) dtable
outputTo h Py opts dtable = hPutStrLn h $ toPY (PYOpts (Options.propstyle opts))  dtable
-- L4 is emitted per FILE, not per table: 'outputToAll' intercepts it before
-- this function is reached. Kept as a loud invariant rather than deleted,
-- because a second emission path is exactly how the duplicate-DECLARE bug would
-- come back.
outputTo _ L4 _opts _dtable = crash "outputTo: L4 is emitted per file by outputToAll, not per table"
outputTo _ filetype _ _   = crash $ "outputTo: Unsupported file type: " ++ show filetype
                                   ++ ".\nSupported output formats are 'ts', 'js', 'py' and 'l4'"

myOutHandle :: FilePath -> IO Handle
myOutHandle h = if h == "-" then return stdout else openFile h WriteMode

--  putStrLn $ toJS (fromRight (error "parse error") (parseOnly (parseTable "mydmn1") dmn2))
