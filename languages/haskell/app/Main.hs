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
import DMN.Translate.XML ( toXMLFile, defaultXMLOpts )
import DMN.Translate.FEELhelpers ( showFeels )
import DMN.XML.ParseDMN (parseDMNEither)
import DMN.XML.XmlToDmnmd (convertAll, renderDiagnostic, isError, anyErrors, Diagnostic)

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

  -- Refuse an output format we cannot write BEFORE reading anything. Two things
  -- were wrong while this lived in the per-table loop: the refusal only fired
  -- if the input happened to contain a table (so the same flag exited 0 on
  -- prose and 1 on a table — a refusal contingent on the input is an
  -- unimplemented branch, not a decision), and it fired after the -o file had
  -- already been destroyed.
  unless (query opts) $ checkOutFormat (outformat opts)

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

  if | not $ query opts              -> do
         -- Render first, open the destination second. 'openFile … WriteMode'
         -- truncates, so opening it up front destroyed the user's file before
         -- dmnmd knew whether it could read the input, whether the tables
         -- parsed, or whether it could write the format at all — defeating,
         -- for every -o user, the very guarantee the refusal below prints.
         src <- renderAll (outformat opts) opts pickedTables
         withOutHandle (out opts) (`hPutStr` src)
     | differentlyTyped pickedTables -> fail $ "tables " ++ show (tableName <$> pickedTables) ++ " have different types; can't query. use --pick to choose one"
     | query opts                    -> runInputT defaultSettings (loop opts pickedTables)

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
  -- D-7. Same shape as 'parseDmnXml' below, and for the same reason: print
  -- every diagnostic, then decide on the SEVERITIES rather than on the length
  -- of the list. A Warning here is something dropped and worth saying; an Error
  -- is a table we refused, and the tables list already excludes it, so exiting
  -- is what stops a partial answer being presented as a whole one.
  Md -> do
    (diags, tables) <- parseMarkdown opts
    mapM_ (hPutStrLn stderr . renderDiagnostic) diags
    when (anyErrors diags) $ do
      let errs = filter isError diags
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

-- | The output formats this binary can actually write.
--
-- One list, consulted once. There used to be two — @Options.parseFileFormat@'s
-- and the @outputTo@ fallthrough's — and they disagreed about @xml@ and @md@,
-- with the user meeting the generous one first.
implementedOutFormats :: [FileFormat]
implementedOutFormats = [Ts, Js, Py, L4, Xml]

-- | Refuse an output format we cannot write, before reading anything.
checkOutFormat :: FileFormat -> IO ()
checkOutFormat fmt
  | fmt `elem` implementedOutFormats = pure ()
  | otherwise = crash $
      "unsupported output format: " ++ show fmt
        ++ ".\nSupported output formats are 'ts', 'js', 'py', 'l4' and 'xml'"

-- | Render every picked table into the text of one output file.
--
-- L4 and XML go through a file-level function rather than table-by-table,
-- because both have file-level scope: a @DECLARE@ one table emits collides with
-- an identical one from the next
-- (@symptom\/l4-duplicate-declare-across-tables@), and a DMN document is a
-- single @\<definitions\>@ carrying every decision. A JS\/PY\/TS file is a
-- sequence of independent function definitions, so those stay per-table.
--
-- Byte-identical to the previous @mapM_ (outputTo …)@ for every format that
-- existed before: @hPutStrLn@ per table is @concatMap (++ "\\n")@, and L4's
-- @hPutStr src@ is @src@.
renderAll :: FileFormat -> ArgOptions -> [DecisionTable] -> IO String
renderAll L4 _opts dtables = fileLevel "L4 file" (toL4File defaultL4Opts dtables)
renderAll Xml _opts dtables = fileLevel "DMN document" (toXMLFile defaultXMLOpts dtables)
renderAll fmt opts dtables = pure $ concatMap ((++ "\n") . renderOne fmt opts) dtables

-- | Diagnostics from a file-level backend, and its text if none was an error.
fileLevel :: String -> ([Diagnostic], String) -> IO String
fileLevel what (diags, src) = do
  mapM_ (hPutStrLn stderr . ("dmnmd: " ++) . renderDiagnostic) diags
  when (any isError diags) $ do
    hPutStrLn stderr $
      "dmnmd: nothing was emitted for any table, because a partial " ++ what
        ++ " is indistinguishable from a complete one."
    exitFailure
  pure src

-- | Render one table, for the backends whose output is a run of independent
-- definitions.
renderOne :: FileFormat -> ArgOptions -> DecisionTable -> String
renderOne Js opts dtable = toJS (JSOpts (Options.propstyle opts) (outformat opts == Ts)) dtable
renderOne Ts opts dtable = toJS (JSOpts (Options.propstyle opts) (outformat opts == Ts)) dtable
renderOne Py opts dtable = toPY (PYOpts (Options.propstyle opts))  dtable
-- L4 and Xml are emitted per FILE: 'renderAll' intercepts them before this
-- function is reached. Kept as loud invariants rather than deleted, because a
-- second emission path is exactly how the duplicate-DECLARE bug would come back.
renderOne L4 _opts _dtable = crash "renderOne: L4 is emitted per file by renderAll, not per table"
renderOne Xml _opts _dtable = crash "renderOne: Xml is emitted per file by renderAll, not per table"
renderOne filetype _ _ = crash $ "renderOne: unsupported output format: " ++ show filetype
                                 ++ ".\nSupported output formats are 'ts', 'js', 'py', 'l4' and 'xml'"

-- | Run an action on the output destination, closing it only if we opened it.
--
-- @stdout@ was previously passed to 'hClose' on the way out, which is harmless
-- only because it was the last thing main did.
withOutHandle :: FilePath -> (Handle -> IO a) -> IO a
withOutHandle "-" act = act stdout
withOutHandle path act = do
  h <- openFile path WriteMode
  r <- act h
  hClose h
  pure r

--  putStrLn $ toJS (fromRight (error "parse error") (parseOnly (parseTable "mydmn1") dmn2))
