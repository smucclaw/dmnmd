{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction, MultiWayIf, OverloadedStrings, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}

{-| Handle command line options -}

module Options 
   where

import System.FilePath ( takeExtension )
import Data.Maybe ( fromMaybe )
import qualified Options.Applicative as OA
import Options.Applicative (Parser, long, short, help, helper, fullDesc, progDesc, strOption, switch, value, info, metavar, str, argument)

-- | let's do getopt properly

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

-- | define the options we take
argOptions :: OA.Parser ArgOptions
argOptions = ArgOptions
  <$> switch    (long "verbose"    <> short 'v'                                          <> help "more verbosity" )
  <*> switch    (long "query"      <> short 'q'                                          <> help "evaluate interactively" )
  <*> switch    (long "props"      <> short 'r'                                          <> help "JS functions use props style" )
  <*> ffmtOption (long "from"      <> short 'f' <> metavar "InputFormat"  <> value Unknown <> help "input format" )
  <*> ffmtOption (long "to"        <> short 't' <> metavar "OutputFormat" <> value Unknown <> help "output format" )
  <*> strOption (long "out"        <> short 'o' <> metavar "FILE"         <> value "-"   <> help "output file" )
  <*> strOption (long "pick"       <> short 'p' <> metavar "TABLE,..."    <> value ""    <> help "name of desired decision table" )
  <*> (OA.some ( argument str (metavar "FILES...")) OA.<|> pure ["-"])

-- | do the actual parsing of options
parseOptions :: IO ArgOptions
parseOptions = do
  opts1 <- OA.execParser $ info (argOptions OA.<**> helper) (fullDesc <> progDesc "DMN CLI interpreter and converter" <> OA.header "dmnmd")
  return $ detectOutformat . detectInformat $ opts1

-- * File format detection

-- | guess output format
detectOutformat :: ArgOptions -> ArgOptions
detectOutformat opts = opts { outformat = detectFormat [out opts] (outformat opts)}

-- | guess input format
detectInformat :: ArgOptions -> ArgOptions
detectInformat opts = opts { informat = detectFormat (input opts) (informat opts)}

-- | supported formats include typescriot and python
data FileFormat = Ts | Js | Py | Xml | Md | Unknown
  deriving (Show, Eq)

-- | A file format option
ffmtOption :: OA.Mod OA.OptionFields FileFormat -> Parser FileFormat
ffmtOption = OA.option parseFileFormat

-- | Parses the file format argument for the flags --to and --from
parseFileFormat :: OA.ReadM FileFormat
parseFileFormat = OA.eitherReader $ \case 
    "ts" -> return Ts
    "js" -> return Js
    "py" -> return Py
    "md" -> return Md
    "xml" -> return Xml
    _    -> Left "Accepted file types are 'ts', 'js', 'py', 'xml', and 'md'."

-- | from file extension string to internal types
fileExtensionMappings :: [(String, FileFormat)]
fileExtensionMappings =
  [ (".ts", Ts)
  , (".js", Js)
  , (".py", Py)
  , (".dmn", Xml)
  , (".md", Md)
  ]

-- | opposite direction
extensionToFileFormat :: String -> Maybe FileFormat
extensionToFileFormat ext = lookup ext fileExtensionMappings

-- | guess at file format
detectFormat :: [FilePath] -> FileFormat -> FileFormat
detectFormat files Unknown = fromMaybe Unknown $ detectFormat' files
detectFormat _ origFormat = origFormat

-- | helper function
-- > detectFormat' ["test/simulation.dmn"]
-- Just Xml
--
detectFormat' :: [FilePath] -> Maybe FileFormat
detectFormat' files = do
  let exts = map takeExtension files
  ext <- getSame exts
  extensionToFileFormat ext

-- | Checks if all elements in a list are equal, and if so, returns that element.
getSame :: Eq a => [a] -> Maybe a
getSame (x : xs) | all (== x) xs = Just x
getSame _ = Nothing
