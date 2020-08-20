{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction, MultiWayIf, OverloadedStrings, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}

module Options 
--    ( parseOptions, ArgOptions(..), FileFormat(..))
   where

import System.FilePath
import Data.Maybe

-- import Debug.Trace

import qualified Options.Applicative as OA
import Options.Applicative (Parser, long, short, help, helper, fullDesc, progDesc, strOption, switch, value, info, metavar, str, argument)

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
  <*> ffmtOption (long "from"      <> short 'f' <> metavar "InputFormat"  <> value Unknown <> help "input format" )
  <*> ffmtOption (long "to"        <> short 't' <> metavar "OutputFormat" <> value Unknown <> help "output format" )
  <*> strOption (long "out"        <> short 'o' <> metavar "FILE"         <> value "-"   <> help "output file" )
  <*> strOption (long "pick"       <> short 'p' <> metavar "TABLE,..."    <> value ""    <> help "name of desired decision table" )
  <*> (OA.some ( argument str (metavar "FILES...")) OA.<|> pure ["-"])

parseOptions :: IO ArgOptions
parseOptions = do
  opts1 <- OA.execParser $ info (argOptions OA.<**> helper) (fullDesc <> progDesc "DMN CLI interpreter and converter" <> OA.header "dmnmd")
  return $ detectOutformat . detectInformat $ opts1

-- * File format detection

detectOutformat :: ArgOptions -> ArgOptions
detectOutformat opts = opts { outformat = detectFormat [out opts] (outformat opts)}

detectInformat :: ArgOptions -> ArgOptions
detectInformat opts = opts { informat = detectFormat (input opts) (informat opts)}

data FileFormat = Ts | Js | Xml | Md | Unknown
  deriving (Show, Eq)

-- | A file format option
ffmtOption :: OA.Mod OA.OptionFields FileFormat -> Parser FileFormat
ffmtOption = OA.option parseFileFormat

-- | Parses the file format argument for the flags --to and --from
parseFileFormat :: OA.ReadM FileFormat
parseFileFormat = OA.eitherReader $ \case 
    "ts" -> return Ts
    "js" -> return Js
    "md" -> return Md
    "xml" -> return Xml
    _    -> Left "Accepted file types are 'ts', 'js', 'xml', and 'md'."

testExtThing :: String
testExtThing = takeExtension "test/simulation.dmn"

fileExtensionMappings :: [(String, FileFormat)]
fileExtensionMappings =
  [ (".ts", Ts)
  , (".js", Js)
  , (".dmn", Xml)
  , (".md", Md)
  ]

extensionToFileFormat :: String -> Maybe FileFormat
extensionToFileFormat ext = lookup ext fileExtensionMappings

detectFormat :: [FilePath] -> FileFormat -> FileFormat
detectFormat files Unknown = fromMaybe Unknown $ detectFormat' files
detectFormat _ origFormat = origFormat

-- >>> detectFormat' ["test/simulation.dmn"]
-- Just Xml
detectFormat' :: [FilePath] -> Maybe FileFormat
detectFormat' files = do
  let exts = map takeExtension files
  ext <- getSame exts
  extensionToFileFormat ext

-- | Checks if all elements in a list are equal, and if so, returns that element.
getSame :: Eq a => [a] -> Maybe a
getSame (x : xs) | all (== x) xs = Just x
getSame _ = Nothing