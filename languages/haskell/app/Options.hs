{-# LANGUAGE NoMonomorphismRestriction, MultiWayIf, OverloadedStrings, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}

module Options ( parseOptions, ArgOptions(..), FileFormat) where

import System.FilePath
import Data.Maybe

-- import Debug.Trace

import qualified Options.Applicative as OA
import Options.Applicative (long, short, help, helper, fullDesc, progDesc, strOption, switch, value, info, metavar, str, argument)

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

parseOptions :: IO ArgOptions
parseOptions = do
  opts1 <- OA.execParser $ info (argOptions OA.<**> helper) (fullDesc <> progDesc "DMN CLI interpreter and converter" <> OA.header "dmnmd")
  let opts = detectOutformat . detectInformat $ opts1
  let infiles = if null (input opts) then ["-"] else input opts
  return $ opts { input = infiles }

-- * File format detection

detectOutformat :: ArgOptions -> ArgOptions
detectOutformat opts = opts { outformat = detectFormat [out opts] (outformat opts)}

detectInformat :: ArgOptions -> ArgOptions
detectInformat opts = opts { informat = detectFormat (input opts) (informat opts)}

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