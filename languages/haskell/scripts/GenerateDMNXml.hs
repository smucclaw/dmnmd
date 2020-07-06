#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell --show-trace -p "import scripts/my-stuff/fadno-xml/overlay.nix {}"
-- #! nix-shell -i runhaskell -p "haskellPackages.ghcWithPackages (pkg: with pkg;[ (haskell.lib.doJailbreak (import scripts/fadno-xml/overlay.nix)) ])" 
-- #! nix-shell -i runhaskell -p "haskellPackages.ghcWithPackages (pkg: with pkg;[ (haskell.lib.doJailbreak fadno-xml) ])" 
-- -- #! nix-shell -i runhaskell -shell -p "haskellPackages.ghcWithPackages (pkg: with pkg;[ (haskell.lib.doJailbreak fadno-xml) ])" 
-- #!/usr/bin/env stack
-- -- stack --resolver lts-15.12 script --package fadno-xml
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Fadno.Gen.GenDMNXml where

import Fadno.Xml.ParseXsd
import Fadno.Xml.EmitTypes
import Fadno.Xml.Codegen
import Data.Monoid
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import System.IO

main :: IO ()
main = runDMNXml

-- | Emit the first element only.
emitOneElement :: IO (Type,EmitState)
emitOneElement = do
  s <- loadDMNXml13
  runEmit (Env s) mempty $ emitElement (head $ M.elems $ _elements s)

-- | Emit 2.0 code.
runDMNXml :: IO ()
runDMNXml = do
  s <- loadDMNXml13
  e <- snd <$> runEmit (Env s) mempty (emitSchema s)
  withFile "src/DMN/DMNXml/DMNXml13.hs" WriteMode $ \h ->
      void $ runOut' h $ do
                  outputHeader "Fadno.DMNXml.DMNXml13"
                  outputTypes e


-- | Load XSD,XML and Xlink schemas.
loadXlinkXmlSchemas :: IO Schema
loadXlinkXmlSchemas = do
  xml <- namespaceSchema "xml" <$> parseFile "xsd/xml.xsd"
  xlink <- namespaceSchema "xlink" <$> parseFile "xsd/xlink.xsd"
  xsd <- loadXsdSchema "xsd/XMLSchema.xsd"
  return (xml <> xlink <> xsd)

-- | Load Music XML 2.0 schema
loadDMNXml13 :: IO Schema
loadDMNXml13 = do
  x <- parseFile "xsd/DMN13.xsd"
  deps <- loadXlinkXmlSchemas
  return (x <> deps)


{-

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Fadno.Gen.GenMusicXml where

import Fadno.Xml.ParseXsd
import Fadno.Xml.EmitTypes
import Fadno.Xml.Codegen
import Data.Monoid
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import System.IO

-- | Emit the first element only.
emitOneElement :: IO (Type,EmitState)
emitOneElement = do
  s <- loadMusicXml20
  runEmit (Env s) mempty $ emitElement (head $ M.elems $ _elements s)

-- | Emit 2.0 code.
runMusicXml20 :: IO ()
runMusicXml20 = do
  s <- loadMusicXml20
  e <- snd <$> runEmit (Env s) mempty (emitSchema s)
  withFile "src/Fadno/MusicXml/MusicXml20.hs" WriteMode $ \h ->
      void $ runOut' h $ do
                  outputHeader "Fadno.MusicXml.MusicXml20"
                  outputTypes e


-- | Load XSD,XML and Xlink schemas.
loadXlinkXmlSchemas :: IO Schema
loadXlinkXmlSchemas = do
  xml <- namespaceSchema "xml" <$> parseFile "xsd/xml.xsd"
  xlink <- namespaceSchema "xlink" <$> parseFile "xsd/xlink.xsd"
  xsd <- loadXsdSchema "xsd/XMLSchema.xsd"
  return (xml <> xlink <> xsd)

-- | Load Music XML 2.0 schema
loadMusicXml20 :: IO Schema
loadMusicXml20 = do
  x <- parseFile "xsd/musicxml.20.xsd"
  deps <- loadXlinkXmlSchemas
  return (x <> deps)
-}