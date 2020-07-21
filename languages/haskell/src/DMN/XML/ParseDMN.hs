{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module DMN.XML.ParseDMN where

-- import Text.XML.HXT.Arrow.Pickle.Schema
-- import Text.XML.HXT.Arrow.ParserInterface

-- import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.Core

import qualified Control.Lens as L
import qualified Control.Lens.TH as L_TH
import Control.Lens.TH (makePrisms)

import DMN.XML.PickleHelpers

getEx1 :: IO [XmlTree]
getEx1 = runX $ readDocument [] "test/simulation.dmn"

getEx2 :: IO XmlTree
getEx2 = do
  [ans] <- runX $ removeAllWhiteSpace <<< readDocument [withCheckNamespaces True] "test/simple.dmn"
  pure ans

xmlns_dmn, xmlns_dmndi, xmlns_dc, xmlns_di, xmlns_camunda :: String
xmlns_dmn = "https://www.omg.org/spec/DMN/20191111/MODEL/"
xmlns_dmndi = "https://www.omg.org/spec/DMN/20191111/DMNDI/"
xmlns_dc = "http://www.omg.org/spec/DMN/20180521/DC/"
xmlns_di = "http://www.omg.org/spec/DMN/20180521/DI/"
xmlns_camunda = "http://camunda.org/schema/1.0/dmn"

data DMNDI = DMNDI
  deriving (Show)
makePrisms ''DMNDI

pdmndi :: PU DMNDI
pdmndi = xpElemNS xmlns_dmndi "dmndi" "DMNDI" $ xpLift DMNDI


type XDMN = Definitions

data Definitions = Definitions {defId :: String, defName :: String, defDMNDI :: DMNDI}
  deriving (Show)
makePrisms ''Definitions

dmnPickler :: PU XDMN
dmnPickler =
  wrapIso _Definitions
    . xpElemNS xmlns_dmn "" "definitions"
    . withNS
    -- . xpFilterAttr (getAttrValue _ _)
    -- . xpSeq' (xpAttr "namespace" xpUnit)  -- Ignore the namespace (I want to do the above though)
    . xpAddFixedAttr "namespace" xmlns_camunda  -- Ignore the namespace (I want to do the above though, and make it optional)
    $ xpTriple
     (xpAttr "id" xpText)
     (xpAttr "name" xpText)
     pdmndi

--- $> :i _Definitions
-- _Definitions :: L.Iso' Definitions (String, String, DMNDI)

instance XmlPickler Definitions where
  xpickle = dmnPickler

--   xpickle :: PU XDMN
--   xpickle = xpElemNS xmlns_dmn "dmn" "definitions" $ xpLift XDMN

withNS :: PU a -> PU a
withNS =
  xpAddNSDecl "" xmlns_dmn
    . xpAddNSDecl "dmndi" xmlns_dmndi
    . xpAddNSDecl "dc" xmlns_dc
    . xpAddNSDecl "di" xmlns_di
    . xpAddNSDecl "camunda" xmlns_camunda
    -- . xpAddNSDecl "qw4" "nope"

-- dmnPickler = xpElemNS xmlns_dmn "" "definitions" $ xpLift XDMN

pickleConfig :: [SysConfig]
pickleConfig = [withValidate no, withCheckNamespaces yes, withRemoveWS yes, withIndent yes]

parseDMN :: FilePath -> IO [XDMN]
parseDMN filename = runX $ xunpickleDocument dmnPickler pickleConfig filename

-- $> runEx1
runEx1 :: IO [XDMN]
runEx1 = parseDMN "test/simulation.dmn"

-- $> runEx2
runEx2 :: IO [XDMN]
runEx2 = parseDMN "test/simple.dmn"

ex3 :: XDMN
ex3 = Definitions "hi" "there" DMNDI

-- id="dinnerDecisions"
-- name="Dinner Decisions"
-- namespace="http://camunda.org/schema/1.0/dmn"

-- $> showEx3
showEx3 :: IO ()
showEx3 =
  putStrLn $
    showPickled
      [withValidate no, withCheckNamespaces yes, withRemoveWS yes, withIndent yes]
      ex3