{-# LANGUAGE Arrows #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module DMN.XML.ParseDMN where

-- import Text.XML.HXT.Arrow.Pickle.Schema
-- import Text.XML.HXT.Arrow.ParserInterface

-- import Data.Tree.NTree.TypeDefs

import qualified Control.Lens as L
import Control.Lens.Iso (AnIso')
import Control.Lens.TH (makePrisms)
import qualified Control.Lens.TH as L_TH
import DMN.XML.PickleHelpers
import Text.XML.HXT.Core

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

xpDMNElem :: String -> AnIso' a b -> PU b -> PU a
xpDMNElem name iso = xpElemNS xmlns_dmn "" name . wrapIso iso

xpDMNDIElem :: String -> AnIso' a b -> PU b -> PU a
xpDMNDIElem name iso = xpElemNS xmlns_dmndi "dmndi" name . wrapIso iso

withNS :: PU a -> PU a
withNS =
  xpAddNSDecl "" xmlns_dmn
    . xpAddNSDecl "dmndi" xmlns_dmndi
    . xpAddNSDecl "dc" xmlns_dc
    . xpAddNSDecl "di" xmlns_di
    . xpAddNSDecl "camunda" xmlns_camunda

data DmnCommon = DmnCommon
  { dmnId :: String,
    dmnName :: String
  }
  deriving (Show)

makePrisms ''DmnCommon

instance XmlPickler DmnCommon where
  xpickle =
    wrapIso _DmnCommon
      $ xpPair
        (xpAttr "id" xpText)
        (xpAttr "name" xpText)

data DMNDI = DMNDI
  deriving (Show)

makePrisms ''DMNDI

pdmndi :: PU DMNDI
pdmndi = xpDMNDIElem "DMNDI" _DMNDI $ xpLift ()
instance XmlPickler DMNDI where
  xpickle =
    xpDMNDIElem "DMNDI" _DMNDI
      -- . xpFilterAttr (hasName "id" <+> hasName "name")
      . xpFilterCont none -- TODO
      $ xpickle


data Decision = Decision
  { decLabel :: DmnCommon
  }
  deriving (Show)

makePrisms ''Decision

instance XmlPickler Decision where
  xpickle =
    xpDMNElem "decision" _Decision 
      -- . xpFilterAttr (hasName "id" <+> hasName "name")
      . xpFilterCont none -- TODO
      $ xpickle

data InputData = InputData
  { inpLabel :: DmnCommon
  }
  deriving (Show)

makePrisms ''InputData

instance XmlPickler InputData where
  xpickle =
    xpDMNElem "inputData" _InputData
      -- . xpFilterAttr (hasName "id" <+> hasName "name")
      . xpFilterCont none -- TODO
      $ xpickle

data KnowledgeSource = KnowledgeSource
  { knsLabel :: DmnCommon
  }
  deriving (Show)

makePrisms ''KnowledgeSource

instance XmlPickler KnowledgeSource where
  xpickle =
    xpDMNElem "knowledgeSource" _KnowledgeSource
      -- . xpFilterAttr (hasName "id" <+> hasName "name")
      . xpFilterCont none -- TODO
      $ xpickle

data Definitions = Definitions
  { defLabel :: DmnCommon,
    descisionsDiagrams :: [Decision],
    defInputData :: [InputData],
    defDrgElems :: [KnowledgeSource],
    defDMNDI :: Maybe DMNDI
  }
  deriving (Show)

makePrisms ''Definitions

type XDMN = Definitions

ex3 :: XDMN
ex3 =
  Definitions
    { defLabel = DmnCommon "hi" "there",
      descisionsDiagrams = [Decision $ DmnCommon "a" "b"],
      defInputData = [],
      defDrgElems = [],
      defDMNDI = Just DMNDI
    }

dmnPickler :: PU XDMN
dmnPickler =
  xpDMNElem "definitions" _Definitions
    . withNS
    -- . xpFilterAttr (getAttrValue _ _)
    -- . xpSeq' (xpAttr "namespace" xpUnit)  -- Ignore the namespace (I want to do the above though)
    . xpAddFixedAttr "namespace" xmlns_camunda -- Ignore the namespace (I want to do the above though, and make it optional)
    $ xpickle

--- $> :i _Definitions
-- _Definitions :: L.Iso' Definitions (String, String, DMNDI)

instance XmlPickler Definitions where
  xpickle = dmnPickler

--   xpickle :: PU XDMN
--   xpickle = xpElemNS xmlns_dmn "dmn" "definitions" $ xpLift XDMN

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
