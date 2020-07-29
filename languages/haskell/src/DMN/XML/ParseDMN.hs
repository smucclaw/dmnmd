{-# LANGUAGE Arrows #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module DMN.XML.ParseDMN where

-- import Text.XML.HXT.Arrow.Pickle.Schema
-- import Text.XML.HXT.Arrow.ParserInterface

-- import Data.Tree.NTree.TypeDefs

import qualified Control.Lens as L
import Control.Lens.Iso (AnIso')
import Control.Lens.TH (makePrisms)
import qualified Control.Lens.TH as L_TH
import qualified DMN.Types as DT
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
  { dmnId :: Maybe String,
    dmnName :: Maybe String
  }
  deriving (Eq)

makePrisms ''DmnCommon

instance Show DmnCommon where
  show (DmnCommon Nothing Nothing) = "unnamed"
  show (DmnCommon (Just a) (Just b)) = "dmnNamed " ++ show a ++ " " ++ show b
  show (DmnCommon a b) = "DmnCommon (" ++ show a ++ ") (" ++ show b ++ ")"

dmnNamed eid name = DmnCommon (Just eid) (Just name)

unnamed = DmnCommon Nothing Nothing

instance XmlPickler DmnCommon where
  xpickle =
    wrapIso _DmnCommon $
      xpPair
        (xpOption $ xpAttr "id" xpText)
        (xpOption $ xpAttr "name" xpText)

data DMNDI = DMNDI
  deriving (Show, Eq)

makePrisms ''DMNDI

instance XmlPickler DMNDI where
  xpickle =
    xpDMNDIElem "DMNDI" _DMNDI
      -- . xpFilterAttr (hasName "id" <+> hasName "name")
      . xpFilterCont none -- TODO
      $ xpickle

data RequiredInput = RequiredInput | RequiredDecision
  deriving (Show, Eq, Enum)

makePrisms ''RequiredInput

instance XmlPickler RequiredInput where
  xpickle =
    xpAlt
      fromEnum
      [ xpElemNS xmlns_dmn "" "requiredInput" $ xpLift RequiredInput,
        xpElemNS xmlns_dmn "" "requiredDecision" $ xpLift RequiredDecision
      ]

-- xpDMNElem "requiredInput" _RequiredInput
--   $ xpickle

pcklReqInput :: PU a -> PU (RequiredInput, a)
pcklReqInput p =
  xpAlt
    (fromEnum . fst)
    [ xpElemNS xmlns_dmn "" "requiredInput" $ xpPair (xpLift RequiredInput) p,
      xpElemNS xmlns_dmn "" "requiredDecision" $ xpPair (xpLift RequiredDecision) p
    ]

newtype Href = Href String
  deriving (Show, Eq)

makePrisms ''Href

instance XmlPickler Href where
  xpickle = wrapIso _Href $ xpAttr "href" xpText

data InformationRequirement = InformationRequirement
  { infrLabel :: DmnCommon,
    infrReq :: RequiredInput,
    infoHref :: Href
  }
  deriving (Show, Eq)

makePrisms ''InformationRequirement

instance XmlPickler InformationRequirement where
  xpickle =
    xpDMNElem "informationRequirement" (_InformationRequirement . pairsIso)
    -- . xpFilterAttr (hasName "id" <+> hasName "name")
    -- . xpFilterCont none -- TODO
    $
      xpPair xpickle (pcklReqInput xpickle)

{-
	<xsd:simpleType name="tBuiltinAggregator">
		<xsd:restriction base="xsd:string">
			<xsd:enumeration value="SUM"/>
			<xsd:enumeration value="COUNT"/>
			<xsd:enumeration value="MIN"/>
			<xsd:enumeration value="MAX"/>
-}

{-
<decisionTable id="DecisionTable_07q05jb" hitPolicy="COLLECT" aggregation="SUM">
 -}

showHitPolicy :: DT.HitPolicy -> String
showHitPolicy DT.HP_Unique = "UNIQUE"
showHitPolicy DT.HP_First = "FIRST"
showHitPolicy DT.HP_Priority = "PRIORITY"
showHitPolicy DT.HP_Any = "ANY"
showHitPolicy DT.HP_RuleOrder = "RULE ORDER"
showHitPolicy DT.HP_OutputOrder = "OUTPUT ORDER"
-- showHitPolicy DT.HP_Aggregate = "AGGREGATE" -- Not in the enum?
showHitPolicy (DT.HP_Collect _) = "COLLECT"
showHitPolicy DT.HP_Aggregate = error "HP_Aggregate is not supported for xml" -- What is this even?

-- TODO: Write tests for this

xparseHitPolicy :: String -> Either String DT.HitPolicy
xparseHitPolicy "UNIQUE" = Right DT.HP_Unique
xparseHitPolicy "FIRST" = Right DT.HP_First
xparseHitPolicy "PRIORITY" = Right DT.HP_Priority
xparseHitPolicy "ANY" = Right DT.HP_Any
xparseHitPolicy "OUTPUT ORDER" = Right DT.HP_OutputOrder
xparseHitPolicy "RULE ORDER" = Right DT.HP_RuleOrder
-- xparseHitPolicy "AGGREGATE" = Right DT.HP_Aggregate
xparseHitPolicy "COLLECT" = Right $ DT.HP_Collect DT.Collect_All
xparseHitPolicy x = Left $ "Unkown hit policy: " ++ x

--- $> xparseHitPolicy "UNIQUE"

-- This is a hack to handle merging the pair of HitPolicy and CollectOperator into a single value
groupHp :: (DT.HitPolicy, DT.CollectOperator) -> DT.HitPolicy
groupHp (DT.HP_Collect _, oper) = DT.HP_Collect oper
-- groupHp (hitPolicy, _) = hitPolicy
groupHp (hitPolicy, DT.Collect_All) = hitPolicy
groupHp (hitPolicy, oper) = error $ "Invalid aggregation " ++ show oper ++ " with hit policy " ++ show hitPolicy
                                  ++ ". Only COLLECT supports aggregation"

ungroupHp :: DT.HitPolicy -> (DT.HitPolicy, DT.CollectOperator)
ungroupHp (DT.HP_Collect oper) = (DT.HP_Collect oper, oper)
ungroupHp hp = (hp, DT.Collect_All)

xpHitPolicy :: PU DT.HitPolicy
xpHitPolicy =
  xpWrap (groupHp, ungroupHp) $
    xpPair
      (xpDefault DT.HP_Unique . xpAttr "hitPolicy" $ xpWrapEither (xparseHitPolicy, showHitPolicy) xpText)
      (xpDefault DT.Collect_All . xpAttr "aggregation" $ xpWrapEither (xparseAggregation, xshowAggregation) xpText)

xparseAggregation :: String -> Either String DT.CollectOperator
xparseAggregation "SUM" = Right DT.Collect_Sum
xparseAggregation "COUNT" = Right DT.Collect_Cnt
xparseAggregation "MIN" = Right DT.Collect_Min
xparseAggregation "MAX" = Right DT.Collect_Max
xparseAggregation x = Left $ "Unknown aggregation type: " ++ x

xshowAggregation :: DT.CollectOperator -> String
xshowAggregation DT.Collect_All = ""
xshowAggregation DT.Collect_Sum = "SUM"
xshowAggregation DT.Collect_Min = "MIN"
xshowAggregation DT.Collect_Max = "MAX"
xshowAggregation DT.Collect_Cnt = "COUNT"

-- DONE: Optional: default UNIQUE
-- Note: xpDefault doesn't write the default case, maybe we want to keep it still for HP_Unique?

data ColumnLabel = ColumnLabel
  { columnLabel :: String
  }
  deriving (Show, Eq)

makePrisms ''ColumnLabel

instance XmlPickler ColumnLabel where
  xpickle = wrapIso _ColumnLabel $ xpAttr "label" xpText

data TableInput = TableInput
  { tinpName :: DmnCommon
  , tinpLabel :: ColumnLabel
  }
  deriving (Show, Eq)

makePrisms ''TableInput

instance XmlPickler TableInput where
  xpickle =
    xpDMNElem "input" _TableInput
      . xpFilterAttr (hasName "id" <+> hasName "name" <+> hasName "label") -- TOOD
      . xpFilterCont none -- TODO
      $ xpickle

data TableOutput = TableOutput
  { toutName :: DmnCommon
  , toutLabel :: ColumnLabel
  }
  deriving (Show, Eq)

makePrisms ''TableOutput

instance XmlPickler TableOutput where
  xpickle =
    xpDMNElem "output" _TableOutput
      . xpFilterAttr (hasName "id" <+> hasName "name" <+> hasName "label") -- TOOD
      . xpFilterCont none -- TODO
      $ xpickle

data Rule = Rule
  { ruleLabel :: DmnCommon
  }
  deriving (Show, Eq)

makePrisms ''Rule

instance XmlPickler Rule where
  xpickle =
    xpDMNElem "rule" _Rule
      . xpFilterCont none -- TODO
      $ xpickle

data DecisionTable = DecisionTable
  { dtLabel :: DmnCommon,
    dtHitPolicy :: DT.HitPolicy,
    dtInput :: [TableInput],
    dtOutput :: [TableOutput], -- TODO: Should be NonEmpty
    dtRules :: [Rule]
  }
  deriving (Show, Eq)

makePrisms ''DecisionTable

instance XmlPickler DecisionTable where
  xpickle =
    xpDMNElem "decisionTable" _DecisionTable
      -- . xpFilterAttr (hasName "id" <+> hasName "name") -- TODO
      -- . xpFilterAttr (none `when` hasName "hitPolicy")
      -- . xpFilterCont none -- TODO
      $ xp5Tuple
        xpickle
        xpHitPolicy
        xpickle
        xpickle
        xpickle

data Decision = Decision
  { decLabel :: DmnCommon,
    decInfoReq :: [InformationRequirement],
    decDTable :: Maybe DecisionTable
  }
  deriving (Show, Eq)

makePrisms ''Decision

instance XmlPickler Decision where
  xpickle =
    xpDMNElem "decision" _Decision
      -- . xpFilterAttr (hasName "id" <+> hasName "name")
      -- . xpFilterCont none -- TODO
      . xpFilterCont (none `when` hasName "authorityRequirement")
      $ xpickle

data InputData = InputData
  { inpLabel :: DmnCommon
  }
  deriving (Show, Eq)

makePrisms ''InputData

instance XmlPickler InputData where
  xpickle = xpDMNElem "inputData" _InputData $ xpickle

data KnowledgeSource = KnowledgeSource
  { knsLabel :: DmnCommon
  }
  deriving (Show, Eq)

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
  deriving (Show, Eq)

makePrisms ''Definitions

type XDMN = Definitions

ex3 :: XDMN
ex3 =
  Definitions
    { defLabel = dmnNamed "hi" "there",
      descisionsDiagrams = [Decision (dmnNamed "a" "b") [InformationRequirement (dmnNamed "c" "d") RequiredInput (Href "#url")] Nothing],
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
-- runEx1 = parseDMN "test/simulation-collect-hit-policy.dmn"

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
