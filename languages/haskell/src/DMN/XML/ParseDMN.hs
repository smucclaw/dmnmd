-- {-# LANGUAGE TypeApplications #-}
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
import Data.Void (Void)

getEx1 :: IO [XmlTree]
getEx1 = runX $ readDocument [] "test/simulation.dmn"

getEx2 :: IO XmlTree
getEx2 = do
  [ans] <- runX $ removeAllWhiteSpace <<< readDocument [withCheckNamespaces True] "test/simple.dmn"
  pure ans

ignoreContent :: [String] -> PU a -> PU a
ignoreContent name = xpFilterCont (none `when` foldl1 (<+>) (hasName <$> name))

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
    . xpFilterAttr (none `when` hasName "xmlns:camunda")

data Description = Description
  { description :: String
  }
  deriving (Show, Eq)

makePrisms ''Description

instance XmlPickler Description where
  xpickle = xpDMNElem "description" _Description xpText

data DmnNamed = DmnNamed
  { dmnnId :: Maybe String
  , dmnnName :: String
 }
  deriving Eq

makePrisms ''DmnNamed

instance XmlPickler DmnNamed where
  xpickle =
    wrapIso _DmnNamed $
      xpPair
        (xpOption $ xpAttr "id" xpText)
        (xpAttr "name" xpText)

dmnNamed' :: String -> String -> DmnNamed
dmnNamed' = DmnNamed . Just

instance Show DmnNamed where
  show (DmnNamed (Just a) b ) = "dmnNamed' " ++ show a ++ " " ++ show b
  show (DmnNamed Nothing b) = "DmnNamed Nothing " ++ show b

-- Corresponds to tDMNElement
data DmnCommon = DmnCommon
  { dmnId :: Maybe String
  , dmnLabel :: Maybe String
  -- The spec says that description should be here, but it's only relevant for rules, so I place it there instead
  -- , dmnDescription :: Maybe String
  }
  deriving (Eq)

makePrisms ''DmnCommon

instance Show DmnCommon where
  show (DmnCommon Nothing  Nothing  ) = "unnamed"
  show (DmnCommon (Just a) Nothing  ) = "dmnWithId " ++ show a
  show (DmnCommon (Just a) (Just b) ) = "dmnLabeled" ++ show a ++ " " ++ show b
  show (DmnCommon a b) = "DmnCommon (" ++ show a ++ ") (" ++ show b ++ ")"
  -- show (DmnCommon Nothing  Nothing  Nothing) = "unnamed"
  -- show (DmnCommon (Just a) Nothing  Nothing) = "dmnWithId " ++ show a
  -- show (DmnCommon (Just a) (Just b) Nothing) = "dmnNamed " ++ show a ++ " " ++ show b
  -- show (DmnCommon a b c) = "DmnCommon (" ++ show a ++ ") (" ++ show b ++ ") (" ++ show c ++ ")"

dmnLabeled :: String -> String -> DmnCommon
dmnLabeled eid name = (dmnWithId eid) {dmnLabel = Just name }

dmnWithId :: String -> DmnCommon
dmnWithId eid = unnamed {dmnId = (Just eid)}

unnamed :: DmnCommon
unnamed = DmnCommon Nothing Nothing
-- unnamed = DmnCommon Nothing Nothing Nothing

instance XmlPickler DmnCommon where
  xpickle =
    wrapIso _DmnCommon $
      xpPair
        (xpOption $ xpAttr "id" xpText)
        (xpOption $ xpAttr "name" xpText) -- NB: This should be "label" and not "name"
        -- (xpOption $ xpElemNS xmlns_dmn "" "description" xpText)

data DMNDI = DMNDI
  deriving (Show, Eq)

makePrisms ''DMNDI

instance XmlPickler DMNDI where
  xpickle =
    xpDMNDIElem "DMNDI" _DMNDI
      -- . xpFilterAttr (hasName "id" <+> hasName "name")
      . xpFilterCont none -- TODO
      $ xpickle

-- These can point to some input node (which is kind of useless) or to another table,
-- in which case it shows their dependency on each other.
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

-- TODO: Parse the "#" prefix of a href
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

data TypeRef = TypeRef
  { typeRef :: String
  }
  deriving (Show, Eq)

makePrisms ''TypeRef

instance XmlPickler TypeRef where
  xpickle = wrapIso _TypeRef $ xpAttr "typeRef" $ xpText

data ColumnLabel = ColumnLabel
  { columnLabel :: String
  }
  deriving (Show, Eq)

makePrisms ''ColumnLabel

instance XmlPickler ColumnLabel where
  xpickle = wrapIso _ColumnLabel $ xpAttr "label" xpText

-- TODO: This is only one of the possible options for tLiteralExpression
data TextElement = TextElement
  { innerText :: String
  }
  deriving (Show, Eq)

makePrisms ''TextElement

instance XmlPickler TextElement where
  xpickle =
    xpDMNElem "text" _TextElement
      $ xpText0

data TExpr = TExpr
  { exprLabel :: DmnCommon
  , exprTypeRef :: Maybe TypeRef
  }
  deriving (Show, Eq)

makePrisms ''TExpr

instance XmlPickler TExpr where
  xpickle = wrapIso _TExpr xpickle

data ExpressionLanguage = ExpressionLanguage String -- xsd:anyURI
  deriving (Show, Eq)

makePrisms ''ExpressionLanguage
instance XmlPickler ExpressionLanguage where
  xpickle = xpAttr "expressionLanguage" $ wrapIso _ExpressionLanguage xpText

data TLiteralExpression = TLiteralExpression
  { tleExpr :: TExpr
  , tleExpressionLanguage :: Maybe ExpressionLanguage
  , tleContent :: Maybe TextElement -- NB: Either this or importedValues
  }
  deriving (Show, Eq)

makePrisms ''TLiteralExpression

instance XmlPickler TLiteralExpression where
  xpickle = wrapIso _TLiteralExpression xpickle

data LiteralExpression = LiteralExpression TLiteralExpression
  deriving (Show, Eq)

makePrisms ''LiteralExpression

instance XmlPickler LiteralExpression where
  xpickle =
    xpDMNElem "literalExpression" _LiteralExpression
      -- . xpFilterAttr (hasName "id" <+> hasName "name")
      -- . xpFilterCont none -- TODO
      $ xpickle


data InputExpression = InputExpression TLiteralExpression
  deriving (Show, Eq)

makePrisms ''InputExpression

instance XmlPickler InputExpression where
  xpickle = xpDMNElem "inputExpression" _InputExpression xpickle

data TableInput = TableInput
  { tinpName :: DmnCommon
  , tinpLabel :: ColumnLabel
  , tinpExpr :: InputExpression
  }
  deriving (Show, Eq)

makePrisms ''TableInput

instance XmlPickler TableInput where
  xpickle =
    xpDMNElem "input" _TableInput
      -- . xpFilterAttr (hasName "id" <+> hasName "name" <+> hasName "label")
      -- . xpFilterAttr (none `when` hasQName (mkQName xmlns_camunda "camunda" "inputVariable"))
      . xpFilterAttr (none `when` hasName "camunda:inputVariable")
      $ xpickle

-- tOutputClause in schema
data TableOutput = TableOutput
  { toutName :: DmnCommon
  , toutLabel :: ColumnLabel -- Note: This is optional according to the schema, but that doesn't make much sense
  , toutTypeRef :: TypeRef
  }
  deriving (Show, Eq)

makePrisms ''TableOutput

instance XmlPickler TableOutput where
  xpickle =
    xpDMNElem "output" _TableOutput
      -- . xpFilterCont none -- TODO: Need a test case
      $ xpickle

--- $> import Text.XML.HXT.Core

--- $> :i PU

--- $> theSchema (xpickle :: PU Decision)

data InputEntry = InputEntry
  { ieLabel :: DmnCommon
  , ieText :: TextElement
  }
  deriving (Show, Eq)

makePrisms ''InputEntry

instance XmlPickler InputEntry where
  xpickle =
    xpDMNElem "inputEntry" _InputEntry
      -- . xpFilterAttr (hasName "id" <+> hasName "name")
      -- . xpFilterCont none -- TODO
      $ xpickle

data OutputEntry = OutputEntry
  { outputEntryLabel :: DmnCommon
  , outputEntryText :: TextElement
  }
  deriving (Show, Eq)

makePrisms ''OutputEntry

instance XmlPickler OutputEntry where
  xpickle =
    xpDMNElem "outputEntry" _OutputEntry
      -- . xpFilterAttr (hasName "id" <+> hasName "name")
      -- . xpFilterCont none -- TODO
      $ xpickle

data Rule = Rule
  { ruleLabel :: DmnCommon
  , ruleDescription :: Maybe Description
  , ruleInputEntry :: [InputEntry]
  , ruleOutputEntry :: [OutputEntry] -- TODO: Should be NonEmpty
  }
  deriving (Show, Eq)

makePrisms ''Rule

instance XmlPickler Rule where
  xpickle =
    xpDMNElem "rule" _Rule
      -- . xpFilterCont none -- TODO
      -- . xpFilterCont (none `when` hasName "hitPolicy")
      -- . xpFilterCont (hasName "description" <+> hasName "inputEntry")
      -- . xpFilterCont (none `when` hasName "outputEntry")
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
        (xpList1 xpickle)
        xpickle

data Expression = ExprDTable DecisionTable | ExprLiteral LiteralExpression
  deriving (Show, Eq)

exprNr :: Expression -> Int
exprNr (ExprDTable _) = 0
exprNr (ExprLiteral _) = 1

instance XmlPickler Expression where
  xpickle =
    xpAlt
      exprNr
      [ xpWrap (ExprDTable, \(ExprDTable x) -> x) xpickle
      , xpWrap (ExprLiteral, \(ExprLiteral x) -> x) xpickle
      ]

data Decision = Decision
  { decLabel :: DmnNamed, -- This should be tNamedElement (or tDRGElement)
    decInfoReq :: [InformationRequirement],
    decDTable :: Maybe Expression -- Schema says this could be any "expression", not just table
  }
  deriving (Show, Eq)

makePrisms ''Decision

instance XmlPickler Decision where
  xpickle =
    xpDMNElem "decision" _Decision
      -- . xpFilterAttr (hasName "id" <+> hasName "name")
      -- . xpFilterCont none -- TODO
      . ignoreContent ["authorityRequirement"] -- We don't care about "Authority" which express where rules come from
      . ignoreContent ["variable"] -- I don't know what this is
      $ xpickle

data InputData = InputData
  { inpLabel :: DmnNamed
  }
  deriving (Show, Eq)

makePrisms ''InputData

instance XmlPickler InputData where
  xpickle = xpDMNElem "inputData" _InputData $ xpickle

data KnowledgeSource = KnowledgeSource
  { knsLabel :: DmnNamed
  }
  deriving (Show, Eq)

makePrisms ''KnowledgeSource

instance XmlPickler KnowledgeSource where
  xpickle =
    xpDMNElem "knowledgeSource" _KnowledgeSource
      -- . xpFilterAttr (hasName "id" <+> hasName "name")
      . xpFilterCont none -- TODO
      $ xpickle

data Namespace = Namespace { namespace :: String }
  deriving (Show, Eq)

makePrisms ''Namespace

instance XmlPickler Namespace where
  xpickle = wrapIso _Namespace $ xpAttr "namespace" xpText

data DrgElems = DrgDec Decision | DrgInpData InputData | DrgKS KnowledgeSource
  deriving (Show, Eq)

drgNr :: DrgElems -> Int
drgNr (DrgDec _) = 0
drgNr (DrgInpData _) = 1
drgNr (DrgKS _) = 2

instance XmlPickler DrgElems where
  xpickle =
    xpAlt
      drgNr
      [ xpWrap (DrgDec, \(DrgDec x) -> x) xpickle
      , xpWrap (DrgInpData, \(DrgInpData x) -> x) xpickle
      , xpWrap (DrgKS, \(DrgKS x) -> x) xpickle
      ]

{-
 TODO: Make a generic instance that generates the code above
 - Make a test that the new code does the same as the old code
   - Preferably with a simpler example that doesn't contain all the nested complexity of the above code
 - Find a way to generically get the index of a constructor for a sum-type
 - Generate the list of the picklers
 - Ignore more complex cases (e.g. product types)
 - Make a newtype for DerivingVia

-}


data Definitions = Definitions
  { defLabel :: DmnNamed,
    defsNamespace :: Namespace,
    defInputData :: [InputData],
    defsDecisions :: [Decision],
    defDrgElems :: [DrgElems],
    defDMNDI :: Maybe DMNDI
  }
  deriving (Show, Eq)

makePrisms ''Definitions

type XDMN = Definitions

ex3 :: XDMN
ex3 =
  Definitions
    { defLabel = dmnNamed' "hi" "there",
      defsNamespace = Namespace xmlns_camunda,
      defsDecisions = [
        Decision (dmnNamed' "a" "b") [
          InformationRequirement (dmnLabeled "c" "d") RequiredInput (Href "#url")
          ]
          Nothing],
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
    -- . xpAddFixedAttr "namespace" xmlns_camunda -- Ignore the namespace (I want to do the above though, and make it optional)
    . ignoreContent ["dmndi"] -- We're not interested in diagrams
    . ignoreContent ["variable"] -- I don't know what this is
    . xpFilterAttr (none `when` (hasName "exporter" <+> hasName "exporterVersion") ) -- Used by Camunda Modeler
    $ xpickle

--     <variable id="InformationItem_1mjp1b5" name="safe price" typeRef="double" />

-- $> :m + Text.Pretty.Simple

-- $> :set -interactive-print pPrint

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

-- runX $ constA undefined >>> xpickleDTD @_ @() (xpickle :: PU Decision)
-- runX $ constA undefined >>> xpickleDTD @_ @() (xpickle :: PU Decision) >>> writeDocumentToString []
--- ^ Doesn't work when the data is filtered with xpFilterAttr/Cont. Fails with Prelude.foldr1.

-- $> parseDMN "test/safe2.dmn"

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
