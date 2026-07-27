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
import Data.Maybe (listToMaybe)
import Data.List (intercalate, nub)

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

-- | Namespaces of the DMN releases we can recognise. Only 1.3 is readable; the
-- others exist so that we can say /which/ version we are refusing rather than
-- failing with a generic unpickling error.
dmnVersionOfNamespace :: String -> Maybe String
dmnVersionOfNamespace ns = lookup ns
  [ ("http://www.omg.org/spec/DMN/20151101/dmn.xsd", "DMN 1.1")
  , ("http://www.omg.org/spec/DMN/20180521/MODEL/",  "DMN 1.2")
  , (xmlns_dmn,                                      "DMN 1.3")
  ]

xpDMNElem :: String -> AnIso' a b -> PU b -> PU a
xpDMNElem name iso = xpElemNS xmlns_dmn "" name . wrapIso iso

xpDMNDIElem :: String -> AnIso' a b -> PU b -> PU a
xpDMNDIElem name iso = xpElemNS xmlns_dmndi "dmndi" name . wrapIso iso

-- * Schema-guided ignoring
--
-- The picklers below are deliberately strict: an element or attribute that the
-- DMN 1.3 XSD does not allow in that position makes the whole document fail.
-- The helpers here exist so that the things the XSD /does/ allow but dmnmd has
-- no use for (diagram interchange, provenance, foreign extensions) can be
-- consumed *in their schema position* instead of being waved through by a
-- blanket filter.

-- | Consume an attribute that the XSD declares here but dmnmd does not model,
-- and throw its value away. Naming it explicitly keeps every *other* attribute
-- an error.
xpIgnoredAttr :: String -> PU a -> PU a
xpIgnoredAttr name =
  xpSeq' (xpWrap (const (), const Nothing) . xpOption $ xpAttr name xpText)

xpIgnoredAttrs :: [String] -> PU a -> PU a
xpIgnoredAttrs names p = foldr xpIgnoredAttr p names

-- | Consume one child element in the DMN namespace, with whatever attributes
-- and content it happens to carry, and throw it away. Only ever used at a
-- position where the XSD permits that element.
xpIgnoredElem :: String -> PU ()
xpIgnoredElem name =
  xpElemNS xmlns_dmn "" name . xpFilterAttr none . xpFilterCont none $ xpUnit

-- | Consume a run of elements we do not model, keeping ONE attribute so the
-- caller can name what it dropped. Everything else about them is discarded, so
-- this is as permissive as 'xpIgnoredElem' and cannot reject a document that
-- parses today.
--
-- 'hasNameWith' on the local part, never 'hasName': HXT's 'hasName' compares the
-- QUALIFIED name, so against a prefix-qualified document — which the DMN
-- specification's own Chapter 11 example is, and it is checked in under
-- @test/examples/@ — a bare 'hasName' silently matches nothing.
xpPeekedElems :: String -> String -> PU [Maybe String]
xpPeekedElems name attr =
  xpList $
    xpElemNS xmlns_dmn "" name
      . xpFilterAttr (hasNameWith ((== attr) . localPart))
      . xpFilterCont none
      $ xpOption (xpAttr attr xpText)

-- | @minOccurs="0" maxOccurs="1"@ version of 'xpIgnoredElem'.
xpIgnoredElemOpt :: String -> PU ()
xpIgnoredElemOpt name =
  xpWrap (const (), const Nothing) . xpOption $ xpIgnoredElem name

-- | @minOccurs="0" maxOccurs="unbounded"@ over a substitution group: any run of
-- children whose local names are in the given list.
xpIgnoredElemsOf :: [String] -> PU ()
xpIgnoredElemsOf [] = xpLift ()
xpIgnoredElemsOf names =
  xpWrap (const (), const []) . xpList . xpAlt (const 0) $ map xpIgnoredElem names

xpIgnoredElems :: String -> PU ()
xpIgnoredElems name = xpIgnoredElemsOf [name]

-- | The two children every @tDMNElement@ may carry, in schema order:
-- @<description>@ and @<extensionElements>@.
--
-- Both are dropped. @<description>@ is human-readable annotation that only
-- 'Rule' makes use of (as a row comment), so 'Rule' parses it explicitly
-- instead of using this. @<extensionElements>@ is declared
-- @<xsd:any namespace="##other" processContents="lax"/>@ — the standard itself
-- says a consumer may ignore it.
xpDmnAnnotations :: PU ()
xpDmnAnnotations =
  xpWrap (const (), const ((), ())) $
    xpPair (xpIgnoredElemOpt "description") (xpIgnoredElemOpt "extensionElements")

-- | @<extensionElements>@ only, for elements whose @<description>@ we keep.
xpExtensionElements :: PU ()
xpExtensionElements = xpIgnoredElemOpt "extensionElements"

-- | Attributes that are XML plumbing rather than DMN content, stripped from the
-- whole tree before unpickling:
--
-- * namespace declarations (@xmlns@, @xmlns:foo@). Once HXT has resolved names
--   these carry no information, and demanding a fixed set of them (as the old
--   'xpAddNSDecl' chain did) rejected both minimal files and files carrying one
--   extra unrelated declaration such as @xsi@.
-- * attributes in a foreign namespace. @tDMNElement@ ends with
--   @<xsd:anyAttribute namespace="##other" processContents="lax"/>@, so DMN
--   itself sanctions ignoring these (@camunda:inputVariable@, @kie:*@ …).
--
-- Unprefixed attributes have no namespace and are therefore *not* touched: an
-- unknown attribute in DMN's own vocabulary is still an error.
stripIgnorableAttrs :: ArrowXml a => a XmlTree XmlTree
stripIgnorableAttrs =
    processTopDown (processAttrl (none `when` (isNsDecl <+> isForeignAttr)) `when` isElem)
  where
    isNsDecl      = hasNameWith isNameSpaceName
    isForeignAttr = hasNameWith $ \qn ->
      let uri = namespaceUri qn in not (null uri) && uri /= xmlns_dmn

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

-- | Diagram interchange: geometry only, deliberately not modelled. The whole
-- subtree (and any attributes on it) is discarded.
instance XmlPickler DMNDI where
  xpickle =
    xpDMNDIElem "DMNDI" _DMNDI
      . xpFilterAttr none
      . xpFilterCont none
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

-- | @tInformationRequirement@: @description?@, @extensionElements?@, then
-- exactly one of @requiredDecision@ / @requiredInput@.
instance XmlPickler InformationRequirement where
  xpickle =
    xpDMNElem "informationRequirement" (_InformationRequirement . pairsIso)
      . xpIgnoredAttr "label"
    $
      xpPair xpickle (xpSeq' xpDmnAnnotations (pcklReqInput xpickle))

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

-- | @tDMNElement/@label@. DMN 1.3 makes it optional everywhere (XSD line 29,
-- @use="optional"@), so callers wrap this in 'xpOption'; requiring it here is
-- what used to reject perfectly legal @<input>@ and @<output>@ clauses.
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

-- | @tExpression@'s attributes: @id@/@label@ (from @tDMNElement@) plus an
-- optional @typeRef@. @label@ is consumed and dropped.
instance XmlPickler TExpr where
  xpickle = xpIgnoredAttr "label" . wrapIso _TExpr $ xpickle

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

-- | @tLiteralExpression@: @description?@, @extensionElements?@, then a choice of
-- @<text>@ or @<importedValues>@ (both optional). We model @<text>@; a literal
-- expression backed by @<importedValues>@ is rejected rather than silently read
-- as an empty expression.
instance XmlPickler TLiteralExpression where
  xpickle =
    wrapIso _TLiteralExpression $
      xpTriple
        xpickle                                    -- TExpr: id/typeRef attrs
        (xpOption xpickle)                         -- expressionLanguage attr
        (xpSeq' xpDmnAnnotations (xpOption xpickle))  -- <text>?

-- | @tUnaryTests@ (used by @<inputValues>@, @<outputValues>@, @<inputEntry>@).
-- Same shape as a literal expression but @<text>@ is required.
data UnaryTestsBody = UnaryTestsBody
  { utExpr :: TExpr
  , utExpressionLanguage :: Maybe ExpressionLanguage
  , utText :: TextElement
  }
  deriving (Show, Eq)

makePrisms ''UnaryTestsBody

instance XmlPickler UnaryTestsBody where
  xpickle =
    wrapIso _UnaryTestsBody $
      xpTriple xpickle (xpOption xpickle) (xpSeq' xpDmnAnnotations xpickle)

-- | @tInputClause/inputValues@ — the declared domain of an input column.
newtype InputValues = InputValues UnaryTestsBody
  deriving (Show, Eq)

makePrisms ''InputValues

instance XmlPickler InputValues where
  xpickle = xpDMNElem "inputValues" _InputValues xpickle

-- | @tOutputClause/outputValues@ — the declared domain of an output column.
-- Byte-identically unmodelled before this change, which is why a fix aimed only
-- at @defaultOutputEntry@ would have left it broken.
newtype OutputValues = OutputValues UnaryTestsBody
  deriving (Show, Eq)

makePrisms ''OutputValues

instance XmlPickler OutputValues where
  xpickle = xpDMNElem "outputValues" _OutputValues xpickle

-- | @tItemDefinition/allowedValues@ — the declared domain of a NAMED type.
--
-- The third @tUnaryTests@ user, and byte-identical in shape to the two above.
-- That is the whole point: an @\<allowedValues\>@ is an @\<inputValues\>@ that
-- has been given a name and can be referred to from more than one column.
newtype AllowedValues = AllowedValues UnaryTestsBody
  deriving (Show, Eq)

makePrisms ''AllowedValues

instance XmlPickler AllowedValues where
  xpickle = xpDMNElem "allowedValues" _AllowedValues xpickle

unAllowedValues :: AllowedValues -> UnaryTestsBody
unAllowedValues (AllowedValues b) = b

-- | The text of @\<itemDefinition\>\<typeRef\>@. Note this is an ELEMENT here,
-- unlike the @typeRef@ ATTRIBUTE that 'TypeRef' models on a column — same name,
-- different XSD position, so they cannot share a pickler.
newtype ItemTypeRef = ItemTypeRef String
  deriving (Show, Eq)

makePrisms ''ItemTypeRef

instance XmlPickler ItemTypeRef where
  xpickle = xpDMNElem "typeRef" _ItemTypeRef xpText0

unItemTypeRef :: ItemTypeRef -> String
unItemTypeRef (ItemTypeRef s) = s

-- | @tItemDefinition@ (XSD lines 232-247) — DMN's data model, and the thing a
-- column's @typeRef@ can name.
--
-- The XSD body is a @\<xsd:choice\>@ of three shapes, and knowing that is what
-- makes the diagnostics precise rather than generic:
--
--  1. @typeRef@ (required) + @allowedValues?@ — a simple type, optionally with a
--     domain. This is the one dmnmd honours.
--  2. @itemComponent*@ — a structured (record) type. Out of scope, and refused
--     by name rather than silently.
--  3. @functionItem?@ — a function type. Likewise.
--
-- __Modelled shallowly and filtered, not modelled fully.__ The choice looked
-- binary — stay totally permissive, or model every attribute and reject anything
-- unmodelled — but 'xpFilterAttr' and 'xpFilterCont' take an ARROW rather than a
-- boolean, so a NAME FILTER deletes everything except what is listed. @id@,
-- @label@, @typeLanguage@, @\<description\>@, @\<extensionElements\>@ and
-- foreign attributes are dropped exactly as they were when the whole element was
-- 'xpIgnoredElems'. So this cannot reject a document that parses today; the only
-- place strictness rises is inside @\<allowedValues\>@, where it becomes
-- identical to what @\<inputValues\>@ has imposed since E4 — both are
-- @tUnaryTests@.
--
-- The three optional children are picked as a SEQUENCE rather than as an
-- @xpAlt@ over the choice, which accepts some combinations the XSD forbids
-- (a @typeRef@ and an @itemComponent@ together). Being more permissive than the
-- schema cannot reject a valid document; being less permissive can.
data ItemDefinition = ItemDefinition
  { itdName :: Maybe String
    -- ^ @name@. Optional in the XSD (via @tNamedElement@), and an unnamed
    -- itemDefinition is unreferenceable — so it is reported, not resolved.
  , itdIsCollection :: Maybe String
    -- ^ @isCollection@, raw. Tier 2 turns this into 'DMN.Types.DMN_List'.
  , itdTypeRef :: Maybe ItemTypeRef
  , itdAllowedValues :: Maybe AllowedValues
  , itdComponents :: [Maybe String]
    -- ^ the @name@ of each @\<itemComponent\>@, and nothing else about it — the
    -- element is recursive, and peeking the name is enough to say what was
    -- refused.
  , itdFunctionItem :: ()
    -- ^ present-or-absent is not recoverable from @()@; a @\<functionItem\>@
    -- itemDefinition simply has no @typeRef@, which is how the converter reports
    -- it. Consumed rather than filtered out so that keeping it in the content
    -- filter stays honest.
  }
  deriving (Show, Eq)

makePrisms ''ItemDefinition

instance XmlPickler ItemDefinition where
  xpickle =
    xpDMNElem "itemDefinition" _ItemDefinition
      . xpFilterAttr (hasNameWith ((`elem` itemDefAttrs) . localPart))
      . xpFilterCont (hasNameWith ((`elem` itemDefElems) . localPart))
      $ xp6Tuple
          (xpOption (xpAttr "name" xpText))
          (xpOption (xpAttr "isCollection" xpText))
          (xpOption xpickle)
          (xpOption xpickle)
          (xpPeekedElems "itemComponent" "name")
          (xpIgnoredElemOpt "functionItem")

-- | The attributes and child elements 'ItemDefinition' keeps. __Every name test
-- here goes through @hasNameWith (… . localPart)@, never @hasName@__: HXT's
-- 'hasName' compares the QUALIFIED name, prefix included, so against
-- @\<semantic:typeRef\>@ it matches nothing and the filter deletes the very
-- children this exists to read. That is not exotic — the DMN specification's own
-- Chapter 11 example is prefix-qualified and is checked in under
-- @test\/examples\/@. The failure was silent and self-contradicting: the
-- itemDefinition parsed as empty and the diagnostic then said it declared no
-- @typeRef@ while the file had one on the next line.
itemDefAttrs :: [String]
itemDefAttrs = ["name", "isCollection"]

itemDefElems :: [String]
itemDefElems = ["typeRef", "allowedValues", "itemComponent", "functionItem"]

-- | @tOutputClause/defaultOutputEntry@ — the value taken when no rule matches.
newtype DefaultOutputEntry = DefaultOutputEntry TLiteralExpression
  deriving (Show, Eq)

makePrisms ''DefaultOutputEntry

instance XmlPickler DefaultOutputEntry where
  xpickle = xpDMNElem "defaultOutputEntry" _DefaultOutputEntry xpickle

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

-- | @tInputClause@: @description?@, @extensionElements?@, @inputExpression@
-- (required), @inputValues?@. Attributes @id@ and @label@ (both optional).
--
-- @camunda:inputVariable@ used to need a hand-written filter here; foreign
-- attributes are now stripped tree-wide by 'stripIgnorableAttrs', which is what
-- the XSD's @anyAttribute namespace="##other"@ actually calls for.
data TableInput = TableInput
  { tinpName :: DmnCommon
  , tinpLabel :: Maybe ColumnLabel
  , tinpExpr :: InputExpression
  , tinpValues :: Maybe InputValues
  }
  deriving (Show, Eq)

makePrisms ''TableInput

instance XmlPickler TableInput where
  xpickle =
    xpDMNElem "input" _TableInput
      $ xp4Tuple
          xpickle
          (xpOption xpickle)
          (xpSeq' xpDmnAnnotations xpickle)
          (xpOption xpickle)

-- | @tOutputClause@: @description?@, @extensionElements?@, @outputValues?@,
-- @defaultOutputEntry?@. Attributes @id@, @label@, @name@, @typeRef@ — all
-- optional (XSD lines 326-339: neither @name@ nor @typeRef@ carries a @use=@,
-- and @use="optional"@ is the default).
data TableOutput = TableOutput
  { toutName :: DmnCommon
  , toutLabel :: Maybe ColumnLabel
  , toutTypeRef :: Maybe TypeRef
  , toutValues :: Maybe OutputValues
  , toutDefault :: Maybe DefaultOutputEntry
  }
  deriving (Show, Eq)

makePrisms ''TableOutput

instance XmlPickler TableOutput where
  xpickle =
    xpDMNElem "output" _TableOutput
      $ xp5Tuple
          xpickle
          (xpOption xpickle)
          (xpOption xpickle)
          (xpSeq' xpDmnAnnotations (xpOption xpickle))
          (xpOption xpickle)

--- $> import Text.XML.HXT.Core

--- $> :i PU

--- $> theSchema (xpickle :: PU Decision)

data InputEntry = InputEntry
  { ieLabel :: DmnCommon
  , ieText :: TextElement
  }
  deriving (Show, Eq)

makePrisms ''InputEntry

-- | @tUnaryTests@ in the @<inputEntry>@ position.
instance XmlPickler InputEntry where
  xpickle =
    xpDMNElem "inputEntry" _InputEntry
      . xpIgnoredAttrs ["label", "typeRef", "expressionLanguage"]
      $ xpPair xpickle (xpSeq' xpDmnAnnotations xpickle)

data OutputEntry = OutputEntry
  { outputEntryLabel :: DmnCommon
  , outputEntryText :: TextElement
  }
  deriving (Show, Eq)

makePrisms ''OutputEntry

-- | @tLiteralExpression@ in the @<outputEntry>@ position.
instance XmlPickler OutputEntry where
  xpickle =
    xpDMNElem "outputEntry" _OutputEntry
      . xpIgnoredAttrs ["label", "typeRef", "expressionLanguage"]
      $ xpPair xpickle (xpSeq' xpDmnAnnotations xpickle)

-- | @tRuleAnnotation@ (XSD line 352): the @<annotationEntry>@ that hangs off a
-- @<rule>@. DMN 1.3's own spelling of a row comment, so 'DMN.XML.XmlToDmnmd'
-- carries it straight into 'DMN.Types.row_comments'. @<text>@ is @minOccurs=0@.
newtype AnnotationEntry = AnnotationEntry (Maybe TextElement)
  deriving (Show, Eq)

makePrisms ''AnnotationEntry

instance XmlPickler AnnotationEntry where
  xpickle = xpDMNElem "annotationEntry" _AnnotationEntry $ xpOption xpickle

-- | The text of an @<annotationEntry>@, or @""@ if it had no @<text>@ child.
annotationEntryText :: AnnotationEntry -> String
annotationEntryText (AnnotationEntry mt) = maybe "" innerText mt

-- | @tRuleAnnotationClause@ (XSD line 338): the column header for one
-- @<annotationEntry>@ position. Carries a @name@ and nothing else.
newtype AnnotationClause = AnnotationClause (Maybe String)
  deriving (Show, Eq)

makePrisms ''AnnotationClause

instance XmlPickler AnnotationClause where
  xpickle =
    xpDMNElem "annotation" _AnnotationClause . xpFilterCont none $
      xpOption (xpAttr "name" xpText)

annotationClauseName :: AnnotationClause -> String
annotationClauseName (AnnotationClause n) = maybe "" id n

data Rule = Rule
  { ruleLabel :: DmnCommon
  , ruleDescription :: Maybe Description
  , ruleInputEntry :: [InputEntry]
  , ruleOutputEntry :: [OutputEntry] -- TODO: Should be NonEmpty
  , ruleAnnotations :: [AnnotationEntry]
  }
  deriving (Show, Eq)

makePrisms ''Rule

-- | @tDecisionRule@: @description?@, @extensionElements?@, @inputEntry*@,
-- @outputEntry+@, @annotationEntry*@. Both @description@ and @annotationEntry@
-- are kept; they become row comments.
instance XmlPickler Rule where
  xpickle =
    xpDMNElem "rule" _Rule
      . xpIgnoredAttr "label"
      $ xp5Tuple
          xpickle
          (xpOption xpickle)
          (xpSeq' xpExtensionElements xpickle)
          xpickle
          xpickle

data DecisionTable = DecisionTable
  { dtLabel :: DmnCommon,
    dtHitPolicy :: DT.HitPolicy,
    dtInput :: [TableInput],
    dtOutput :: [TableOutput], -- TODO: Should be NonEmpty
    dtAnnotations :: [AnnotationClause],
    dtRules :: [Rule]
  }
  deriving (Show, Eq)

makePrisms ''DecisionTable

-- | @tDecisionTable@: @description?@, @extensionElements?@, @input*@,
-- @output+@, @annotation*@, @rule*@.
instance XmlPickler DecisionTable where
  xpickle =
    xpDMNElem "decisionTable" _DecisionTable
      . xpIgnoredAttrs ["label", "typeRef", "preferredOrientation", "outputLabel"]
      $ xp6Tuple
        xpickle
        xpHitPolicy
        (xpSeq' xpDmnAnnotations xpickle)
        (xpList1 xpickle)
        xpickle
        xpickle

data Expression = ExprDTable DecisionTable | ExprLiteral LiteralExpression
  deriving (Show, Eq)

exprNr :: Expression -> Int
exprNr (ExprDTable _) = 0
exprNr (ExprLiteral _) = 0

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

-- | @tDecision@: @description?@, @extensionElements?@, @question?@,
-- @allowedAnswers?@, @variable?@, @informationRequirement*@,
-- @knowledgeRequirement*@, @authorityRequirement*@, then a run of
-- @tDMNElementReference@ children, then the decision logic (@expression?@).
--
-- The previous version used name-based content filters for @variable@ and
-- @authorityRequirement@, which accepted them anywhere among the children.
-- These are positional: an element out of schema order is still an error.
instance XmlPickler Decision where
  xpickle =
    xpDMNElem "decision" _Decision
      . xpIgnoredAttr "label"
      $ xpTriple
          xpickle
          (xpSeq' decisionPrelude xpickle)
          (xpSeq' decisionInterlude xpickle)
    where
      decisionPrelude =
        xpWrap (const (), const ((), ((), ((), ())))) $
          xpPair
            xpDmnAnnotations
            (xpPair
              (xpIgnoredElemOpt "question")
              (xpPair (xpIgnoredElemOpt "allowedAnswers") (xpIgnoredElemOpt "variable")))
      decisionInterlude =
        xpIgnoredElemsOf
          [ "knowledgeRequirement", "authorityRequirement"
          , "supportedObjective", "impactedPerformanceIndicator"
          , "decisionMaker", "decisionOwner", "usingProcess", "usingTask"
          ]

-- | @tInformationItem@ in the @<variable>@ position. Parsed but not modelled
-- beyond its existence — see 'InputData'.
data InformationItem = InformationItem
  { iiLabel :: DmnNamed
  , iiTypeRef :: Maybe TypeRef
  }
  deriving (Show, Eq)

makePrisms ''InformationItem

-- | @tInputData@: @description?@, @extensionElements?@, @variable?@.
--
-- The @<variable>@ is how DMN names the value an @<inputData>@ node carries, so
-- real files have one almost always; before this change /any/ child element at
-- all made the whole document fail to unpickle.
data InputData = InputData
  { inpLabel :: DmnNamed
  , inpVariable :: Maybe InformationItem
  }
  deriving (Show, Eq)

makePrisms ''InputData

xpVariable :: PU InformationItem
xpVariable =
  xpDMNElem "variable" _InformationItem
    . xpIgnoredAttr "label"
    $ xpPair xpickle (xpSeq' xpDmnAnnotations (xpOption xpickle))

instance XmlPickler InputData where
  xpickle =
    xpDMNElem "inputData" _InputData
      . xpIgnoredAttr "label"
      $ xpPair xpickle (xpSeq' xpDmnAnnotations (xpOption xpVariable))

-- | @tKnowledgeSource@: provenance metadata, not decision logic. Consumed
-- wholesale.
data KnowledgeSource = KnowledgeSource
  { knsLabel :: DmnNamed
  }
  deriving (Show, Eq)

makePrisms ''KnowledgeSource

instance XmlPickler KnowledgeSource where
  xpickle =
    xpDMNElem "knowledgeSource" _KnowledgeSource
      . xpIgnoredAttrs ["label", "locationURI"]
      . xpFilterCont none
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



data Definitions = Definitions
  { defLabel :: DmnNamed,
    defsNamespace :: Namespace,
    defItemDefs :: [ItemDefinition],
    -- ^ DMN's data model. This was @xpIgnoredElems@ — the whole section dropped
    -- WITHOUT A WORD — then a name-only peek so the converter could at least say
    -- what it had thrown away, and is now modelled far enough to honour the case
    -- that matters: a named simple type with an @\<allowedValues\>@ domain,
    -- which a column's @typeRef@ inherits.
    defInputData :: [InputData],
    defsDescisions :: [Decision],
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
      defsDescisions = [
        Decision (dmnNamed' "a" "b") [
          InformationRequirement (dmnLabeled "c" "d") RequiredInput (Href "#url")
          ]
          Nothing],
      defInputData = [],
      defDrgElems = [],
      defDMNDI = Just DMNDI
    }

-- | @tDefinitions@: @description?@, @extensionElements?@, @import*@,
-- @itemDefinition*@, @drgElement*@, @artifact*@, @elementCollection*@,
-- @businessContextElement*@, @dmndi:DMNDI?@.
--
-- @defInputData@ / @defsDescisions@ / @defDrgElems@ are a greedy split of the
-- one @drgElement*@ run: leading @<inputData>@, then leading @<decision>@, then
-- whatever mixture follows. 'DMN.XML.XmlToDmnmd.convertIt' therefore has to look
-- for decisions in both of the last two.
--
-- Namespace declarations are no longer demanded here (see 'stripIgnorableAttrs'):
-- requiring a fixed set of them rejected minimal DMN 1.3 files that declare only
-- the model namespace, and also rejected files carrying one extra declaration.
-- The document is still pinned to DMN 1.3 by 'xpDMNElem', which matches
-- @definitions@ only in the 1.3 model namespace.
dmnPickler :: PU XDMN
dmnPickler =
  xpDMNElem "definitions" _Definitions
    . xpIgnoredAttrs ["label", "expressionLanguage", "typeLanguage", "exporter", "exporterVersion"]
    $ xp7Tuple
        xpickle                                  -- id / name attributes
        xpickle                                  -- namespace attribute
        (xpSeq' definitionsPrelude xpickle)       -- [ItemDefinition]; see defItemDefs
        xpickle                                  -- [InputData]
        xpickle                                  -- [Decision]
        xpickle                                  -- [DrgElems]
        (xpSeq' definitionsEpilogue xpickle)     -- Maybe DMNDI
  where
    definitionsPrelude =
      xpWrap (const (), const ((), ())) $
        xpPair
          xpDmnAnnotations
          (xpIgnoredElems "import")
    definitionsEpilogue =
      xpIgnoredElemsOf
        [ "association", "textAnnotation"          -- artifact
        , "elementCollection"
        , "performanceIndicator", "organizationUnit" -- businessContextElement
        ]

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

-- | Read a DMN 1.3 file, reporting *why* it could not be read.
--
-- 'Text.XML.HXT.Arrow.Pickle.xunpickleDocument' swallows unpickling failures:
-- it prints @fatal error: document unpickling failed@ and yields an empty list,
-- which is indistinguishable from a valid file containing nothing. Callers then
-- reported success. Here a failure is a 'Left' that the caller has to deal with.
parseDMNEither :: FilePath -> IO (Either String [XDMN])
parseDMNEither filename = do
  roots <- runX $ readDocument pickleConfig filename
                    >>> getChildren >>> isElem
                    >>> stripIgnorableAttrs
  pure $ case roots of
    [] ->
      Left $ filename ++ ": no XML root element found"
          ++ " (the file is missing, empty, or not well-formed XML)."
    (root : _) -> do
      checkDmnRoot filename root
      case unpickleDoc' dmnPickler root of
        Left msg -> Left $ filename ++ ": " ++ readerRefusal root ++ "\n" ++ msg
        Right v  -> Right [v]

-- | Say what actually went wrong.
--
-- The document has already been confirmed to be a DMN 1.3 @<definitions>@ by
-- 'checkDmnRoot', so blaming the version — as this message used to, for every
-- unpickling failure whatsoever — was simply false. The commonest real cause is
-- a DRG element that DMN 1.3 permits and dmnmd does not model
-- (@<businessKnowledgeModel>@, @<decisionService>@ …); name those when they are
-- present, and otherwise admit that we only have the unpickler's complaint.
readerRefusal :: XmlTree -> String
readerRefusal root
  | not (null unmodelled) =
      "this is valid DMN 1.3, but it contains "
        ++ intercalate ", " (map (\n -> "<" ++ n ++ ">") unmodelled)
        ++ ", which dmnmd does not model. Only <decision>, <inputData> and"
        ++ " <knowledgeSource> are read."
  | otherwise = "dmnmd could not read this DMN 1.3 document."
  where
    childNames = runLA (getChildren >>> isElem >>> getQName >>> arr localPart) root
    unmodelled = nub (filter (`elem` unmodelledDrgElements) childNames)

-- | Children of @<definitions>@ that the DMN 1.3 XSD allows but this reader has
-- no representation for. Listing them is what lets 'readerRefusal' tell the
-- truth instead of guessing at the version.
unmodelledDrgElements :: [String]
unmodelledDrgElements =
  [ "businessKnowledgeModel", "decisionService" ]

-- | Reject non-DMN-1.3 documents up front, naming what we actually found.
-- Without this, a DMN 1.1 or 1.2 file (correctly refused) failed with
-- @xpElem: got element name \"...\"@ deep inside the pickler.
checkDmnRoot :: FilePath -> XmlTree -> Either String ()
checkDmnRoot filename root =
  case listToMaybe (runLA getQName root) of
    Nothing -> Left $ filename ++ ": XML root element has no name."
    Just qn
      | localPart qn /= "definitions" ->
          Left $ filename ++ ": expected a <definitions> root element, but found <"
              ++ qualifiedName qn ++ ">."
      | namespaceUri qn == xmlns_dmn -> Right ()
      | otherwise ->
          Left $ filename ++ ": <definitions> is in namespace "
              ++ show (namespaceUri qn) ++ versionNote
              ++ ".\ndmnmd reads DMN 1.3 only, i.e. " ++ show xmlns_dmn ++ "."
      where
        versionNote = maybe "" (\v -> " (" ++ v ++ ")") (dmnVersionOfNamespace (namespaceUri qn))

-- | Backwards-compatible wrapper: throws in 'IO' on a bad document rather than
-- returning an empty list.
parseDMN :: FilePath -> IO [XDMN]
parseDMN filename = either fail pure =<< parseDMNEither filename

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
