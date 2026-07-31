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
import Data.List (find, intercalate, nub, nubBy)

getEx1 :: IO [XmlTree]
getEx1 = runX $ readDocument [] "test/simulation.dmn"

getEx2 :: IO XmlTree
getEx2 = do
  [ans] <- runX $ removeAllWhiteSpace <<< readDocument [withCheckNamespaces True] "test/simple.dmn"
  pure ans

xmlns_camunda :: String
xmlns_camunda = "http://camunda.org/schema/1.0/dmn"

-- * Which DMN release is this?

-- | One DMN release, as far as this reader is concerned: a name to put in
-- diagnostics, and the two namespace URIs a document of that release uses.
--
-- __MODEL and DMNDI are independent fields, not a date substituted into a
-- template.__ DMN 1.4 pairs a 1.4 model namespace with the /1.3/ DMNDI one:
-- @DMN14.xsd@ declares @xmlns=\"…\/20211108\/MODEL\/\"@ and then imports
-- @namespace=\"…\/20191111\/DMNDI\/\" schemaLocation=\"DMNDI13.xsd\"@, and the
-- OMG ships no @DMNDI14.xsd@ at all. A @mkRelease :: Date -> DmnRelease@ would
-- therefore be wrong on its very first use.
--
-- Both fields are 'String', so getting one wrong compiles, matches nothing, and
-- makes the @\<dmndi:DMNDI\>@ subtree resurface as @xpCheckEmptyContents@ — the
-- generic failure this module exists to avoid. @test\/dmn15\/baseline14.dmn@ and
-- @policy\/xml-dmn14-accepted@ are the guard, and 1.4 is the only release they
-- CAN guard: it is the only one whose DMNDI date differs from its model date, so
-- a 1.3 or 1.5 fixture cannot tell a date-template design from a correct one.
data DmnRelease = DmnRelease
  { relName :: String
  , relModelNS :: String
  , relDmndiNS :: String
  }
  deriving (Show, Eq)

-- | The releases this reader can read.
--
-- Adding a fourth is one record, /provided/ its decision-table complex types are
-- still byte-identical — which is the property that makes one pickler tree
-- serve all of them, and which must be re-measured rather than assumed. It held
-- from 1.3 to 1.5: @tDecisionTable@, @tInputClause@, @tOutputClause@,
-- @tDecisionRule@, @tUnaryTests@ and @tLiteralExpression@ are byte-identical in
-- @DMN13.xsd@ and @DMN15.xsd@, and nothing was removed anywhere.
--
-- DMN 1.6 Beta 1 (OMG @dtc\/24-05-18@) is dated @20240513@ and is /not/ listed
-- here: the beta text pins @…\/20240513\/FEEL\/@ but never spells its MODEL or
-- DMNDI URI, and guessing one from the pattern would put an unverified string in
-- an accept list. Measure it from a published @DMN16.xsd@, then add the record.
dmn13, dmn14, dmn15 :: DmnRelease
dmn13 =
  DmnRelease "DMN 1.3"
    "https://www.omg.org/spec/DMN/20191111/MODEL/"
    "https://www.omg.org/spec/DMN/20191111/DMNDI/"
dmn14 =
  DmnRelease "DMN 1.4"
    "https://www.omg.org/spec/DMN/20211108/MODEL/"
    "https://www.omg.org/spec/DMN/20191111/DMNDI/" -- not a typo; see 'DmnRelease'
dmn15 =
  DmnRelease "DMN 1.5"
    "https://www.omg.org/spec/DMN/20230324/MODEL/"
    "https://www.omg.org/spec/DMN/20230324/DMNDI/"

readableReleases :: [DmnRelease]
readableReleases = [dmn13, dmn14, dmn15]

-- | Releases we can /name/ but not read. Widening acceptance is not accepting
-- everything: a DMN 1.1 or 1.2 document is still refused, and the point of this
-- table is that it is refused by name rather than by a generic unpickling
-- failure.
--
-- Note the scheme. 1.1 and 1.2 are @http:\/\/@; every release from 1.3 on is
-- @https:\/\/@. A copy-paste that carried @http@ forward would match nothing.
refusedReleases :: [(String, String)]
refusedReleases =
  [ ("http://www.omg.org/spec/DMN/20151101/dmn.xsd", "DMN 1.1")
  , ("http://www.omg.org/spec/DMN/20180521/MODEL/",  "DMN 1.2")
  ]

-- | The release a @\<definitions\>@ model namespace identifies, if we read it.
releaseOfNamespace :: String -> Maybe DmnRelease
releaseOfNamespace ns = lookup ns [(relModelNS r, r) | r <- readableReleases]

-- | The name of any release we recognise, readable or not, for diagnostics.
--
-- Also names a DMNDI namespace, and says so: a message reading @(DMN 1.3)@ for a
-- stray @xmlns:dmndi@ would send the reader to the wrong attribute. The DMNDI
-- URIs are not one-to-one with releases — DMN 1.4 reuses 1.3's — so this reports
-- the first release declaring it, which is the one whose spelling the author
-- most likely copied.
releaseNameOfNamespace :: String -> Maybe String
releaseNameOfNamespace ns = case lookup ns refusedReleases of
  Just n -> Just n
  Nothing -> case relName <$> releaseOfNamespace ns of
    Just n -> Just n
    Nothing -> (++ " DMNDI") . relName <$> find ((== ns) . relDmndiNS) readableReleases

-- * The pickler class
--
-- $pickler
--
-- hxt's 'XmlPickler' has method @xpickle :: PU a@ — a VALUE, with nowhere to put
-- the release. Nor can the release go /inside/ the 'PU': it is a plain
-- three-field record with no Functor\/Applicative\/Monad instance, its
-- unpickler state is a fixed hxt record with no extension slot, and 'xpElemQN'
-- bakes the 'QName' in at construction time, so a global mutable cell would be
-- frozen by CAF memoisation the first time 'dmnPickler' was forced. The only
-- channel left is an ordinary function argument — so the class is replaced
-- rather than the picklers restructured.
--
-- __The four container instances below deliberately mirror hxt's own__
-- (@Pickle\/Xml.hs@ :1160, :1163, :1232, :1235). Roughly fifteen sites in this
-- file write a bare @xpickle@ that stands for a list, a @Maybe@, a pair or
-- @()@ and let the instance resolve it; keeping a class keeps them resolving to
-- exactly the same combinator. Hand-writing them instead would be fifteen
-- chances to put 'xpList' where 'xpList1' belongs — a change of @minOccurs@ that
-- has the same type, so neither the compiler nor the golden baseline distinguish
-- it from the original, only a reader would.

-- | Like hxt's 'XmlPickler', but the pickler is a function of the DMN release.
class DmnPU a where
  dmnPU :: DmnRelease -> PU a

instance DmnPU () where
  dmnPU _ = xpUnit

instance (DmnPU a, DmnPU b) => DmnPU (a, b) where
  dmnPU r = xpPair (dmnPU r) (dmnPU r)

instance DmnPU a => DmnPU [a] where
  dmnPU r = xpList (dmnPU r)

instance DmnPU a => DmnPU (Maybe a) where
  dmnPU r = xpOption (dmnPU r)

xpDMNElem :: DmnRelease -> String -> AnIso' a b -> PU b -> PU a
xpDMNElem r name iso = xpElemNS (relModelNS r) "" name . wrapIso iso

-- | The /only/ reader of 'relDmndiNS'. If this is ever given 'relModelNS' by
-- mistake it still compiles, matches nothing, and the whole @\<dmndi:DMNDI\>@
-- subtree then resurfaces as @xpCheckEmptyContents@ — the generic failure this
-- change exists to avoid, arriving through the back door.
xpDMNDIElem :: DmnRelease -> String -> AnIso' a b -> PU b -> PU a
xpDMNDIElem r name iso = xpElemNS (relDmndiNS r) "dmndi" name . wrapIso iso

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
xpIgnoredElem :: DmnRelease -> String -> PU ()
xpIgnoredElem r name =
  xpElemNS (relModelNS r) "" name . xpFilterAttr none . xpFilterCont none $ xpUnit

-- | Consume a run of elements we do not model, keeping ONE attribute so the
-- caller can name what it dropped. Everything else about them is discarded, so
-- this is as permissive as 'xpIgnoredElem' and cannot reject a document that
-- parses today.
--
-- 'hasNameWith' on the local part, never 'hasName': HXT's 'hasName' compares the
-- QUALIFIED name, so against a prefix-qualified document — which the DMN
-- specification's own Chapter 11 example is, and it is checked in under
-- @test/examples/@ — a bare 'hasName' silently matches nothing.
xpPeekedElems :: DmnRelease -> String -> String -> PU [Maybe String]
xpPeekedElems r name attr =
  xpList $
    xpElemNS (relModelNS r) "" name
      . xpFilterAttr (hasNameWith ((== attr) . localPart))
      . xpFilterCont none
      $ xpOption (xpAttr attr xpText)

-- | @minOccurs="0" maxOccurs="1"@ version of 'xpIgnoredElem'.
xpIgnoredElemOpt :: DmnRelease -> String -> PU ()
xpIgnoredElemOpt r name =
  xpWrap (const (), const Nothing) . xpOption $ xpIgnoredElem r name

-- | @minOccurs="0" maxOccurs="unbounded"@ over a substitution group: any run of
-- children whose local names are in the given list.
xpIgnoredElemsOf :: DmnRelease -> [String] -> PU ()
xpIgnoredElemsOf _ [] = xpLift ()
xpIgnoredElemsOf r names =
  xpWrap (const (), const []) . xpList . xpAlt (const 0) $ map (xpIgnoredElem r) names

xpIgnoredElems :: DmnRelease -> String -> PU ()
xpIgnoredElems r name = xpIgnoredElemsOf r [name]

-- | The two children every @tDMNElement@ may carry, in schema order:
-- @<description>@ and @<extensionElements>@.
--
-- Both are dropped. @<description>@ is human-readable annotation that only
-- 'Rule' makes use of (as a row comment), so 'Rule' parses it explicitly
-- instead of using this. @<extensionElements>@ is declared
-- @<xsd:any namespace="##other" processContents="lax"/>@ — the standard itself
-- says a consumer may ignore it.
xpDmnAnnotations :: DmnRelease -> PU ()
xpDmnAnnotations r =
  xpWrap (const (), const ((), ())) $
    xpPair (xpIgnoredElemOpt r "description") (xpIgnoredElemOpt r "extensionElements")

-- | @<extensionElements>@ only, for elements whose @<description>@ we keep.
xpExtensionElements :: DmnRelease -> PU ()
xpExtensionElements r = xpIgnoredElemOpt r "extensionElements"

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
--
-- \"Foreign\" is relative to the release the document declared, which is why
-- this runs /after/ 'checkDmnRoot' rather than inside the reading pipeline: a
-- DMN 1.3 attribute on a DMN 1.5 element is as foreign as a Camunda one.
stripIgnorableAttrs :: ArrowXml a => DmnRelease -> a XmlTree XmlTree
stripIgnorableAttrs r =
    processTopDown (processAttrl (none `when` (isNsDecl <+> isForeignAttr)) `when` isElem)
  where
    isNsDecl      = hasNameWith isNameSpaceName
    isForeignAttr = hasNameWith $ \qn ->
      let uri = namespaceUri qn in not (null uri) && uri /= relModelNS r

data Description = Description
  { description :: String
  }
  deriving (Show, Eq)

makePrisms ''Description

instance DmnPU Description where
  dmnPU r = xpDMNElem r "description" _Description xpText

data DmnNamed = DmnNamed
  { dmnnId :: Maybe String
  , dmnnName :: String
 }
  deriving Eq

makePrisms ''DmnNamed

instance DmnPU DmnNamed where
  dmnPU r =
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

instance DmnPU DmnCommon where
  dmnPU r =
    wrapIso _DmnCommon $
      xpPair
        (xpOption $ xpAttr "id" xpText)
        (xpOption $ xpAttr "name" xpText) -- NB: This should be "label" and not "name"
        -- (xpOption $ xpElemNS (relModelNS r) "" "description" xpText)

data DMNDI = DMNDI
  deriving (Show, Eq)

makePrisms ''DMNDI

-- | Diagram interchange: geometry only, deliberately not modelled. The whole
-- subtree (and any attributes on it) is discarded.
instance DmnPU DMNDI where
  dmnPU r =
    xpDMNDIElem r "DMNDI" _DMNDI
      . xpFilterAttr none
      . xpFilterCont none
      $ (dmnPU r)

-- These can point to some input node (which is kind of useless) or to another table,
-- in which case it shows their dependency on each other.
data RequiredInput = RequiredInput | RequiredDecision
  deriving (Show, Eq, Enum)

makePrisms ''RequiredInput

instance DmnPU RequiredInput where
  dmnPU r =
    xpAlt
      fromEnum
      [ xpElemNS (relModelNS r) "" "requiredInput" $ xpLift RequiredInput,
        xpElemNS (relModelNS r) "" "requiredDecision" $ xpLift RequiredDecision
      ]

-- xpDMNElem "requiredInput" _RequiredInput
--   $ xpickle

pcklReqInput :: DmnRelease -> PU a -> PU (RequiredInput, a)
pcklReqInput r p =
  xpAlt
    (fromEnum . fst)
    [ xpElemNS (relModelNS r) "" "requiredInput" $ xpPair (xpLift RequiredInput) p,
      xpElemNS (relModelNS r) "" "requiredDecision" $ xpPair (xpLift RequiredDecision) p
    ]

newtype Href = Href String
  deriving (Show, Eq)

makePrisms ''Href

-- TODO: Parse the "#" prefix of a href
instance DmnPU Href where
  dmnPU r = wrapIso _Href $ xpAttr "href" xpText

data InformationRequirement = InformationRequirement
  { infrLabel :: DmnCommon,
    infrReq :: RequiredInput,
    infoHref :: Href
  }
  deriving (Show, Eq)

makePrisms ''InformationRequirement

-- | @tInformationRequirement@: @description?@, @extensionElements?@, then
-- exactly one of @requiredDecision@ / @requiredInput@.
instance DmnPU InformationRequirement where
  dmnPU r =
    xpDMNElem r "informationRequirement" (_InformationRequirement . pairsIso)
      . xpIgnoredAttr "label"
    $
      xpPair (dmnPU r) (xpSeq' (xpDmnAnnotations r) (pcklReqInput r (dmnPU r)))

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

instance DmnPU TypeRef where
  dmnPU r = wrapIso _TypeRef $ xpAttr "typeRef" $ xpText

data ColumnLabel = ColumnLabel
  { columnLabel :: String
  }
  deriving (Show, Eq)

makePrisms ''ColumnLabel

-- | @tDMNElement/@label@. DMN 1.3 makes it optional everywhere (XSD line 29,
-- @use="optional"@), so callers wrap this in 'xpOption'; requiring it here is
-- what used to reject perfectly legal @<input>@ and @<output>@ clauses.
instance DmnPU ColumnLabel where
  dmnPU r = wrapIso _ColumnLabel $ xpAttr "label" xpText

-- TODO: This is only one of the possible options for tLiteralExpression
data TextElement = TextElement
  { innerText :: String
  }
  deriving (Show, Eq)

makePrisms ''TextElement

instance DmnPU TextElement where
  dmnPU r =
    xpDMNElem r "text" _TextElement
      $ xpText0

data TExpr = TExpr
  { exprLabel :: DmnCommon
  , exprTypeRef :: Maybe TypeRef
  }
  deriving (Show, Eq)

makePrisms ''TExpr

-- | @tExpression@'s attributes: @id@/@label@ (from @tDMNElement@) plus an
-- optional @typeRef@. @label@ is consumed and dropped.
instance DmnPU TExpr where
  dmnPU r = xpIgnoredAttr "label" . wrapIso _TExpr $ (dmnPU r)

data ExpressionLanguage = ExpressionLanguage String -- xsd:anyURI
  deriving (Show, Eq)

makePrisms ''ExpressionLanguage
instance DmnPU ExpressionLanguage where
  dmnPU r = xpAttr "expressionLanguage" $ wrapIso _ExpressionLanguage xpText

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
instance DmnPU TLiteralExpression where
  dmnPU r =
    wrapIso _TLiteralExpression $
      xpTriple
        (dmnPU r)                                    -- TExpr: id/typeRef attrs
        (xpOption (dmnPU r))                         -- expressionLanguage attr
        (xpSeq' (xpDmnAnnotations r) (xpOption (dmnPU r)))  -- <text>?

-- | @tUnaryTests@ (used by @<inputValues>@, @<outputValues>@, @<inputEntry>@).
-- Same shape as a literal expression but @<text>@ is required.
data UnaryTestsBody = UnaryTestsBody
  { utExpr :: TExpr
  , utExpressionLanguage :: Maybe ExpressionLanguage
  , utText :: TextElement
  }
  deriving (Show, Eq)

makePrisms ''UnaryTestsBody

instance DmnPU UnaryTestsBody where
  dmnPU r =
    wrapIso _UnaryTestsBody $
      xpTriple (dmnPU r) (xpOption (dmnPU r)) (xpSeq' (xpDmnAnnotations r) (dmnPU r))

-- | @tInputClause/inputValues@ — the declared domain of an input column.
newtype InputValues = InputValues UnaryTestsBody
  deriving (Show, Eq)

makePrisms ''InputValues

instance DmnPU InputValues where
  dmnPU r = xpDMNElem r "inputValues" _InputValues (dmnPU r)

-- | @tOutputClause/outputValues@ — the declared domain of an output column.
-- Byte-identically unmodelled before this change, which is why a fix aimed only
-- at @defaultOutputEntry@ would have left it broken.
newtype OutputValues = OutputValues UnaryTestsBody
  deriving (Show, Eq)

makePrisms ''OutputValues

instance DmnPU OutputValues where
  dmnPU r = xpDMNElem r "outputValues" _OutputValues (dmnPU r)

-- | @tItemDefinition/allowedValues@ — the declared domain of a NAMED type.
--
-- The third @tUnaryTests@ user, and byte-identical in shape to the two above.
-- That is the whole point: an @\<allowedValues\>@ is an @\<inputValues\>@ that
-- has been given a name and can be referred to from more than one column.
newtype AllowedValues = AllowedValues UnaryTestsBody
  deriving (Show, Eq)

makePrisms ''AllowedValues

instance DmnPU AllowedValues where
  dmnPU r = xpDMNElem r "allowedValues" _AllowedValues (dmnPU r)

unAllowedValues :: AllowedValues -> UnaryTestsBody
unAllowedValues (AllowedValues b) = b

-- | The text of @\<itemDefinition\>\<typeRef\>@. Note this is an ELEMENT here,
-- unlike the @typeRef@ ATTRIBUTE that 'TypeRef' models on a column — same name,
-- different XSD position, so they cannot share a pickler.
newtype ItemTypeRef = ItemTypeRef String
  deriving (Show, Eq)

makePrisms ''ItemTypeRef

instance DmnPU ItemTypeRef where
  dmnPU r = xpDMNElem r "typeRef" _ItemTypeRef xpText0

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
    -- ^ @isCollection@, raw. 'DMN.XML.XmlToDmnmd.isCollectionOf' reads it (an
    -- absent attribute and an explicit @"false"@ alike, per the XSD default) and
    -- 'resolveTypeRef' wraps the resolved base in 'DMN.Types.DMN_List'.
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

instance DmnPU ItemDefinition where
  dmnPU r =
    xpDMNElem r "itemDefinition" _ItemDefinition
      . xpFilterAttr (hasNameWith ((`elem` itemDefAttrs) . localPart))
      . xpFilterCont (hasNameWith ((`elem` itemDefElems) . localPart))
      $ xp6Tuple
          (xpOption (xpAttr "name" xpText))
          (xpOption (xpAttr "isCollection" xpText))
          (xpOption (dmnPU r))
          (xpOption (dmnPU r))
          (xpPeekedElems r "itemComponent" "name")
          (xpIgnoredElemOpt r "functionItem")

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

instance DmnPU DefaultOutputEntry where
  dmnPU r = xpDMNElem r "defaultOutputEntry" _DefaultOutputEntry (dmnPU r)

data LiteralExpression = LiteralExpression TLiteralExpression
  deriving (Show, Eq)

makePrisms ''LiteralExpression

instance DmnPU LiteralExpression where
  dmnPU r =
    xpDMNElem r "literalExpression" _LiteralExpression
      -- . xpFilterAttr (hasName "id" <+> hasName "name")
      -- . xpFilterCont none -- TODO
      $ (dmnPU r)


data InputExpression = InputExpression TLiteralExpression
  deriving (Show, Eq)

makePrisms ''InputExpression

instance DmnPU InputExpression where
  dmnPU r = xpDMNElem r "inputExpression" _InputExpression (dmnPU r)

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

instance DmnPU TableInput where
  dmnPU r =
    xpDMNElem r "input" _TableInput
      $ xp4Tuple
          (dmnPU r)
          (xpOption (dmnPU r))
          (xpSeq' (xpDmnAnnotations r) (dmnPU r))
          (xpOption (dmnPU r))

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

instance DmnPU TableOutput where
  dmnPU r =
    xpDMNElem r "output" _TableOutput
      $ xp5Tuple
          (dmnPU r)
          (xpOption (dmnPU r))
          (xpOption (dmnPU r))
          (xpSeq' (xpDmnAnnotations r) (xpOption (dmnPU r)))
          (xpOption (dmnPU r))

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
instance DmnPU InputEntry where
  dmnPU r =
    xpDMNElem r "inputEntry" _InputEntry
      . xpIgnoredAttrs ["label", "typeRef", "expressionLanguage"]
      $ xpPair (dmnPU r) (xpSeq' (xpDmnAnnotations r) (dmnPU r))

data OutputEntry = OutputEntry
  { outputEntryLabel :: DmnCommon
  , outputEntryText :: TextElement
  }
  deriving (Show, Eq)

makePrisms ''OutputEntry

-- | @tLiteralExpression@ in the @<outputEntry>@ position.
instance DmnPU OutputEntry where
  dmnPU r =
    xpDMNElem r "outputEntry" _OutputEntry
      . xpIgnoredAttrs ["label", "typeRef", "expressionLanguage"]
      $ xpPair (dmnPU r) (xpSeq' (xpDmnAnnotations r) (dmnPU r))

-- | @tRuleAnnotation@ (XSD line 352): the @<annotationEntry>@ that hangs off a
-- @<rule>@. DMN 1.3's own spelling of a row comment, so 'DMN.XML.XmlToDmnmd'
-- carries it straight into 'DMN.Types.row_comments'. @<text>@ is @minOccurs=0@.
newtype AnnotationEntry = AnnotationEntry (Maybe TextElement)
  deriving (Show, Eq)

makePrisms ''AnnotationEntry

instance DmnPU AnnotationEntry where
  dmnPU r = xpDMNElem r "annotationEntry" _AnnotationEntry $ xpOption (dmnPU r)

-- | The text of an @<annotationEntry>@, or @""@ if it had no @<text>@ child.
annotationEntryText :: AnnotationEntry -> String
annotationEntryText (AnnotationEntry mt) = maybe "" innerText mt

-- | @tRuleAnnotationClause@ (XSD line 338): the column header for one
-- @<annotationEntry>@ position. Carries a @name@ and nothing else.
newtype AnnotationClause = AnnotationClause (Maybe String)
  deriving (Show, Eq)

makePrisms ''AnnotationClause

instance DmnPU AnnotationClause where
  dmnPU r =
    xpDMNElem r "annotation" _AnnotationClause . xpFilterCont none $
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
instance DmnPU Rule where
  dmnPU r =
    xpDMNElem r "rule" _Rule
      . xpIgnoredAttr "label"
      $ xp5Tuple
          (dmnPU r)
          (xpOption (dmnPU r))
          (xpSeq' (xpExtensionElements r) (dmnPU r))
          (dmnPU r)
          (dmnPU r)

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
instance DmnPU DecisionTable where
  dmnPU r =
    xpDMNElem r "decisionTable" _DecisionTable
      . xpIgnoredAttrs ["label", "typeRef", "preferredOrientation", "outputLabel"]
      $ xp6Tuple
        (dmnPU r)
        xpHitPolicy
        (xpSeq' (xpDmnAnnotations r) (dmnPU r))
        (xpList1 (dmnPU r))
        (dmnPU r)
        (dmnPU r)

data Expression = ExprDTable DecisionTable | ExprLiteral LiteralExpression
  deriving (Show, Eq)

exprNr :: Expression -> Int
exprNr (ExprDTable _) = 0
exprNr (ExprLiteral _) = 0

instance DmnPU Expression where
  dmnPU r =
    xpAlt
      exprNr
      [ xpWrap (ExprDTable, \(ExprDTable x) -> x) (dmnPU r)
      , xpWrap (ExprLiteral, \(ExprLiteral x) -> x) (dmnPU r)
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
instance DmnPU Decision where
  dmnPU r =
    xpDMNElem r "decision" _Decision
      . xpIgnoredAttr "label"
      $ xpTriple
          (dmnPU r)
          (xpSeq' decisionPrelude (dmnPU r))
          (xpSeq' decisionInterlude (dmnPU r))
    where
      decisionPrelude =
        xpWrap (const (), const ((), ((), ((), ())))) $
          xpPair
            (xpDmnAnnotations r)
            (xpPair
              (xpIgnoredElemOpt r "question")
              (xpPair (xpIgnoredElemOpt r "allowedAnswers") (xpIgnoredElemOpt r "variable")))
      decisionInterlude =
        xpIgnoredElemsOf r
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

xpVariable :: DmnRelease -> PU InformationItem
xpVariable r =
  xpDMNElem r "variable" _InformationItem
    . xpIgnoredAttr "label"
    $ xpPair (dmnPU r) (xpSeq' (xpDmnAnnotations r) (xpOption (dmnPU r)))

instance DmnPU InputData where
  dmnPU r =
    xpDMNElem r "inputData" _InputData
      . xpIgnoredAttr "label"
      $ xpPair (dmnPU r) (xpSeq' (xpDmnAnnotations r) (xpOption (xpVariable r)))

-- | @tKnowledgeSource@: provenance metadata, not decision logic. Consumed
-- wholesale.
data KnowledgeSource = KnowledgeSource
  { knsLabel :: DmnNamed
  }
  deriving (Show, Eq)

makePrisms ''KnowledgeSource

instance DmnPU KnowledgeSource where
  dmnPU r =
    xpDMNElem r "knowledgeSource" _KnowledgeSource
      . xpIgnoredAttrs ["label", "locationURI"]
      . xpFilterCont none
      $ (dmnPU r)

data Namespace = Namespace { namespace :: String }
  deriving (Show, Eq)

makePrisms ''Namespace

instance DmnPU Namespace where
  dmnPU r = wrapIso _Namespace $ xpAttr "namespace" xpText

data DrgElems = DrgDec Decision | DrgInpData InputData | DrgKS KnowledgeSource
  deriving (Show, Eq)

drgNr :: DrgElems -> Int
drgNr (DrgDec _) = 0
drgNr (DrgInpData _) = 1
drgNr (DrgKS _) = 2

instance DmnPU DrgElems where
  dmnPU r =
    xpAlt
      drgNr
      [ xpWrap (DrgDec, \(DrgDec x) -> x) (dmnPU r)
      , xpWrap (DrgInpData, \(DrgInpData x) -> x) (dmnPU r)
      , xpWrap (DrgKS, \(DrgKS x) -> x) (dmnPU r)
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
      defItemDefs = [],
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
--
-- The document is matched in exactly ONE release's namespace — the caller's.
-- That is deliberate: a @\<definitions\>@ declaring DMN 1.5 whose
-- @\<decisionTable\>@ is in the 1.3 namespace is not a document any DMN
-- validator accepts, and reading it would be inventing a dialect. Accepting any
-- of several namespaces at every position independently would do exactly that.
--
-- @expressionLanguage@ and @typeLanguage@ are consumed and dropped here, which
-- is the whole reason @tDefinitions@ needs no per-release treatment: it is the
-- one shared complex type that differs between 1.3 and 1.5, and it differs only
-- in those two attributes' schema DEFAULT values (@…\/20191111\/FEEL\/@ becomes
-- @…\/20230324\/FEEL\/@).
dmnPickler :: DmnRelease -> PU XDMN
dmnPickler r =
  xpDMNElem r "definitions" _Definitions
    . xpIgnoredAttrs ["label", "expressionLanguage", "typeLanguage", "exporter", "exporterVersion"]
    $ xp7Tuple
        (dmnPU r)                                  -- id / name attributes
        (dmnPU r)                                  -- namespace attribute
        (xpSeq' definitionsPrelude (dmnPU r))       -- [ItemDefinition]; see defItemDefs
        (dmnPU r)                                  -- [InputData]
        (dmnPU r)                                  -- [Decision]
        (dmnPU r)                                  -- [DrgElems]
        (xpSeq' definitionsEpilogue (dmnPU r))     -- Maybe DMNDI
  where
    definitionsPrelude =
      xpWrap (const (), const ((), ())) $
        xpPair
          (xpDmnAnnotations r)
          (xpIgnoredElems r "import")
    definitionsEpilogue =
      xpIgnoredElemsOf r
        [ "association", "textAnnotation"          -- artifact
        , "elementCollection"
        , "performanceIndicator", "organizationUnit" -- businessContextElement
        ]

--     <variable id="InformationItem_1mjp1b5" name="safe price" typeRef="double" />

-- $> :m + Text.Pretty.Simple

-- $> :set -interactive-print pPrint

--- $> :i _Definitions
-- _Definitions :: L.Iso' Definitions (String, String, DMNDI)

-- | The one surviving hxt 'XmlPickler' instance, and it exists only so that the
-- dev-scratch 'showEx3' below can call 'showPickled', whose signature demands
-- the class. It is pinned to 1.3 because that is a PICKLING direction and
-- @--to=xml@ is unimplemented; when it lands, \"which release does dmnmd emit?\"
-- becomes a real question and this is where it will be answered.
instance XmlPickler Definitions where
  xpickle = dmnPickler dmn13

-- . xpAddNSDecl "qw4" "nope"

pickleConfig :: [SysConfig]
pickleConfig = [withValidate no, withCheckNamespaces yes, withRemoveWS yes, withIndent yes]

-- | Read a DMN file, reporting *why* it could not be read.
--
-- 'Text.XML.HXT.Arrow.Pickle.xunpickleDocument' swallows unpickling failures:
-- it prints @fatal error: document unpickling failed@ and yields an empty list,
-- which is indistinguishable from a valid file containing nothing. Callers then
-- reported success. Here a failure is a 'Left' that the caller has to deal with.
--
-- The release is resolved ONCE, from the root element's namespace, and then
-- threaded: it decides which namespace the picklers match, which namespace
-- counts as foreign for attribute stripping, and what every diagnostic below
-- calls this document.
parseDMNEither :: FilePath -> IO (Either String [XDMN])
parseDMNEither filename = do
  roots <- runX $ readDocument pickleConfig filename >>> getChildren >>> isElem
  pure $ case roots of
    [] ->
      Left $ filename ++ ": no XML root element found"
          ++ " (the file is missing, empty, or not well-formed XML)."
    (raw : _) -> do
      release <- checkDmnRoot filename raw
      refuseUnmodelled filename release raw
      root <- maybe
                (Left (filename ++ ": root element vanished under attribute stripping."))
                Right
                (listToMaybe (runLA (stripIgnorableAttrs release) raw))
      case unpickleDoc' (dmnPickler release) root of
        Left msg -> Left $ filename ++ ": " ++ readerRefusal release root ++ "\n" ++ msg
        Right v  -> Right [v]

-- | Elements dmnmd does not model, refused by NAME before unpickling starts.
--
-- Each entry is @(local name, (release that introduced it, what it is))@.
--
-- These are the five boxed expressions DMN 1.4 added — @\<conditional\>@,
-- @\<for\>@, @\<some\>@, @\<every\>@, @\<filter\>@, all
-- @substitutionGroup=\"expression\"@ — plus @\<typeConstraint\>@, the single
-- structural addition DMN 1.5 made over 1.4. __Five spellable names, not seven
-- complex types.__
--
-- The reason is narrower than an earlier version of this comment claimed, and
-- that version shipped to three other files before anyone checked it. It said
-- @tIterator@ and @tQuantified@ are abstract bases. They are not: __no__
-- complexType in @DMN15.xsd@ carries @abstract=\"true\"@ (all seven occurrences
-- of that attribute are on /element/ declarations, e.g. the substitution-group
-- head @\<xsd:element name=\"expression\" abstract=\"true\"\/\>@ at :223), and
-- @tQuantified@ has __two__ global elements — @\<xsd:element name=\"every\"@ and
-- @name=\"some\"@ at @DMN15.xsd:558-559@ — both of which the list above already
-- refuses.
--
-- The true reason three of the four extra types are unspellable is simply that
-- @tIterator@, @tChildExpression@ and @tTypedChildExpression@ have no global
-- @\<xsd:element\>@ declaration. The last two are the types of the named children
-- @in@\/@return@\/@satisfies@\/@if@\/@then@\/@else@\/@match@, which are declared
-- only /inside/ the five parents above and so are unreachable in a valid document
-- without one of them. A fixture for @\<iterator\>@ would test nothing.
--
-- None of the six names is declared anywhere in @DMN13.xsd@, so adding this
-- check cannot make a document that reads today start failing — except the
-- @\<typeConstraint\>@ case, which is the intended change and is recorded in
-- @test\/corpus\/cases\/symptom\/xml-typeconstraint-dropped-silently@ as it
-- behaved before.
--
-- The list is the extension point. The DMN /1.3/ boxed expressions dmnmd has
-- never modelled — @\<context\>@, @\<invocation\>@, @\<relation\>@,
-- @\<list\>@, @\<functionDefinition\>@ — belong here too and would turn a
-- generic @xpCheckEmptyContents@ into a named refusal, but they are a separate
-- change: adding them alters recordings this one must leave untouched.
unmodelledConstructs :: [(String, (String, String))]
unmodelledConstructs =
  [ ("conditional",    ("DMN 1.4", "a boxed conditional (if / then / else)"))
  , ("for",            ("DMN 1.4", "a boxed iterator (for / in / return)"))
  , ("some",           ("DMN 1.4", "a boxed quantifier (some / in / satisfies)"))
  , ("every",          ("DMN 1.4", "a boxed quantifier (every / in / satisfies)"))
  , ("filter",         ("DMN 1.4", "a boxed filter (in / match)"))
  , ("typeConstraint", ("DMN 1.5", "a unary test constraining an <itemDefinition>'s values"))
  ]

-- | Refuse the constructs in 'unmodelledConstructs' by name, before unpickling.
--
-- __Why a pre-flight rather than a refusal arm inside the picklers.__ Three
-- reasons, each sufficient on its own.
--
--  1. @\<typeConstraint\>@ never reaches a pickler at all. 'ItemDefinition'
--     applies @xpFilterCont@ with a NAME FILTER, which DELETES every child not
--     in 'itemDefElems', so the element is gone before @xpCheckEmptyContents@
--     could notice it — it is dropped silently, which is worse than the generic
--     message, and no in-pickler arm can see it.
--  2. All five boxed expressions substitute for @expression@, which
--     @DMN15.xsd@ writes in seven places (@tDecision@, @tInvocation@,
--     @tBinding@, @tContextEntry@, @tFunctionDefinition@, @tList@,
--     @tChildExpression@). dmnmd models exactly one of them, 'decDTable', so at
--     the other six there is no pickler to hang a refusal on.
--  3. It needs nothing from hxt beyond 'runLA' — no @throwMsg@ or
--     @liftUnpickleVal@ from @Text.XML.HXT.Arrow.Pickle.Xml@, which
--     @Text.XML.HXT.Core@ does not re-export.
--
-- __'multi', not 'deep'__: hxt's 'deep' stops at the first success on each
-- branch (@deep f = f \`orElse\` (getChildren >>> deep f)@), and the root
-- @\<definitions\>@ is itself an element, so @deep isElem@ returns the root and
-- nothing else. 'multi' is the one that visits every descendant.
--
-- The scan is confined to the document's own model namespace, so an element of
-- the same name inside @\<extensionElements\>@ — declared
-- @\<xsd:any namespace=\"##other\"\/\>@, which the standard says a consumer may
-- ignore — is not touched.
refuseUnmodelled :: FilePath -> DmnRelease -> XmlTree -> Either String ()
refuseUnmodelled filename release root
  | null problems = Right ()
  | otherwise =
      Left . intercalate "\n" $
        (filename ++ ": this document declares " ++ relName release
           ++ " and dmnmd cannot represent it faithfully.")
          : map ("  " ++) problems
          ++ [ "dmnmd models decision tables: a <decision> must hold a"
                 ++ " <decisionTable>, and an <itemDefinition> may carry"
                 ++ " <allowedValues> but not <typeConstraint>."
             | not (null unmodelled)
             ]
  where
    problems = nub (map renderUnmodelled unmodelled ++ map renderStrayNS strayNS)

    -- Constructs we have no representation for, wherever they appear.
    unmodelled =
      [ (owner kid, nm)
      | kid <- topLevel
      , nm <- runLA offenders kid
      ]
    offenders =
      multi (isElem >>> getQName)
        >>> isA ((== relModelNS release) . namespaceUri)
        >>> arr localPart
        >>> isA (`elem` map fst unmodelledConstructs)
    renderUnmodelled (who, nm) =
      case lookup nm unmodelledConstructs of
        Just (since, what) ->
          who ++ ": <" ++ nm ++ "> is " ++ what ++ ", added in " ++ since
            ++ ". dmnmd does not model it."
        Nothing -> who ++ ": <" ++ nm ++ "> is not modelled by dmnmd."

    -- A subtree in a DIFFERENT DMN release's namespace. No validator accepts a
    -- document that mixes them, and reading one would be inventing a dialect —
    -- so it is refused, but by name rather than as a bare "unprocessed XML
    -- content" a hundred lines later.
    --
    -- Only the OUTERMOST element of each stray subtree is reported. An @xmlns@
    -- is inherited by every descendant, so one misplaced declaration otherwise
    -- yields one line per element in the subtree — a dozen lines that all say
    -- the same thing about the same mistake. 'multi' is top-down, so the first
    -- hit for a given namespace is the shallowest.
    strayNS =
      [ (owner kid, nm, uri)
      | kid <- topLevel
      , (nm, uri) <- nubBy (\(_, u1) (_, u2) -> u1 == u2) (runLA strays kid)
      ]
    strays =
      multi (isElem >>> getQName)
        >>> arr (\qn -> (localPart qn, namespaceUri qn))
        >>> isA (\(_, uri) -> uri `notElem` ownNamespaces && isNameableDmnNS uri)

    -- The document's own two URIs. A release is a MODEL namespace *and* a DMNDI
    -- namespace, independently versioned — DMN 1.4 pairs MODEL 20211108 with
    -- DMNDI 20191111 — so "belongs to this document" is a two-element test, not
    -- a comparison against 'relModelNS' alone.
    ownNamespaces = [relModelNS release, relDmndiNS release]

    -- Any DMN namespace we can put a name to. Deliberately wider than
    -- 'readableReleases', on both axes:
    --
    --  * 'relDmndiNS' as well as 'relModelNS'. Bumping MODEL and forgetting
    --    DMNDI is exactly what a tool does by accident, and it used to land on
    --    the generic @xpCheckEmptyContents@ this scan exists to prevent.
    --  * 'refusedReleases' as well. A 1.5 document with a 1.2 @\<decision\>@
    --    subtree was reaching the generic message too, because the filter
    --    consulted only the readable list — while 'renderStrayNS' below already
    --    called 'releaseNameOfNamespace', which handles refused releases. The
    --    renderer was prepared for a URI the filter could never deliver, which
    --    is how the gap reads as an accident rather than a decision. It also
    --    /widened/ with every release added, since 'refusedReleases' stays at two.
    isNameableDmnNS uri =
      any (\r -> uri == relModelNS r || uri == relDmndiNS r) readableReleases
        || uri `elem` map fst refusedReleases
    renderStrayNS (who, nm, uri) =
      who ++ ": <" ++ nm ++ "> is in namespace " ++ show uri
        ++ maybe "" (\v -> " (" ++ v ++ ")") (releaseNameOfNamespace uri)
        ++ ", but this document declares " ++ relName release
        ++ ". dmnmd reads one release per document."

    -- Name the top-level DRG element the offender sits under, so the message is
    -- located rather than merely loud. Nesting deeper than that (a <conditional>
    -- inside a <contextEntry> inside a <decision>) still reports the decision,
    -- which is the unit a reader can go and look at.
    topLevel = runLA (getChildren >>> isElem) root
    owner kid =
      let ln = concat (runLA (getQName >>> arr localPart) kid)
       in case listToMaybe (runLA (getAttrValue0 "name") kid) of
            Just n -> ln ++ " " ++ show n
            Nothing -> "<" ++ ln ++ ">"

-- | Say what actually went wrong.
--
-- The document has already been confirmed to be a readable @<definitions>@ by
-- 'checkDmnRoot', so blaming the version — as this message used to, for every
-- unpickling failure whatsoever — was simply false. The commonest real cause is
-- a DRG element that DMN permits and dmnmd does not model
-- (@<businessKnowledgeModel>@, @<decisionService>@ …); name those when they are
-- present, and otherwise admit that we only have the unpickler's complaint.
--
-- The release is named from the document rather than hardcoded, so a DMN 1.5
-- file is not told it is a DMN 1.3 one.
readerRefusal :: DmnRelease -> XmlTree -> String
readerRefusal release root
  | not (null unmodelled) =
      "this is valid " ++ relName release ++ ", but it contains "
        ++ intercalate ", " (map (\n -> "<" ++ n ++ ">") unmodelled)
        ++ ", which dmnmd does not model. Only <decision>, <inputData> and"
        ++ " <knowledgeSource> are read."
  | otherwise = "dmnmd could not read this " ++ relName release ++ " document."
  where
    childNames = runLA (getChildren >>> isElem >>> getQName >>> arr localPart) root
    unmodelled = nub (filter (`elem` unmodelledDrgElements) childNames)

-- | Children of @<definitions>@ that the DMN XSD allows but this reader has
-- no representation for. Listing them is what lets 'readerRefusal' tell the
-- truth instead of guessing at the version.
unmodelledDrgElements :: [String]
unmodelledDrgElements =
  [ "businessKnowledgeModel", "decisionService" ]

-- | Identify the release, or refuse the document up front naming what we found.
-- Without this, a DMN 1.1 or 1.2 file (correctly refused) failed with
-- @xpElem: got element name \"...\"@ deep inside the pickler.
--
-- Widening from one readable release to three does not weaken this: a namespace
-- that is not in 'readableReleases' is still refused, and 'refusedReleases' is
-- what lets 1.1 and 1.2 be refused BY NAME rather than as an anonymous URI.
-- Before this change a DMN 1.5 document was not even told it was 1.5, because
-- the version table stopped at 1.3.
checkDmnRoot :: FilePath -> XmlTree -> Either String DmnRelease
checkDmnRoot filename root =
  case listToMaybe (runLA getQName root) of
    Nothing -> Left $ filename ++ ": XML root element has no name."
    Just qn
      | localPart qn /= "definitions" ->
          Left $ filename ++ ": expected a <definitions> root element, but found <"
              ++ qualifiedName qn ++ ">."
      | Just release <- releaseOfNamespace (namespaceUri qn) -> Right release
      | otherwise ->
          Left $ filename ++ ": <definitions> is in namespace "
              ++ show (namespaceUri qn) ++ versionNote
              ++ ".\ndmnmd reads " ++ readableList ++ "."
      where
        versionNote =
          maybe "" (\v -> " (" ++ v ++ ")") (releaseNameOfNamespace (namespaceUri qn))
        readableList =
          intercalate ", "
            [relName r ++ " (" ++ show (relModelNS r) ++ ")" | r <- readableReleases]

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
