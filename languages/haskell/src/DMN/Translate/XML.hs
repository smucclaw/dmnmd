{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- | @--to=xml@: dmnmd's decision tables as a DMN XML document.
--
-- Issue #13, ruled on as @DECISIONS.md@ D-8. This is the only backend whose
-- output is read by something other than a programming-language toolchain, and
-- that gives it a failure mode none of the others have: it can emit a
-- __well-formed, XSD-valid document that says the wrong thing__, and no
-- validator will catch it. Everything below is arranged around that.
--
-- == Three structural decisions
--
-- __1. The document is built as a "DMN.XML.ParseDMN" 'X.Definitions' and
-- serialised by the reader's own picklers, running backwards.__ hxt picklers
-- are bidirectional, so @dmnPickler@ — the function that reads DMN — is also
-- the function that writes it. A hand-rolled writer would be a second
-- description of the same element structure, free to drift from the reader's;
-- this one cannot drift, because there is only one description. What is left
-- for this module is a pure @DecisionTable -> Definitions@ mapping and the cell
-- language.
--
-- __2. dmnmd emits DMN 1.3, and the release is an 'XMLOpts' field rather than a
-- hardcoded URI.__ 1.3 because @xsd\/@ holds DMN 1.1, 1.2 and 1.3 and nothing
-- later, so 1.3 is the only release whose output this repository can actually
-- validate (measured: @test\/dmn15\/baseline15.dmn@ fails against
-- @xsd\/DMN13.xsd@); because the root @README.md@ has promised DMN 1.3 since
-- before there was an emitter; and because 1.3 is the release every reader
-- accepts. There is deliberately __no CLI flag__ to select another: emitting
-- 1.4 or 1.5 would be a capability nothing here can check, and D-4's whole
-- point was that a namespace is a parameter. The parameter is here, in
-- 'xmlRelease'; a flag is one line whenever a DMN14\/15 schema arrives.
--
-- __3. XML is emitted per FILE, not per table.__ A DMN document is one
-- @\<definitions\>@ carrying every decision, so 'toXMLFile' is the entry point
-- and @app\/Main.hs@ intercepts 'Options.Xml' before the per-table path, the
-- same way it does L4.
--
-- == What DMN cannot spell
--
-- dmnmd's markdown surface is not a subset of S-FEEL, and where the two differ
-- this module either translates faithfully and says so, or refuses. It never
-- emits a document that means something else in silence. See 'fidelityDiags'
-- for the list, and @DECISIONS.md@ D-11 for the one case whose promised
-- diagnostic turned out to be unpayable here.
module DMN.Translate.XML
  ( XMLOpts (..)
  , defaultXMLOpts
  , toXMLFile
  , toXMLDoc
    -- * Exposed for the test suite
  , showFeelXML
  , cellText
  ) where

import Data.Char (isAlphaNum, isDigit, toLower)
import Data.List (intercalate, nub)
import Data.Maybe (mapMaybe)
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.Edit (escapeXmlRefs)
import qualified Text.XML.HXT.DOM.ShowXml as SX

import DMN.Diagnostic
import DMN.Number (showNumPlain)
import DMN.Types
import qualified DMN.XML.ParseDMN as X

-- * Options

data XMLOpts = XMLOpts
  { xmlRelease :: X.DmnRelease
    -- ^ Which DMN release to write. See decision 2 in the module header.
  , xmlDocName :: String
    -- ^ @\<definitions name=…\>@. Required by the XSD (@tNamedElement@) and by
    -- dmnmd's own reader.
  , xmlTargetNS :: String
    -- ^ @\<definitions namespace=…\>@, required by the XSD. This is the
    -- document's OWN target namespace — the thing @href@s and imports would
    -- resolve against — not the DMN model namespace, which is 'xmlRelease'\'s.
  }

defaultXMLOpts :: XMLOpts
defaultXMLOpts = XMLOpts
  { xmlRelease = X.dmn13
  , xmlDocName = "dmnmd"
  , xmlTargetNS = "https://github.com/smucclaw/dmnmd"
  }

-- * Entry point

-- | Every table as one DMN document, plus everything worth saying about the
-- translation.
--
-- An 'Error' means a table could not be written faithfully, and then __nothing
-- at all is emitted__ — @app\/Main.hs@ enforces that, because a DMN document
-- missing one of the decisions it was asked to carry is indistinguishable from
-- a complete one.
toXMLFile :: XMLOpts -> [DecisionTable] -> ([Diagnostic], String)
toXMLFile opts dts
  | anyErrors diags = (diags, "")
  | otherwise = (diags, toXMLDoc opts dts)
  where diags = concatMap (fidelityDiags opts) dts

-- | Serialise, with no diagnostics and no refusal. Split out so a test can ask
-- for the document text alone.
toXMLDoc :: XMLOpts -> [DecisionTable] -> String
toXMLDoc opts dts = render (xmlRelease opts) (definitionsOf opts dts)

-- * Serialisation

-- | Pickle a 'X.Definitions' and print it.
--
-- __The @xmlns@ has to be added after pickling, as an ordinary attribute.__
-- @xpElemNS@ builds a UNIVERSAL name, and hxt's writer serialises the QUALIFIED
-- one — which, at prefix @\"\"@, is a bare @\<definitions\>@ with no namespace
-- declaration at all. dmnmd's own reader then refuses the result with
-- @\<definitions\> is in namespace \"\"@. Measured: neither @xpAddNSDecl@ nor
-- @xpAddFixedAttr \"xmlns\"@ survives into the output; adding the attribute to
-- the root element after 'pickleDoc' does.
--
-- Everything here is pure. @indentDoc@ and @addAttr@ are hxt ARROWS, but a list
-- arrow (@runLA@) needs no IO, so a backend stays a @… -> String@ function like
-- every other one.
render :: X.DmnRelease -> X.Definitions -> String
render r defs =
  xmlDecl ++ SX.xshow (runLA (declareNS >>> escapeTree >>> indentDoc >>> getChildren) tree)
  where
    tree = pickleDoc (X.dmnPickler r) defs
    declareNS = processChildren (addAttr "xmlns" (X.relModelNS r) `when` isElem)
    xmlDecl = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

-- | Escape text and attribute values, because 'SX.xshow' does __not__.
--
-- @xshow@ is hxt's tree printer, not its document writer: it renders
-- @\<text\>\<= 0\</text\>@ verbatim, producing a file that is not well-formed
-- XML at all. Every table with a @\<= 0@ cell — and @miles-card-dmn.md@ has
-- eight — came out unparseable, by xmllint and by dmnmd's own reader.
--
-- hxt's writer escapes in an @IOStateArrow@, which would make this backend
-- @IO@; 'escapeXmlRefs' is the same escaping table it uses, exposed as a pair
-- of pure functions (text, attribute value), so the tree is escaped in place
-- and the backend stays a @… -> String@ function.
escapeTree :: ArrowXml a => a XmlTree XmlTree
escapeTree = processTopDown $
  (changeText (escapeWith textEsc) `when` isText)
    >>> (processAttrl (changeAttrValue (escapeWith attrEsc)) `when` isElem)
  where
    (textEsc, attrEsc) = escapeXmlRefs
    escapeWith f = concatMap (\c -> f c "")

-- * The mapping

-- | The whole document.
--
-- Order matters and is not cosmetic: 'X.Definitions' splits the XSD's one
-- @drgElement*@ run greedily into leading @\<inputData\>@, then leading
-- @\<decision\>@, then a mixture. Writing all the inputData first and all the
-- decisions second is what lets the same pickler read this document back.
definitionsOf :: XMLOpts -> [DecisionTable] -> X.Definitions
definitionsOf opts dts = X.Definitions
  { X.defLabel = X.dmnNamed' "definitions_dmnmd" (xmlDocName opts)
  , X.defsNamespace = X.Namespace (xmlTargetNS opts)
  , X.defItemDefs = collectionItemDefs dts
  , X.defInputData = map inputDataOf (documentInputs dts)
  , X.defsDescisions = zipWith decisionOf [1 ..] dts
  , X.defDrgElems = []
  , X.defDMNDI = Nothing
    -- ^ Diagram interchange is geometry dmnmd does not have. The reader models
    -- 'X.DMNDI' as a nullary constructor that pickles to @\<dmndi:DMNDI\/\>@
    -- with an UNDECLARED prefix, so emitting one would produce a document
    -- neither dmnmd nor anything else can read.
  }
  where
    documentInputs = nub . concatMap tableInputVars
    tableInputVars dt = [ (varname ch, vartype ch) | ch <- inHeaders dt ]

    inputDataOf (nm, ty) = X.InputData
      { X.inpLabel = X.dmnNamed' (idOf "inputData" [slug nm]) nm
      , X.inpVariable = Just X.InformationItem
          { X.iiLabel = X.dmnNamed' (idOf "informationItem" [slug nm]) nm
          , X.iiTypeRef = X.TypeRef <$> typeRefOf ty
          }
      }

-- | One table as a @\<decision\>@ wrapping a @\<decisionTable\>@.
--
-- The @\<informationRequirement\>@ edges are not invented: an input column IS
-- the statement that this decision reads that input, which is exactly what the
-- DRG edge means. dmnmd's own reader parses them into @decInfoReq@ and then
-- ignores them (D-6), so a round trip cannot check them — that is not a reason
-- to leave them out, because it is what makes the output usable in a real DMN
-- tool.
decisionOf :: Int -> DecisionTable -> X.Decision
decisionOf t dt = X.Decision
  { X.decLabel = X.dmnNamed' (idOf "decision" [show t]) (tableName dt)
  , X.decInfoReq =
      [ X.InformationRequirement
          { X.infrLabel = X.DmnCommon (Just (idOf "informationRequirement" [show t, show c])) Nothing
          , X.infrReq = X.RequiredInput
          , X.infoHref = X.Href ("#" ++ idOf "inputData" [slug (varname ch)])
          }
      | (c, ch) <- zip [1 :: Int ..] (inHeaders dt)
      ]
  , X.decDTable = Just (X.ExprDTable (decisionTableOf t dt))
  }

decisionTableOf :: Int -> DecisionTable -> X.DecisionTable
decisionTableOf t dt = X.DecisionTable
  { X.dtLabel = X.DmnCommon (Just (idOf "decisionTable" [show t])) Nothing
  , X.dtHitPolicy = hitpolicy dt
  , X.dtInput = zipWith (inputClauseOf t) [1 ..] (inHeaders dt)
  , X.dtOutput = zipWith (outputClauseOf t) [1 ..] (outHeaders dt)
  , X.dtAnnotations =
      [ X.AnnotationClause (Just (varname ch)) | ch <- commentHeaders dt ]
  , X.dtRules = zipWith (ruleOf t dt) [1 ..] (allrows dt)
  }

-- | @\<input\>@.
--
-- The column's name goes in @\<inputExpression\>\<text\>@ — the FEEL expression
-- whose value the column tests — and is repeated on @label@. That is not
-- belt-and-braces: 'DMN.XML.XmlToDmnmd.convInputCol' reads the name from the
-- @\<text\>@ first and @\@label@ second, so the two must agree or a round trip
-- silently renames the column.
--
-- __No @\@name@ anywhere on an input.__ @tInputClause@ extends @tDMNElement@,
-- which has @id@ and @label@ and no @name@; 'X.DmnCommon' pickles a @name@
-- attribute, so its second field is 'Nothing' here and on every literal
-- expression. Only @tOutputClause@ genuinely has @\@name@.
inputClauseOf :: Int -> Int -> ColHeader -> X.TableInput
inputClauseOf t c ch = X.TableInput
  { X.tinpName = X.DmnCommon (Just (idOf "input" [show t, show c])) Nothing
  , X.tinpLabel = Just (X.ColumnLabel (varname ch))
  , X.tinpExpr = X.InputExpression X.TLiteralExpression
      { X.tleExpr = X.TExpr
          { X.exprLabel = X.DmnCommon (Just (idOf "inputExpression" [show t, show c])) Nothing
          , X.exprTypeRef = X.TypeRef <$> typeRefOf (vartype ch)
          }
      , X.tleExpressionLanguage = Nothing
      , X.tleContent = Just (X.TextElement (varname ch))
      }
  , X.tinpValues = X.InputValues <$> unaryTestsOf ch
  }

-- | @\<output\>@. @\@name@ and @\@label@ both carry the column name: the reader
-- prefers @label@, other DMN tools prefer @name@, and a document where they
-- disagree is a document that renames a column depending on who reads it.
outputClauseOf :: Int -> Int -> ColHeader -> X.TableOutput
outputClauseOf t c ch = X.TableOutput
  { X.toutName = X.DmnCommon (Just (idOf "output" [show t, show c])) (Just (varname ch))
  , X.toutLabel = Just (X.ColumnLabel (varname ch))
  , X.toutTypeRef = X.TypeRef <$> typeRefOf (vartype ch)
  , X.toutValues = X.OutputValues <$> unaryTestsOf ch
  , X.toutDefault = Nothing
    -- ^ dmnmd has no slot for a default output — the reader warns that it drops
    -- one — so there is never anything to write here. An OTHERWISE synthesised
    -- by a backend belongs to that backend, not to the table.
  }

-- | A column's declared domain — the markdown sub-header row — as
-- @\<inputValues\>@ \/ @\<outputValues\>@. DMN's own spelling for the same idea,
-- and the reason hit policies O and P survive a round trip at all, since
-- @outputOrder@ reads @enums@.
unaryTestsOf :: ColHeader -> Maybe X.UnaryTestsBody
unaryTestsOf ch = case enums ch of
  Nothing -> Nothing
  Just [] -> Nothing
  Just fs -> Just X.UnaryTestsBody
    { X.utExpr = X.TExpr (X.DmnCommon Nothing Nothing) Nothing
    , X.utExpressionLanguage = Nothing
    , X.utText = X.TextElement (cellText ch fs)
    }

-- | One row as a @\<rule\>@.
--
-- Row comments split across two XML shapes because that is how the reader reads
-- them back: 'DMN.XML.XmlToDmnmd' builds @row_comments@ as
-- @description : annotationEntries@. So the comment columns become
-- @\<annotationEntry\>@s, in order, matching the @\<annotation\>@ clauses
-- declared on the table, and @\<description\>@ is left out — dmnmd has no
-- table-level place to put one back.
ruleOf :: Int -> DecisionTable -> Int -> DTrow -> X.Rule
ruleOf t dt r row = X.Rule
  { X.ruleLabel = X.DmnCommon (Just (idOf "rule" [show t, show r])) Nothing
  , X.ruleDescription = Nothing
  , X.ruleInputEntry =
      [ X.InputEntry
          { X.ieLabel = X.DmnCommon (Just (idOf "inputEntry" [show t, show r, show c])) Nothing
          , X.ieText = X.TextElement (cellText ch cell)
          }
      | (c, ch, cell) <- zip3 [1 :: Int ..] (inHeaders dt) (row_inputs row)
      ]
  , X.ruleOutputEntry =
      [ X.OutputEntry
          { X.outputEntryLabel = X.DmnCommon (Just (idOf "outputEntry" [show t, show r, show c])) Nothing
          , X.outputEntryText = X.TextElement (cellText ch cell)
          }
      | (c, ch, cell) <- zip3 [1 :: Int ..] (outHeaders dt) (row_outputs row)
      ]
  , X.ruleAnnotations =
      [ X.AnnotationEntry (X.TextElement <$> mtext)
      | mtext <- take (length (commentHeaders dt)) (row_comments row)
      ]
  }

-- * Types

-- | A dmnmd column type as a DMN @typeRef@.
--
-- Three built-in spellings, and a synthesised @\<itemDefinition\>@ for a
-- collection — because __DMN has no column-level spelling for a collection at
-- all__. @isCollection@ is an @\<itemDefinition\>@ attribute; there is no such
-- attribute on @tInputClause@, @tOutputClause@ or @tLiteralExpression@. So a
-- @tags : [Number]@ column forces the emitter to declare a named type. That is
-- the one element in the document with no markdown counterpart, and it is
-- invention only in its NAME — the isCollection flag and the base type are both
-- read straight off the column.
--
-- An untyped column (all wildcards; 'DMN.DecisionTable.columnVerdict' says
-- @VNone@) gets no @typeRef@ at all, which is what the XSD's @use=\"optional\"@
-- is for and what the reader reads back as untyped.
typeRefOf :: Maybe DMNType -> Maybe String
typeRefOf Nothing = Nothing
typeRefOf (Just t) = Just (typeRefName t)

typeRefName :: DMNType -> String
typeRefName DMN_String = "string"
typeRefName DMN_Number = "number"
typeRefName DMN_Boolean = "boolean"
typeRefName (DMN_List t) = collectionTypeName t

-- | The name of the synthesised collection type. Not user-supplied and not
-- derived from a column name, so it cannot collide with a user's own type
-- coming the other way: there are exactly three of them per document.
collectionTypeName :: DMNType -> String
collectionTypeName t = "dmnmd_list_of_" ++ map toLower (typeRefName t)

-- | The @\<itemDefinition\>@s the document needs, one per distinct collection
-- element type actually used.
--
-- A nested @[[T]]@ cannot reach here: 'DMN.DecisionTable.structuralErrors' R1
-- refuses it for both readers, so the recursive case is named rather than
-- silently flattened.
collectionItemDefs :: [DecisionTable] -> [X.ItemDefinition]
collectionItemDefs dts =
  [ X.ItemDefinition
      { X.itdName = Just (collectionTypeName t)
      , X.itdIsCollection = Just "true"
      , X.itdTypeRef = Just (X.ItemTypeRef (typeRefName t))
      , X.itdAllowedValues = Nothing
      , X.itdComponents = []
      , X.itdFunctionItem = ()
      }
  | t <- nub (concatMap elemTypesOf dts)
  ]
  where
    elemTypesOf dt = [ t | ch <- header dt, Just (DMN_List t) <- [vartype ch] ]

-- * The cell language

-- | One cell — the whole @\<inputEntry\>\<text\>@ or @\<outputEntry\>\<text\>@.
--
-- A cell is a LIST of 'FEELexp' (DMN 1.3 §9.2 rule 11's comma-separated
-- disjunction), and the list is rendered by joining with @\", \"@, which is
-- what the reader splits on.
cellText :: ColHeader -> [FEELexp] -> String
cellText ch fs = intercalate ", " (showFeelXML (scalarType ch) <$> fs)
  where
    -- A collection column's cells are parsed and rendered at the ELEMENT type;
    -- the list-ness lives in the column type, exactly as it does on the way in.
    scalarType c = case vartype c of
      Just (DMN_List t) -> Just t
      other -> other

-- | One FEEL expression as DMN would spell it, given the column's scalar type.
--
-- The differences from 'DMN.DecisionTable.showDomainMember', which renders the
-- same values for a diagnostic, are all deliberate and each is a conformance
-- point:
--
--  * a string is QUOTED and ESCAPED. @showDomainMember@ prints it bare, which
--    is right for quoting a cell back at its author and wrong for a document.
--    Escaping matters because 'DMN.XML.XmlToDmnmd.feelChar' un-escapes on the
--    way back in, so a value containing a quote survives a round trip only if
--    it is escaped on the way out.
--  * @FSection Feq v@ renders as a BARE VALUE. @= 5@ is dmnmd's own spelling
--    and is not in §9.2 rule 5, whose operator slot is @\< \<= > >=@ only; a
--    bare value IS DMN's equality test. Same meaning, and the reader reads it
--    back to a shape 'DMN.Translate.JS.feel2jsIn' renders identically
--    (@feel2jsIn lhs (FNullary rhs) = feel2jsIn lhs (FSection Feq rhs)@), so
--    the normalisation is invisible downstream.
--  * arithmetic is PARENTHESISED at every operator application.
--    @showFNumFunction@ renders @FNF3@ flat, so a nested one re-parses with
--    FEEL's precedence rather than the tree's — @(a + b) * c@ would come back as
--    @a + (b * c)@. That is a changed meaning in a well-formed document, which
--    is the exact failure this backend has to avoid.
--  * 'FAnything' in an OUTPUT column is not spellable and is handled by
--    'fidelityDiags', not here; @-@ is emitted, which is correct in an
--    @\<inputEntry\>@ (rule 12) and warned about in an @\<outputEntry\>@.
showFeelXML :: Maybe DMNType -> FEELexp -> String
showFeelXML _ FAnything = "-"
showFeelXML _ (FNullary v) = showValXML v
showFeelXML _ (FSection Feq v) = showValXML v
showFeelXML _ (FSection op v) = showCmpOp op ++ " " ++ showValXML v
showFeelXML _ (FInRange lk lo hi rk) =
  openB lk ++ showNumPlain lo ++ ".." ++ showNumPlain hi ++ closeB rk
  where
    openB BClosed = "["
    openB BOpen = "("
    closeB BClosed = "]"
    closeB BOpen = ")"
showFeelXML _ (FFunction f) = showArith f

-- | Only the four §9.2 rule 5 operators reach here; 'Feq' is handled above by
-- dropping the operator entirely.
showCmpOp :: FBinOp -> String
showCmpOp Flt = "<"
showCmpOp Flte = "<="
showCmpOp Fgt = ">"
showCmpOp Fgte = ">="
showCmpOp Feq = ""

showValXML :: DMNVal -> String
showValXML (VS s) = feelStringLiteral s
showValXML (VN n) = showNumPlain n
showValXML (VB b) = toLower <$> show b
-- A 'VL' is a runtime argument, never a cell, and cannot be built by any cell
-- parser. Spelled out because the alternative is a Haskell constructor name
-- reaching a DMN document.
showValXML (VL vs) = "[" ++ intercalate ", " (showValXML <$> vs) ++ "]"

-- | A FEEL string literal, escaped so that
-- 'DMN.XML.XmlToDmnmd.feelEscape' reads it back unchanged. That parser accepts
-- exactly @\\n \\r \\t \\" \\' \\\\ \\uXXXX@, and an unrecognised escape sends
-- the whole cell down the verbatim path — so this must emit only those.
feelStringLiteral :: String -> String
feelStringLiteral s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '\\' = "\\\\"
    esc '"' = "\\\""
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    esc c = [c]

-- | Arithmetic, fully parenthesised. See 'showFeelXML'.
showArith :: FNumFunction -> String
showArith (FNF0 v) = showValXML v
showArith (FNF1 v) = v
showArith (FNF3 l op r) = "(" ++ showArith l ++ showOp op ++ showArith r ++ ")"
  where
    showOp FNMul = " * "
    showOp FNDiv = " / "
    showOp FNPlus = " + "
    showOp FNMinus = " - "
    -- FEEL's exponentiation is ** (DMN 1.3 §10.3.1.2 rule 27), same spelling.
    showOp FNExp = " ** "

-- * Fidelity
--
-- $fidelity
--
-- The governing rule from @CLAUDE.md@ — never silently discard, never quietly
-- give a different answer — is harder for a writer than for a reader, because a
-- writer can produce a document that is well-formed, XSD-valid, and wrong. What
-- follows is every construct dmnmd accepts that DMN spells differently or not
-- at all, and what this backend does about it.

-- | Everything worth saying about one table's translation.
fidelityDiags :: XMLOpts -> DecisionTable -> [Diagnostic]
fidelityDiags _opts dt = concat
  [ aggregateErrs, outputTestErrs, outputWildcardWarns, collectionWarns
  , rowNumberWarns ]
  where
    inTable msg = "table " ++ show (tableName dt) ++ ": " ++ msg
    at ch row msg =
      inTable $ "output column " ++ show (varname ch) ++ ": "
        ++ maybe "" (\n -> "row " ++ show n ++ ": ") (row_number row) ++ msg

    outCells = [ (ch, row, cell)
               | row@DTrow{} <- allrows dt
               , (ch, cells) <- zip (outHeaders dt) (row_outputs row)
               , cell <- cells ]

    -- HP_Aggregate has no letter in 'DMN.ParseTable.mkHitPolicy_' and no arm in
    -- 'DMN.XML.ParseDMN.xparseHitPolicy', and DMN13.xsd's tHitPolicy enumeration
    -- has no AGGREGATE. It is unreachable, not safe: the pickler's write half
    -- meets it with a bare `error` carrying no table name. Refuse it here, where
    -- there is something to name.
    aggregateErrs = case hitpolicy dt of
      HP_Aggregate ->
        [ errorAt . inTable $
            "hit policy Aggregate has no DMN spelling. DMN 1.3's tHitPolicy"
              ++ " enumeration is UNIQUE, FIRST, PRIORITY, ANY, COLLECT,"
              ++ " RULE ORDER and OUTPUT ORDER; aggregation is COLLECT plus an"
              ++ " @aggregation attribute. Refusing to write this table." ]
      _ -> []

    -- An <outputEntry> is a tLiteralExpression: a VALUE. A comparison is a
    -- unary test, which is legal only in an <inputEntry>. dmnmd accepts `= 5`
    -- and `< 5` in an output column (structuralErrors refuses only an OPEN
    -- range there, R9), and FEEL has no expression that spells them, so writing
    -- one would be inventing a dialect.
    outputTestErrs =
      [ errorAt $ at ch row $
          "the output cell reads " ++ show (showFeelXML (vartype ch) cell)
            ++ ". A DMN <outputEntry> is a literal expression — a VALUE — and a"
            ++ " comparison is a unary test, which DMN admits only in an"
            ++ " <inputEntry> (DMN 1.3 §9.2 rule 12 vs rule 3)."
            ++ " Move the test to an input column, or write the value it selects."
      | (ch, row, cell@FSection{}) <- outCells ]

    -- `-` in an output column. There IS no <outputEntry> spelling: `-` is
    -- rule-12 syntax and lives in <inputEntry> only. An EMPTY <text/> is what
    -- gets written, because dmnmd's own reader reads it back as FAnything —
    -- so the round trip is exact — but a conformant engine will read the rule
    -- as producing null, which is a different statement from dmnmd's
    -- `undefined`, and the author should hear about it.
    outputWildcardWarns =
      [ warnAt $ at ch row $
          "the output cell is a wildcard. DMN has no <outputEntry> spelling for"
            ++ " one — \"-\" is unary-test syntax (§9.2 rule 12) and an"
            ++ " <outputEntry> is a literal expression — so dmnmd writes an empty"
            ++ " <text/>. dmnmd reads that back as the same wildcard; another"
            ++ " engine will read the rule as producing null."
      | (ch, row, FAnything) <- outCells ]

    -- Inherited from the reader, and worth saying on the way out because the
    -- document leaves dmnmd's control. policy/xml-iscollection-membership pins
    -- the reading dmnmd gives it.
    collectionWarns =
      [ warnAt . inTable $
          "column " ++ show (varname ch) ++ " is a collection ("
            ++ typeRefName t ++ "). dmnmd reads a plain value in a collection"
            ++ " column as MEMBERSHIP; FEEL rules 12-13 read the same unary test"
            ++ " as equality against the whole list, which is false for any"
            ++ " non-singleton. The emitted document is self-consistent for dmnmd"
            ++ " and divergent for a conformant engine."
      | ch <- header dt, Just t@(DMN_List _) <- [vartype ch] ]

    -- Markdown row numbers are AUTHORED, so gaps and repeats are meaningful and
    -- survive into diagnostics. DMN has no field for them: the reader assigns
    -- 1..n by position. Silent for the overwhelmingly common 1..n table.
    rowNumberWarns
      | authored == [1 .. length authored] = []
      | otherwise =
          [ warnAt . inTable $
              "the rule numbers written in this table (" ++ showNums authored
                ++ ") are not 1.." ++ show (length authored) ++ ". DMN has no field"
                ++ " for an authored rule number — a <rule> is identified by"
                ++ " position — so they are renumbered on the way out and any gap"
                ++ " or repeat is lost." ]
      where
        authored = mapMaybe row_number [ row | row@DTrow{} <- allrows dt ]
        showNums = intercalate ", " . map show

-- * Small helpers

inHeaders, outHeaders, commentHeaders :: DecisionTable -> [ColHeader]
inHeaders dt = [ ch | ch <- header dt, label ch == DTCH_In ]
outHeaders dt = [ ch | ch <- header dt, label ch == DTCH_Out ]
commentHeaders dt = [ ch | ch <- header dt, label ch == DTCH_Comment ]

-- | An @xsd:ID@. Every one in the document is built here, so they are unique by
-- construction (each is a kind plus its position) and NCName-safe by
-- construction ('slug'), which table and column names are not.
idOf :: String -> [String] -> String
idOf kind parts = intercalate "_" (kind : parts)

-- | An arbitrary string as an NCName fragment. Anything outside
-- @[A-Za-z0-9_]@ becomes @_@, and a leading digit is pushed behind one, because
-- an NCName may not start with a digit.
slug :: String -> String
slug s = case map keep s of
  [] -> "_"
  cs@(c : _) | isDigit c -> '_' : cs
             | otherwise -> cs
  where keep c | isAlphaNum c = c
               | otherwise = '_'
