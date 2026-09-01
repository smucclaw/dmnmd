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
  , fidelityDiags
  ) where

import Data.Char (toLower)
import Data.Function (on)
import Data.List (intercalate, nub, nubBy)
import Data.Maybe (isJust, mapMaybe)
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.Edit (escapeXmlRefs)
import qualified Text.XML.HXT.DOM.ShowXml as SX

import DMN.DecisionTable (showType, showDomainMember, fEval)
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
toXMLFile opts dts0
  | anyErrors diags = (diags, "")
  | otherwise = (diags, toXMLDoc opts dts)
  where
    promoted = map promoteTrailingCatchAll dts0
    dts = map snd promoted
    diags = concatMap fst promoted
         ++ crossTableDiags dts ++ concatMap (fidelityDiags opts) dts

-- | D-16 phase 2. A TRAILING all-wildcard row in a @U@ table is the markdown
-- spelling of DMN's default output value (§8.2.11), so that is what it is
-- emitted as: the row is moved into 'dtDefaultOutput' and dropped from the
-- rules, which makes the emitted table genuinely conformant — no rule overlaps
-- any other — while preserving the meaning for every engine, ordered or not.
-- dmnmd's own reader reads the document back into the same 'DecisionTable'
-- this function returns, so the promotion is invisible to a round trip.
--
-- Warned rather than silent for the same reason 'rowNumberWarns' is: something
-- authored is genuinely dropped — the row's NUMBER, which a default has no
-- field for — and the author should hear that the document spells their row
-- differently than they wrote it.
--
-- Eligibility is deliberately narrow, and each exclusion leaves the row as a
-- rule for 'fidelityDiags' to handle exactly as phase 1 did:
--
--  * @U@ only — under F\/P\/O\/R the row is an ordinary, legal rule, and under
--    the ordered policies it even participates in ordering.
--  * trailing only — a mid-table catch-all makes the rules after it dead under
--    dmnmd's first-match reading, which a default cannot express.
--  * no row comment — a @\<defaultOutputEntry\>@ has no description slot in
--    dmnmd's model, and dropping a comment in silence is worse than the warning.
--  * full arity — a short row is already refused by @ruleArityErrs@; promoting
--    it would hide that refusal.
--  * no comparison in its outputs — @outputTestErrs@ refuses those as rules,
--    and a promotion must not smuggle one past the refusal.
--  * at least one non-wildcard output — an all-wildcard default writes no
--    element at all, so the row would vanish rather than move.
--  * no default already present — a table cannot honestly carry two.
promoteTrailingCatchAll :: DecisionTable -> ([Diagnostic], DecisionTable)
promoteTrailingCatchAll dt
  | hitpolicy dt == HP_Unique
  , Nothing <- dtDefaultOutput dt
  , (lastR : restRev) <- reverse (allrows dt)
  , eligible lastR
  = ( [ warnAt $ "table " ++ show (tableName dt) ++ ": " ++ rowLabel lastR
          ++ " has \"-\" in every input column, so it is this table's answer for"
          ++ " any input no other rule matches. Under hit policy Unique that must"
          ++ " not be a rule — it would overlap every other rule (§8.2.10) — so it"
          ++ " is emitted as the table's default output value (§8.2.11) instead,"
          ++ " and the rule count drops by one. The meaning is preserved for every"
          ++ " engine; the row's own number is not, because a default has no rule"
          ++ " number." ]
    , dt { allrows = reverse restRev, dtDefaultOutput = Just (row_outputs lastR) } )
  | otherwise = ([], dt)
  where
    eligible r =
      not (null (row_inputs r))
        && all (all isAnything) (row_inputs r)
        && length (row_inputs r) == length (inHeaders dt)
        && length (row_outputs r) == length (outHeaders dt)
        && all (== Nothing) (row_comments r)
        && not (any (any isSection) (row_outputs r))
        && not (all (all isAnything) (row_outputs r))
    isAnything FAnything = True
    isAnything _         = False
    isSection FSection{} = True
    isSection _          = False
    rowLabel r = case row_number r of
      Just n  -> "row " ++ show n
      Nothing -> "the unnumbered row at position "
                   ++ show (length [ x | x@DTrow{} <- allrows dt ])

-- | Diagnostics that no single table can see, because they are about the
-- document the tables are assembled INTO.
--
-- The markdown surface has no global variable scope: two tables with a column
-- called @x@ are two unrelated columns, and nothing in a @.md@ file says
-- otherwise. A DMN document does have one — @definitionsOf@ dedupes
-- @\<inputData\>@ by NAME and every decision @href@s the shared node — so the
-- emitter necessarily invents an identification the source did not make.
--
-- That is fine when the types agree and wrong when they do not: two @x@ columns
-- typed @Number@ and @String@ collapse into one @\<inputData\>@ whose
-- @\<variable\>@ can only carry one @typeRef@, while the second decision's own
-- @\<inputExpression\>@ contradicts it. The document is XSD-valid and says
-- something the source did not, at exit 0 — the failure mode this backend
-- exists to avoid, and one the round trip cannot see because dmnmd's reader
-- ignores the DRG entirely (D-6). The same blind spot hid the duplicate-@xsd:ID@
-- defect.
--
-- Warned, not refused: each table is individually fine and the emission is the
-- best available reading. Refusing would block a document whose tables merely
-- reuse a common word. Pinned by @policy/xml-inputdata-name-type-conflict@.
crossTableDiags :: [DecisionTable] -> [Diagnostic]
crossTableDiags dts =
  [ warnAt $
      "input column " ++ show nm ++ " appears with more than one type ("
        ++ intercalate ", " (nub (showType <$> ts)) ++ ") across tables "
        ++ intercalate ", " (show <$> nub [ tableName dt | dt <- dts
                                          , ch <- inHeaders dt, varname ch == nm ])
        ++ ". A DMN document has ONE <inputData> per name and every decision"
        ++ " references it, so the emitted <variable> can carry only one typeRef"
        ++ " while each <inputExpression> keeps its own — the document will say"
        ++ " the column is one type and use it as another. Markdown has no global"
        ++ " scope, so this identification is the emitter's, not the author's."
        ++ " Rename one of the columns if they are not the same thing."
  | (nm, ts) <- namedTypes
  , length (nub (showType <$> ts)) > 1 ]
  where
    namedTypes =
      [ (nm, [ t | dt <- dts, ch <- inHeaders dt, varname ch == nm
                 , Just t <- [vartype ch] ])
      | nm <- nub [ varname ch | dt <- dts, ch <- inHeaders dt ] ]

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
-- @>@ is escaped on top of hxt's table, and that is not belt-and-braces: hxt
-- escapes @<@ and @&@ only, but XML 1.0 §2.4 says the sequence @]]>@ MUST NOT
-- appear in content except when closing a CDATA section. So a cell reading
-- @a]]>b@ was written raw and produced a file that is not well-formed — caught
-- by @xmllint --noout@ on well-formedness alone, and refused by dmnmd's own
-- reader, at __exit 0__. Escaping every @>@ is unconditionally legal, is what
-- mainstream serializers do, and needs no lookahead for the two-character
-- prefix. Pinned by @policy/xml-emit-cdata-close-escaped@.
escapeTree :: ArrowXml a => a XmlTree XmlTree
escapeTree = processTopDown $
  (changeText (escapeWith textEsc) `when` isText)
    >>> (processAttrl (changeAttrValue (escapeWith attrEsc)) `when` isElem)
  where
    (textEsc, attrEsc) = escapeXmlRefs
    escapeWith f = concatMap (\c -> if c == '>' then "&gt;" else f c "")

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
  , X.defInputData = zipWith inputDataOf [1 ..] inputVars
  , X.defsDescisions = zipWith (decisionOf inputDataId) [1 ..] dts
  , X.defDrgElems = []
  , X.defDMNDI = Nothing
    -- ^ Diagram interchange is geometry dmnmd does not have. The reader models
    -- 'X.DMNDI' as a nullary constructor that pickles to @\<dmndi:DMNDI\/\>@
    -- with an UNDECLARED prefix, so emitting one would produce a document
    -- neither dmnmd nor anything else can read.
  }
  where
    -- Every distinct input column name in the document, in first-seen order.
    -- Distinct by NAME: the same column read by two tables is one input.
    inputVars = nubBy ((==) `on` fst)
      [ (varname ch, vartype ch) | dt <- dts, ch <- inHeaders dt ]

    -- __Positional, not name-derived, and that is a correctness fix rather than
    -- a style choice.__ An @xsd:ID@ must be unique across the document, and
    -- 'slug' is not injective — the markdown identifier grammar admits both a
    -- space and an underscore, so columns @a b@ and @a_b@ both slugged to
    -- @inputData_a_b@. That produced two elements with one id and an
    -- @href@ pointing at both: an XSD-INVALID document, at exit 0, which is
    -- precisely the failure mode this backend exists to avoid. Caught by
    -- xmllint on a hand-built probe, not by any fixture.
    inputDataId nm = idOf "inputData" [show (position nm)]
    position nm = length (takeWhile ((/= nm) . fst) inputVars) + 1

    inputDataOf i (nm, ty) = X.InputData
      { X.inpLabel = X.dmnNamed' (idOf "inputData" [show (i :: Int)]) nm
      , X.inpVariable = Just X.InformationItem
          { X.iiLabel = X.dmnNamed' (idOf "informationItem" [show i]) nm
          , X.iiTypeRef = X.TypeRef <$> typeRefOf ty
          }
      }

-- | One table as a @\<decision\>@ wrapping a @\<decisionTable\>@.
--
-- __The @\<variable\>@ is where a single-output table's result type belongs__,
-- so it is emitted rather than left out: a consumer that follows the
-- specification looks there and not at @\<output\>\/\@typeRef@, and until this
-- was written dmnmd stated the type in only the place such a consumer ignores.
-- Its @name@ repeats the decision\'s, which is the DMN convention and what KIE
-- checks for. With two or more outputs the variable names a composite whose
-- type would be a synthesised @\<itemDefinition\>@ dmnmd does not build, so it
-- is emitted with a name and no @typeRef@ rather than with a type that is only
-- one column\'s.
--
-- @\<output\>\/\@typeRef@ is still written as well. Dropping it is a separate
-- question, and a real conformance defect rather than a style point: DMN 1.3
-- §8.3.2, Table 34 says a single-output clause SHALL NOT specify a @typeRef@ OR a
-- @name@, and KIE enforces it with @ILLEGAL_USE_OF_TYPEREF@ \/
-- @ILLEGAL_USE_OF_NAME@. It is nevertheless what every version of this backend
-- has emitted, and the reader must honour the @\<variable\>@ before the writer
-- can stop repeating itself — which is what this change lands. Removing the two
-- attributes is the deliberate follow-up, not a side effect of adding this.
--
-- The @\<informationRequirement\>@ edges are not invented: an input column IS
-- the statement that this decision reads that input, which is exactly what the
-- DRG edge means. dmnmd's own reader parses them into @decInfoReq@ and then
-- ignores them (D-6), so a round trip cannot check them — that is not a reason
-- to leave them out, because it is what makes the output usable in a real DMN
-- tool.
decisionOf :: (String -> String) -> Int -> DecisionTable -> X.Decision
decisionOf inputDataId t dt = X.Decision
  { X.decLabel = X.dmnNamed' (idOf "decision" [show t]) (tableName dt)
  , X.decVariable = Just X.InformationItem
      { X.iiLabel = X.dmnNamed' (idOf "variable" [show t]) (tableName dt)
      , X.iiTypeRef = case outHeaders dt of
          [ch] -> X.TypeRef <$> typeRefOf (vartype ch)
          _    -> Nothing
      }
  , X.decInfoReq =
      [ X.InformationRequirement
          { X.infrLabel = X.DmnCommon (Just (idOf "informationRequirement" [show t, show c])) Nothing
          , X.infrReq = X.RequiredInput
          , X.infoHref = X.Href ("#" ++ inputDataId (varname ch))
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
  , X.dtOutput = zipWith3 (outputClauseOf t) [1 ..] (outHeaders dt) defaultCells
  -- The default output value, sliced per output column. A wildcard slice is a
  -- column with no declared default and writes no element; that is why an
  -- all-wildcard default is not promotable (see 'promoteTrailingCatchAll').
  , X.dtAnnotations =
      [ X.AnnotationClause (Just (varname ch)) | ch <- drop 1 (commentHeaders dt) ]
  , X.dtRules = zipWith (ruleOf t dt) [1 ..] (allrows dt)
  }
  where
    defaultCells = case dtDefaultOutput dt of
      Nothing -> repeat Nothing
      Just ds -> [ if all (== FAnything) d then Nothing else Just d | d <- ds ]
                   ++ repeat Nothing

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
outputClauseOf :: Int -> Int -> ColHeader -> Maybe [FEELexp] -> X.TableOutput
outputClauseOf t c ch dflt = X.TableOutput
  { X.toutName = X.DmnCommon (Just (idOf "output" [show t, show c])) (Just (varname ch))
  , X.toutLabel = Just (X.ColumnLabel (varname ch))
  , X.toutTypeRef = X.TypeRef <$> typeRefOf (vartype ch)
  , X.toutValues = X.OutputValues <$> unaryTestsOf ch
  , X.toutDefault = mkDefault <$> dflt
    -- ^ 'dtDefaultOutput', D-16 phase 2: filled by the XML reader's
    -- @\<defaultOutputEntry\>@ and by 'promoteTrailingCatchAll', and written
    -- through the same 'cellText' an @\<outputEntry>@ uses, so the reader
    -- rebuilds the identical cell.
  }
  where
    mkDefault d = X.DefaultOutputEntry X.TLiteralExpression
      { X.tleExpr = X.TExpr
          { X.exprLabel =
              X.DmnCommon (Just (idOf "defaultOutputEntry" [show t, show c])) Nothing
          , X.exprTypeRef = Nothing
          }
      , X.tleExpressionLanguage = Nothing
      , X.tleContent = Just (X.TextElement (cellText ch d))
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
-- Row comments split across two XML shapes, and the split is dictated by the
-- reader: 'DMN.XML.XmlToDmnmd' builds @row_comments@ as
-- @description : annotationEntries@. So the FIRST comment column becomes
-- @\<description\>@ and the rest become @\<annotationEntry\>@s matching the
-- @\<annotation\>@ clauses on the table.
--
-- __Putting the first one in @\<description\>@ is what makes the common case
-- round-trip exactly__, and it is not a cosmetic preference. A markdown comment
-- cell is @Maybe String@ and an absent one is 'Nothing'; an
-- @\<annotationEntry\>@ has no absent state that survives the reader, which
-- maps both @\<annotationEntry\/\>@ and @\<annotationEntry\>\<text\/\>@
-- to @Just ""@. Emitting the column as annotations therefore turned every
-- comment-less row into an EMPTY comment — ten spurious @\/\/@ lines in
-- @README.md@ alone, which is how this was found. @\<description\>@ is
-- @minOccurs=0@, so "no comment" is spellable there, and it is also the more
-- honest mapping: DMN's per-rule description IS a comment on the rule.
ruleOf :: Int -> DecisionTable -> Int -> DTrow -> X.Rule
ruleOf t dt r row = X.Rule
  { X.ruleLabel = X.DmnCommon (Just (idOf "rule" [show t, show r])) Nothing
  , X.ruleDescription = case row_comments row of
      (Just c : _) | not (null (commentHeaders dt)) -> Just (X.Description c)
      _ -> Nothing
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
      | mtext <- take (length (commentHeaders dt) - 1) (drop 1 (row_comments row))
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
-- __A wildcard is spelled differently on the two sides, and the column's own
-- 'label' is what says which side we are on.__ @-@ is DMN 1.3 §9.2 rule 12
-- syntax, legal in an @\<inputEntry\>@ (a @tUnaryTests@) and meaningless in an
-- @\<outputEntry\>@ (a @tLiteralExpression@) — so an output wildcard becomes an
-- EMPTY entry, which is what 'fidelityDiags' warns about and what dmnmd's own
-- reader reads back as the same 'FAnything'.
cellText :: ColHeader -> [FEELexp] -> String
cellText ch fs
  | label ch == DTCH_Out = intercalate ", " (spell <$> filter (/= FAnything) fs)
  | otherwise = intercalate ", " (spell <$> fs)
  where
    spell = showFeelXML (scalarType ch)
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
-- @not(…)@ is rule 12.b and needs no translation: DMN spells negation exactly as
-- dmnmd does, so this is the rare construct that is isomorphic in both
-- directions. The operand recurses, so `not([1..5])` keeps its interval.
--
-- This arm arrived from the other side of a concurrent branch: D-9 added 'FNot'
-- to 'FEELexp' while this backend was being written, and
-- @-Werror=incomplete-patterns@ turned the collision into a compile error rather
-- than a silently unhandled cell. That is the whole argument for making an IR
-- change a TYPE change, and it is why the flag is on the library stanza.
showFeelXML t (FNot inner) = "not(" ++ showFeelXML t inner ++ ")"
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

-- | Arithmetic. Parentheses go round a nested operator application and __not__
-- round the whole expression: 'DMN.ParseFEEL.parseFNF3' accepts a parenthesised
-- OPERAND but has no production for a parenthesised whole cell, so wrapping the
-- top level made every arithmetic output cell unreadable by dmnmd's own reader
-- ("the cell reads \"(Age * 2)\" … that is not … an arithmetic expression").
showArith :: FNumFunction -> String
showArith (FNF0 v) = showValXML v
showArith (FNF1 v) = v
showArith (FNF3 l op r) = operand l ++ showOp op ++ operand r
  where
    operand f@FNF3{} = "(" ++ showArith f ++ ")"
    operand f = showArith f
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
  [ noOutputErrs, ruleArityErrs
  , aggregateErrs, outputTestErrs, outputWildcardWarns, collectionWarns
  , rowNumberWarns, eqDomainWarns, uniqueCatchAllWarns ]
  where
    inTable msg = "table " ++ show (tableName dt) ++ ": " ++ msg
    at ch row msg =
      inTable $ "output column " ++ show (varname ch) ++ ": "
        ++ maybe "" (\n -> "row " ++ show n ++ ": ") (row_number row) ++ msg

    outCells = [ (ch, row, cell)
               | row@DTrow{} <- allrows dt
               , (ch, cells) <- zip (outHeaders dt) (row_outputs row)
               , cell <- cells ]

    -- @tDecisionTable@ is @output+@ (DMN13.xsd:357) and dmnmd's own reader
    -- picks it with xpList1. A markdown table with no output column parses
    -- today (symptom/struct-onecol-no-output and two others) and there is
    -- simply no DMN document that expresses it. Without this the xpList1 write
    -- half dies on `Prelude.tail: empty list`, which names nothing at all.
    noOutputErrs
      | null (outHeaders dt) =
          [ errorAt . inTable $
              "this table has no output column, and DMN has no document that"
                ++ " expresses one. A <decisionTable> requires at least one"
                ++ " <output> (DMN 1.3, tDecisionTable). Refusing to write this"
                ++ " table." ]
      | otherwise = []

    -- A markdown row may be SHORT: the parser accepts fewer cells than there
    -- are columns and the backends read the missing ones as absent. DMN
    -- requires one entry per column, and dmnmd's own reader refuses a document
    -- that breaks that (XmlToDmnmd checkArity) — so emitting a short rule would
    -- write a document nothing can read, and padding it with "-" would widen
    -- the rule silently, which is worse.
    ruleArityErrs =
      [ errorAt . inTable $
          "rule " ++ maybe ("at position " ++ show ix) show (row_number row)
            ++ " has " ++ show got ++ " " ++ what ++ " cell" ++ plural got
            ++ " but the table declares " ++ show want ++ " " ++ what
            ++ " column" ++ plural want ++ ". DMN requires one entry per column"
            ++ " (tDecisionRule), and dmnmd has no way to say which column a"
            ++ " missing cell belongs to. Fill the row in."
      | (ix, row@DTrow{}) <- zip [1 :: Int ..] (allrows dt)
      , (what, want, got) <-
          [ ("input", length (inHeaders dt), length (row_inputs row))
          , ("output", length (outHeaders dt), length (row_outputs row)) ]
      , want /= got
      ]

    plural 1 = ""
    plural _ = "s"

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

    -- `= v` in an INPUT cell of a column that declares a domain, where v is not
    -- in that domain.
    --
    -- The normalisation `= v` -> `v` is forced: §9.2 rule 5's operator slot is
    -- `< <= > >=` and a bare value IS DMN's equality test, so there is no `= v`
    -- to emit. It is invisible almost everywhere — and NOT here, because dmnmd
    -- makes a distinction DMN does not. `DecisionTable.domainErrors` exempts a
    -- TEST from the declared domain ("a test selects a subset of the domain
    -- rather than naming a member") while checking a plain VALUE as a member. So
    -- `= 9` against a domain of `1, 2, 3` is accepted on the way in and refused
    -- by dmnmd's own reader on the way back, at exit 0 in between.
    --
    -- Warned rather than refused: the emitted document is correct DMN and says
    -- exactly what the author meant. What changes is which of dmnmd's own checks
    -- it then trips. Refusing would block a legal table over an internal
    -- exemption. The round-trip harness carries the matching xfail.
    eqDomainWarns =
      [ warnAt . inTable $
          "input column " ++ show (varname ch) ++ ": the cell reads "
            ++ show (showDomainMember cell) ++ ", which DMN spells as the bare"
            ++ " value " ++ show (showDomainMember (FNullary v))
            ++ " (§9.2 rule 5 has no \"=\" operator). dmnmd exempts a TEST from a"
            ++ " declared domain but checks a plain VALUE against it, and "
            ++ showDomainMember (FNullary v) ++ " is outside this column's domain — so"
            ++ " dmnmd will refuse the document it just wrote. The document is"
            ++ " correct DMN; the asymmetry is dmnmd's."
      | row@DTrow{} <- allrows dt
      , (ch, cells) <- zip (inHeaders dt) (row_inputs row)
      , Just dom <- [enums ch]
      , cell@(FSection Feq v) <- cells
        -- Same membership test domainErrors uses, so the warning cannot drift
        -- from the refusal it is predicting.
      , not (any (`fEval` FNullary v) dom) ]

    -- Inherited from the reader, and worth saying on the way out because the
    -- document leaves dmnmd's control. policy/xml-iscollection-membership pins
    -- the reading dmnmd gives it.
    collectionWarns =
      [ warnAt . inTable $
          "column " ++ show (varname ch) ++ " is a collection ("
            ++ showType t ++ "). dmnmd reads a plain value in a collection"
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

    -- D-16 phase 1. A row with `-` in every input column matches everything, so
    -- under @U@ it overlaps every other rule — the one thing §8.2.10 says a @U@
    -- table must not contain.
    --
    -- __Why this is an emit-time warning and not a reader one.__ The shape is
    -- idiomatic: 40 of the corpus's 221 fixtures have it, the README's own
    -- example among them. Nothing dmnmd itself answers is wrong — 'evalTable'
    -- is first-match and @--to=l4@ renders the row as @OTHERWISE@ — so on the
    -- js/ts/py/l4 paths there is no hazard to report, and a warning printed
    -- there would fire on a fifth of all input and teach people to ignore it.
    -- Putting it in 'DMN.DecisionTable.tableWarnings' was tried and measured:
    -- 33 policy recordings gained a paragraph about a problem they do not have.
    --
    -- The hazard is real and exclusive to THIS backend: under @U@ a conforming
    -- engine may evaluate rules in any order, so a foreign engine reading this
    -- document may return the catch-all instead of a more specific rule. That
    -- is a wrong answer at exit 0 in the one backend whose output is read by
    -- something other than a language toolchain.
    --
    -- Warned rather than refused, like 'eqDomainWarns': the emitted document is
    -- correct DMN and says exactly what the author wrote. Since D-16 phase 2
    -- the ELIGIBLE version of this shape — a trailing, comment-free catch-all —
    -- never reaches here at all: 'promoteTrailingCatchAll' has already turned
    -- it into the default output value DMN spells the intent as (§8.2.11). What
    -- is left to warn about is a catch-all this backend could not promote, and
    -- the message says why not.
    uniqueCatchAllWarns
      | hitpolicy dt /= HP_Unique || null (inHeaders dt) = []
      | otherwise =
          [ warnAt . inTable $
              rowLabel i row ++ " has \"-\" in every input column, so it matches"
                ++ " everything and overlaps every other rule; a table with hit"
                ++ " policy Unique must not contain overlapping rules (§8.2.10)."
                ++ " dmnmd reads it as you meant it — matching is first-match —"
                ++ " but under Unique another engine may evaluate the rules in"
                ++ " any order and return this row instead of a more specific"
                ++ " one. DMN spells this as a default output value (§8.2.11),"
                ++ " and dmnmd emits a trailing comment-free catch-all row as"
                ++ " exactly that; this row was not promoted because " ++ why
                ++ ". For a portable document make the row eligible, or give the"
                ++ " table hit policy F or P, which permit overlapping rules."
          | (i, row) <- zip [1 :: Int ..] rows
          , not (null (row_inputs row))
          , all (all isAnything) (row_inputs row)
          , let why
                  | i /= length rows =
                      "it is not the last row"
                  | any (/= Nothing) (row_comments row) =
                      "it carries a row comment, which a default output value"
                        ++ " has no place for"
                  | isJust (dtDefaultOutput dt) =
                      "the table already carries a default output value"
                  | all (all isAnything) (row_outputs row) =
                      "its outputs are all wildcards, so there is no value to"
                        ++ " declare"
                  | any (\case FSection{} -> True; _ -> False)
                        (concat (row_outputs row)) =
                      "an output cell holds a comparison, which is not a value"
                  | otherwise =
                      "its cells do not line up with the table's columns"
          ]
      where
        rows = [ r | r@DTrow{} <- allrows dt ]
        isAnything FAnything = True
        isAnything _         = False
        rowLabel i row = case row_number row of
          Just n  -> "row " ++ show n
          Nothing -> "the unnumbered row at position " ++ show i

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

