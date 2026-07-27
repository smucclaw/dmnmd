{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- | Turn the DMN XML syntax tree from "DMN.XML.ParseDMN" into dmnmd's own
-- 'DMN.Types.DecisionTable'.
--
-- The rule this module is written to:
--
-- > Anything we parse but do not yet honour must produce a loud, located
-- > diagnostic. Never silently discard.
--
-- Concretely that means three things:
--
-- * Conversion returns 'Diagnostic's as values rather than calling @error@, so
--   one bad column no longer takes the whole document with it, and the caller
--   can decide the exit status.
-- * A construct we cannot represent faithfully makes us refuse /that table/.
--   Emitting a table that quietly never matches is worse than emitting none.
-- * A construct we parse but drop gets a warning naming the table, the column
--   and what was lost.
module DMN.XML.XmlToDmnmd (module DMN.Diagnostic, module DMN.XML.XmlToDmnmd) where

import DMN.XML.ParseDMN as X
import DMN.Diagnostic
import qualified DMN.Types as T
import Data.Char (toLower, digitToInt)
import Data.List (transpose, intercalate)
import DMN.DecisionTable (inferTypes, mkFs, mkFsEither, tableErrors, tableWarnings, trim)
import DMN.ParsingUtils (Parser, parseOnly)
import qualified Data.Text as Text
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char (char, space, hexDigitChar)

-- $> :t runEx1

-- $> dmnThings <- runEx1

-- $> convertIt =<< dmnThings

-- * Diagnostics
--
-- Moved to "DMN.Diagnostic" when the L4 backend needed them too — a transpiler
-- importing the XML reader for a two-constructor type is backwards. Re-exported
-- here so every existing @import DMN.XML.XmlToDmnmd (Diagnostic(..), ...)@ still
-- resolves.

-- * Conversion

-- | Convert parsed DMN into dmnmd decision tables, together with any
-- diagnostics produced along the way.
convertAll :: [XDMN] -> ([Diagnostic], [T.DecisionTable])
convertAll = mconcat . map convertIt

convertIt :: X.XDMN -> ([Diagnostic], [T.DecisionTable])
convertIt d = (itemDefDiags (X.defItemDefs d), []) <> mconcat (map (convdec env) (allDecisions d))
  where env = typeEnvOf d

-- * DMN's data model (@\<itemDefinition\>@)

-- | Every NAMED @\<itemDefinition\>@ in the document, in document order.
--
-- An association list rather than a Map: a document has a handful of these, and
-- keeping duplicates visible is the point — 'itemDefDiags' reports them, and
-- 'lookup' taking the first is then a stated rule rather than an accident of
-- which one @fromList@ happened to keep.
type TypeEnv = [(String, X.ItemDefinition)]

typeEnvOf :: X.XDMN -> TypeEnv
typeEnvOf d = [ (nm, itd) | itd <- X.defItemDefs d, Just nm <- [X.itdName itd] ]

-- | Complaints about the data model itself, independent of whether any column
-- refers to it.
--
-- Only about what we CANNOT honour. A named simple type — @\<typeRef\>@ plus an
-- optional @\<allowedValues\>@ — is now read and used ('resolveTypeRef'), so it
-- is silent. Before tier 1 every itemDefinition warned, because every one of
-- them was being discarded.
--
-- All Warnings: nothing is being read wrongly here. A table that actually USES
-- an unhonourable type fails loudly at its column instead, and says so there.
-- The point of warning at the document level too is the connection between the
-- two — a reader told "unknown typeRef" needs to be able to learn that the
-- document declares that name and we could not use it.
itemDefDiags :: [X.ItemDefinition] -> [Diagnostic]
itemDefDiags itds = concatMap one itds ++ dups
  where
    one itd = case X.itdName itd of
      Nothing ->
        [ warnAt $ "<itemDefinition> without a name: it cannot be referred to by any"
            ++ " typeRef, so it is dropped." ]
      Just nm -> case unusable itd of
        Nothing  -> []
        Just why -> [ warnAt $ "<itemDefinition> " ++ show nm ++ ": " ++ why ]

    dups =
      [ warnAt $ "<itemDefinition> " ++ show nm ++ " is declared " ++ show n
          ++ " times; the first is used and the rest are dropped."
      | (nm, n) <- counts, n > (1 :: Int) ]
    counts = [ (nm, length g)
             | nm <- nubOrd names, let g = filter (== nm) names ]
    names = [ nm | Just nm <- map X.itdName itds ]
    nubOrd = foldr (\x acc -> x : filter (/= x) acc) []

-- | Why this @\<itemDefinition\>@ cannot be honoured, or 'Nothing' if it can.
--
-- The XSD body is a three-way choice (typeRef+allowedValues | itemComponent* |
-- functionItem?), so these are not arbitrary rejections — they are the two
-- branches of that choice dmnmd has no representation for.
-- This used to refuse @isCollection="true"@ outright, at length, because the
-- cell layer read every cell of a collection column as a scalar and mapping the
-- attribute onto that would have turned a loud refusal into a silent wrong
-- answer. The cell layer now honours collections, so the attribute is HONOURED
-- here — see 'isCollectionOf' — and the guarantee the old message was protecting
-- is kept a level down instead: 'DMN.DecisionTable.structuralErrors' refuses,
-- per rule id, every collection cell that is not a plain member. So a
-- @\<inputEntry\>@ holding @list contains(?, "RED")@, @not(…)@ or @count(?)>0@
-- is still refused; what changed is that the refusal is now about that cell
-- rather than about the whole type.
unusable :: X.ItemDefinition -> Maybe String
unusable itd
  | not (null (X.itdComponents itd)) =
      Just $ "it is a structured type (<itemComponent> "
        ++ intercalate ", " [maybe "(unnamed)" show c | c <- X.itdComponents itd]
        ++ "). dmnmd's decision table columns are scalars, so there is nothing to"
        ++ " map a record onto. A column whose typeRef names this is refused."
  | Nothing <- X.itdTypeRef itd =
      Just $ "it declares no <typeRef>, so it is either a <functionItem> type or"
        ++ " empty. dmnmd has neither. A column whose typeRef names this is refused."
  | otherwise = Nothing

-- | Does this @\<itemDefinition\>@ say @isCollection="true"@?
--
-- The XSD gives the attribute a default of @"false"@ (@xsd/DMN13.xsd:244@), and
-- DMN's own examples write it out explicitly on non-collections, so an absent
-- attribute and an explicit @"false"@ must read the same.
isCollectionOf :: X.ItemDefinition -> Bool
isCollectionOf = maybe False ((== "true") . map toLower . trim) . X.itdIsCollection

-- | Every @<decision>@ in the file. 'X.defsDescisions' only holds the leading
-- run of them: a file that interleaves @<inputData>@ with @<decision>@ puts the
-- rest in 'X.defDrgElems', and those used to be dropped on the floor.
allDecisions :: X.XDMN -> [X.Decision]
allDecisions d = X.defsDescisions d ++ [dec | X.DrgDec dec <- X.defDrgElems d]

convdec :: TypeEnv -> X.Decision -> ([Diagnostic], [T.DecisionTable])
convdec env dec = case X.decDTable dec of
    Just (X.ExprDTable tabl) -> convTable env decisionName tabl
    Just (X.ExprLiteral _)   ->
      ([ warnAt $ "decision " ++ show decisionName
          ++ ": its decision logic is a <literalExpression>, not a <decisionTable>; skipped." ], [])
    Nothing ->
      ([ warnAt $ "decision " ++ show decisionName ++ ": no decision logic; skipped." ], [])
  where
    decisionName = dmnnName $ decLabel dec

-- | One column of the table, before its cells have been looked at.
data Col = Col
  { colKind :: T.DTCH_Label
  , colName :: String
  , colDeclared :: Maybe T.DMNType
    -- ^ the @typeRef@, if there was one and we understood it. 'Nothing' means
    -- "no usable declaration", which is the only case in which we infer.
  , colValuesText :: Maybe String
    -- ^ the FEEL text of @<inputValues>@ / @<outputValues>@, the column's
    -- declared domain. Becomes 'T.enums'.
  }

-- | Convert one @<decisionTable>@.
--
-- Either we emit a table we believe in, or we emit none and say why. There is
-- no middle setting: a table whose guards can never fire, or whose rules have
-- been silently widened by a dropped entry, is not a lesser version of the
-- right answer — it is a wrong answer that exits 0.
convTable :: TypeEnv -> String -> X.DecisionTable -> ([Diagnostic], [T.DecisionTable])
convTable env name X.DecisionTable
  { X.dtHitPolicy
  , X.dtInput
  , X.dtOutput
  , X.dtAnnotations
  , X.dtRules
  }
  | anyErrors structural  = (structural, [])
  | anyErrors cellDiags   = (structural ++ cellDiags, [])
  | anyErrors domainDiags = (structural ++ cellDiags ++ domainDiags, [])
  | otherwise             = (structural ++ cellDiags ++ domainDiags, [table])
  where
    -- @<inputValues>@ and @<outputValues>@ are a declared domain, and this is
    -- the one place a domain arrives declared by the input format rather than
    -- inferred. They used to be unpickled, converted into 'T.enums' and then
    -- read by nobody for input columns — parsed, not honoured, silent, which is
    -- the rule this module exists to enforce, broken inside the module itself.
    --
    -- The rule is 'tableErrors', shared with the markdown reader rather than
    -- reimplemented: see the standing note above 'DMN.DecisionTable.mkFsEither'
    -- about validators that drift from their constructor. Only the framing is
    -- local — `inTable` supplies the table name, matching every other
    -- diagnostic here, and an Error means this table is not emitted at all.
    domainDiags = (errorAt . inTable <$> tableErrors table)
               ++ (warnAt  . inTable <$> tableWarnings table)

    inTable :: String -> String
    inTable msg = "table " ++ show name ++ ": " ++ msg

    -- ---- columns -------------------------------------------------------
    (inColDiags,  inCols)  = unzip' (zipWith (convInputCol  env inTable) [1 ..] dtInput)
    (outColDiags, outCols) = unzip' (zipWith (convOutputCol env inTable) [1 ..] dtOutput)

    nIn = length dtInput
    nOut = length dtOutput
    nRows = length dtRules

    -- ---- structural checks ---------------------------------------------
    -- A rule with more entries than there are columns had the surplus dropped
    -- by zipWith, which broadens what the rule matches. A rule with fewer had
    -- a column invented for it. Both are XSD violations, not degradations.
    arityDiags = concat (zipWith checkArity [1 :: Int ..] dtRules)

    checkArity ix r =
      concat
        [ arity "inputEntry" nIn (length (ruleInputEntry r))
        , arity "outputEntry" nOut (length (ruleOutputEntry r))
        , annotationArity (length (ruleAnnotations r))
        ]
      where
        rid = ruleIdent ix r
        arity what expected actual
          | expected == actual = []
          | otherwise =
              [ errorAt . inTable $
                  rid ++ " has " ++ show actual ++ " <" ++ what ++ "> element"
                      ++ plural actual ++ " but the table declares " ++ show expected
                      ++ " column" ++ plural expected ++ "."
                      ++ " DMN requires one entry per column." ]
        annotationArity actual
          | actual == length dtAnnotations = []
          | otherwise =
              [ warnAt . inTable $
                  rid ++ " has " ++ show actual ++ " <annotationEntry> element"
                      ++ plural actual ++ " but the table declares "
                      ++ show (length dtAnnotations) ++ " <annotation> column"
                      ++ plural (length dtAnnotations)
                      ++ "; the extra text is still carried into the row comments." ]

    annotationDiags
      | null dtAnnotations = []
      | otherwise =
          [ warnAt . inTable $
              "the <annotation> column names ("
                ++ intercalate ", " (map (show . annotationClauseName) dtAnnotations)
                ++ ") are dropped. Each rule's <annotationEntry> text is kept, as a row"
                ++ " comment, but dmnmd has no place to record which annotation column"
                ++ " it came from." ]

    structural = inColDiags ++ outColDiags ++ arityDiags ++ annotationDiags

    -- ---- cells ----------------------------------------------------------
    -- Column-major: the type of a column is a property of the whole column, so
    -- the cells cannot be built until every cell text in it has been seen.
    inTexts  = columnTexts nIn  [map entryText (ruleInputEntry r)  | r <- dtRules]
    outTexts = columnTexts nOut [map entryText (ruleOutputEntry r) | r <- dtRules]

    ruleIdents = zipWith ruleIdent [1 ..] dtRules

    inResolved  = zipWith (resolveColumn inTable ruleIdents) inCols  inTexts
    outResolved = zipWith (resolveColumn inTable ruleIdents) outCols outTexts

    cellDiags = concatMap rcDiags inResolved ++ concatMap rcDiags outResolved

    -- ---- assembly --------------------------------------------------------
    header = map rcHeader inResolved ++ map rcHeader outResolved

    inByRow  = byRow nRows nIn  (map rcCells inResolved)
    outByRow = byRow nRows nOut (map rcCells outResolved)

    rows =
      [ T.DTrow
          { T.row_number = Just i
          , T.row_inputs = ins
          , T.row_outputs = outs
          , T.row_comments =
              fmap description (ruleDescription r)
                : map (Just . annotationEntryText) (ruleAnnotations r)
          }
      | (i, r, ins, outs) <- zip4 [1 ..] dtRules inByRow outByRow
      ]

    table = T.DTable
      { T.tableName = name
      , T.hitpolicy = dtHitPolicy
      , T.header = header
      , T.allrows = rows
      }

-- | @<input>@ → column descriptor. The variable name is the FEEL text of the
-- @<inputExpression>@; the display label and a positional name are fallbacks,
-- rather than the old literal @"I don't know?"@.
convInputCol :: TypeEnv -> (String -> String) -> Int -> TableInput -> (Diagnostics, Col)
convInputCol env inTable ix TableInput { tinpLabel, tinpExpr = InputExpression inpexpr, tinpValues } =
    ( diags ++ domDiags
    , Col { colKind = T.DTCH_In
          , colName = nm
          , colDeclared = ty
          , colValuesText = domain
          }
    )
  where
    nm = firstNonEmpty
      [ maybe "" innerText (tleContent inpexpr)
      , maybe "" columnLabel tinpLabel
      , "input" ++ show ix
      ]
    locate = inTable . (("input column " ++ show nm ++ ": ") ++)
    (diags, ty, inherited) = resolveType env locate (exprTypeRef (tleExpr inpexpr))
    (domDiags, domain) =
      pickDomain locate "<inputValues>"
        (fmap (innerText . utText . unInputValues) tinpValues) inherited

-- | @<output>@ → column descriptor. DMN 1.3 makes @label@, @name@ and
-- @typeRef@ all optional on @<output>@, so none can be assumed present.
convOutputCol :: TypeEnv -> (String -> String) -> Int -> TableOutput -> (Diagnostics, Col)
convOutputCol env inTable ix TableOutput { toutName, toutLabel, toutTypeRef, toutValues, toutDefault } =
    ( diags ++ domDiags ++ defaultDiags
    , Col { colKind = T.DTCH_Out
          , colName = nm
          , colDeclared = ty
          , colValuesText = domain
          }
    )
  where
    nm = firstNonEmpty
      [ maybe "" columnLabel toutLabel
      , maybe "" id (dmnLabel toutName)
      , "output" ++ show ix
      ]
    locate = inTable . (("output column " ++ show nm ++ ": ") ++)
    (diags, ty, inherited) = resolveType env locate toutTypeRef
    (domDiags, domain) =
      pickDomain locate "<outputValues>"
        (fmap (innerText . utText . unOutputValues) toutValues) inherited

    -- <defaultOutputEntry> is the value the table takes when no rule matches.
    -- dmnmd's DecisionTable has no slot for it — the L4 backend's own
    -- L4Opts.defaultResult is set by the CLI, not by the table — so it is
    -- parsed, checked, and then genuinely lost. Say so, with the value, rather
    -- than emitting an OTHERWISE that quietly disagrees with the source file.
    defaultDiags = case toutDefault of
      Nothing -> []
      Just d ->
        [ warnAt . inTable $
            "output column " ++ show nm ++ " declares <defaultOutputEntry> "
              ++ show (defaultOutputText d)
              ++ ", which dmnmd does not carry into its decision table."
              ++ " The generated code will fall back to its own default instead."
              ++ " Pass the value to the backend explicitly if it matters." ]

defaultOutputText :: DefaultOutputEntry -> String
defaultOutputText (DefaultOutputEntry tle) = maybe "" innerText (tleContent tle)

unInputValues :: InputValues -> UnaryTestsBody
unInputValues (InputValues b) = b

unOutputValues :: OutputValues -> UnaryTestsBody
unOutputValues (OutputValues b) = b

type Diagnostics = [Diagnostic]

-- | A column after its cells have been read: header, cells (row-major within
-- the column) and whatever we had to complain about.
data ResolvedCol = ResolvedCol
  { rcHeader :: T.ColHeader
  , rcCells :: [[T.FEELexp]]
  , rcDiags :: Diagnostics
  }

-- | Give a column its type and build its cells.
--
-- Type inference is applied to a column only when it has no usable @typeRef@.
-- DMN carries the types explicitly, so guessing at a declared column — and
-- worse, letting a guess override or truncate it, which is what running
-- markdown's whole-table 'DMN.DecisionTable.mkDTable' pass over XML did — is
-- never right.
resolveColumn :: (String -> String) -> [String] -> Col -> [String] -> ResolvedCol
resolveColumn inTable ruleIdents col texts = ResolvedCol
    { rcHeader = T.DTCH
        { T.label = colKind col
        , T.varname = colName col
        , T.vartype = ty
        , T.enums = enums
        }
    , rcCells = map snd built
    , rcDiags = concatMap fst built ++ enumDiags
    }
  where
    where_ = inTable . ((kindWord ++ " column " ++ show (colName col) ++ ": ") ++)
    kindWord = case colKind col of
      T.DTCH_In -> "input"
      T.DTCH_Out -> "output"
      T.DTCH_Comment -> "comment"

    -- First pass: read every cell as an untyped string. This is exactly the
    -- evidence inferType wants — in particular the FEEL quotes are still on
    -- "2020", which is the only thing that distinguishes it from the number.
    firstPass = map (mkFs Nothing) texts

    ty = case colDeclared col of
      Just t -> Just t
      Nothing -> T.vartype (inferTypes (T.DTCH (colKind col) (colName col) Nothing Nothing) firstPass)

    built =
      [ mkCells (\m -> where_ (atRule rid ++ m)) ty t
      | (rid, t) <- zip (ruleIdents ++ repeat "") texts
      ]
    atRule "" = ""
    atRule rid = rid ++ ": "

    (enumDiags, enums) = case colValuesText col of
      Nothing -> ([], Nothing)
      Just vtext ->
        let (ds, fs) = mkCells (\m -> where_ ("declared value list: " ++ m)) ty vtext
        in (ds, Just fs)

-- | Build the FEEL expressions for one cell, given the column's settled type.
--
-- The XML-side quoting is handled here, where the type is known, and by
-- parsing: a string column's cell is read as a FEEL string literal (or a
-- comma-separated list of them), escapes and surrounding whitespace included.
-- What it is emphatically not is a first-char\/last-char test — that reinstated
-- the double-quoting bug for anything with a newline round it, turned the FEEL
-- string @"2020"@ into the number 2020 and @"007"@ into 7, and corrupted any
-- expression that merely happened to start and end with a quote.
mkCells :: (String -> String) -> Maybe T.DMNType -> String -> (Diagnostics, [T.FEELexp])
mkCells locate ty text
  | isStringy = case parseOnly feelStringList (Text.pack text) of
      Right ss -> ([], [T.FNullary (T.VS s) | s <- ss])
      Left _
        | '"' `elem` text ->
            ( [ warnAt . locate $
                  show text ++ " is not a FEEL string literal, nor a comma-separated list"
                    ++ " of them. dmnmd keeps the text as written — quotes included — and"
                    ++ " splits it on commas, which is wrong for anything more structured"
                    ++ " than a literal." ]
            , verbatim )
        | otherwise -> ([], verbatim)
  | otherwise = case mkFsEither ty text of
      Right fs -> ([], fs)
      Left msg -> ([errorAt (locate msg)], [])
  where
    -- Tests the ELEMENT type, not just the column type. A `[String]` column
    -- whose isStringy said False would skip the FEEL string-literal parser and
    -- fall through to the `mkFsEither` comma-split path — reinstating
    -- symptom/xml-comma-split-negation for exactly the type this release adds,
    -- a fresh silent shred introduced by the fix that was supposed to help.
    isStringy = ty == Nothing
             || ty == Just T.DMN_String
             || T.elemType ty == Just T.DMN_String
    -- total for Nothing and DMN_String: both just keep the text
    verbatim = mkFs ty text

-- * FEEL string literals

-- | A FEEL string literal, or a comma-separated list of them — the shape a
-- string column's @<inputEntry>@ \/ @<outputEntry>@ \/ @<outputValues>@ takes
-- when it is a plain literal. Anything else fails, and the caller keeps the
-- text verbatim rather than guessing.
feelStringList :: Parser [String]
feelStringList = space *> M.sepBy1 (feelString <* space) (char ',' *> space)

feelString :: Parser String
feelString = char '"' *> M.manyTill feelChar (char '"')

feelChar :: Parser Char
feelChar = (char '\\' *> feelEscape) M.<|> M.anySingle

-- | Only the escapes FEEL defines. An unrecognised one fails the parse, which
-- sends the cell down the verbatim path — better than inventing a meaning.
feelEscape :: Parser Char
feelEscape = M.choice
  [ '\n' <$ char 'n'
  , '\r' <$ char 'r'
  , '\t' <$ char 't'
  , '"'  <$ char '"'
  , '\'' <$ char '\''
  , '\\' <$ char '\\'
  , char 'u' *> (toEnum . foldl (\acc d -> acc * 16 + digitToInt d) 0 <$> M.count 4 hexDigitChar)
  ]

-- * Types

-- | Resolve a @typeRef@ against the document's data model and then the FEEL
-- built-ins, attributing any complaint to the column it came from.
--
-- Returns the column's type AND the domain it inherits from a named type, if
-- any. Those two travel together because they come from the same
-- @\<itemDefinition\>@, and splitting them would mean looking it up twice with
-- two chances to disagree.
resolveType :: TypeEnv -> (String -> String) -> Maybe TypeRef -> (Diagnostics, Maybe T.DMNType, Maybe String)
resolveType _ _ Nothing = ([], Nothing, Nothing)
resolveType env locate (Just (TypeRef raw)) =
  let (ds, ty, dom) = resolveTypeRef env raw
  in (map (\d -> d { diagMessage = locate (diagMessage d) }) ds, ty, dom)

-- | The named-type lookup, and the FEEL built-in fallback behind it.
--
-- A @typeRef@ naming an @\<itemDefinition\>@ resolves to THAT type's own
-- @\<typeRef\>@ — which may itself be another itemDefinition, so this follows
-- the chain — and inherits the NEAREST @\<allowedValues\>@ along it. Nearest
-- rather than merged: DMN lets a derived type restrict its base, so the one the
-- column actually names is the tighter statement, and merging two domains could
-- only ever widen one of them.
--
-- The cycle guard is not defensive programming. @\<itemDefinition\>@ is
-- recursive by construction and nothing in the XSD forbids @A@ deriving from
-- @B@ deriving from @A@; without the visited set that document hangs the
-- converter instead of being reported.
resolveTypeRef :: TypeEnv -> String -> (Diagnostics, Maybe T.DMNType, Maybe String)
resolveTypeRef env = go []
  where
    go seen raw = case lookupItemDef env raw of
      Nothing ->
        -- Not a declared type, so it must be a FEEL built-in. If it is not, the
        -- existing "unknown typeRef" error stands — but it can now say whether
        -- the document DECLARED that name and we refused it, which is the whole
        -- point of the document-level warnings above.
        let (ds, ty) = convertType (TypeRef raw)
        in (map (enrich raw) ds, ty, Nothing)
      Just (nm, itd)
        | nm `elem` seen ->
            ( [ errorAt $ "typeRef " ++ show raw ++ " is circular: "
                  ++ intercalate " -> " (reverse seen) ++ " -> " ++ nm
                  ++ ". Refusing to convert this table." ]
            , Nothing, Nothing )
        | Just why <- unusable itd ->
            ( [ errorAt $ "typeRef " ++ show raw ++ " names an <itemDefinition> dmnmd"
                  ++ " cannot use: " ++ why ++ " Refusing to convert this table." ]
            , Nothing, Nothing )
        | otherwise ->
            let base = maybe "" X.unItemTypeRef (X.itdTypeRef itd)
                (ds, ty, inherited) = go (nm : seen) base
                -- Wrap AFTER the recursive resolve, so a collection derived from
                -- a named base composes for free. A collection OF a collection
                -- yields [[T]], which 'structuralErrors' R1 refuses with the same
                -- message the markdown side gives — one rule, not two.
                ty' | isCollectionOf itd = T.DMN_List <$> ty
                    | otherwise          = ty
            in (ds, ty', allowedValuesText itd `orElse` inherited)

    orElse a b = maybe b Just a

    -- Say more when the unknown name IS in the document. Without this the
    -- reader is told a type is unknown while the file declares it three lines
    -- up, which is exactly the self-contradicting diagnostic tier 1 exists to
    -- remove.
    enrich raw d
      | diagSeverity d == Error, Just why <- declaredButUnusable raw =
          d { diagMessage = diagMessage d ++ " (This document DOES declare "
                ++ show raw ++ ", but " ++ why ++ ")" }
      | otherwise = d

    declaredButUnusable raw = lookupItemDef env raw >>= unusable . snd

-- | Look a @typeRef@ up in the data model, exactly first and then with any
-- namespace prefix stripped.
--
-- Prefix-stripped as a fallback because a @typeRef@ pointing at a type declared
-- in the same file is routinely written @tns:Category@ while the
-- @\<itemDefinition name=\"Category\"\>@ carries no prefix. Exact first, so a
-- type genuinely named with a colon still wins over the stripped reading.
lookupItemDef :: TypeEnv -> String -> Maybe (String, X.ItemDefinition)
lookupItemDef env raw =
  case lookup key env of
    Just itd -> Just (key, itd)
    Nothing  -> (,) stripped <$> lookup stripped env
  where
    key = trim raw
    stripped = reverse (takeWhile (/= ':') (reverse key))

-- | The FEEL text of an @\<itemDefinition\>@'s @\<allowedValues\>@.
allowedValuesText :: X.ItemDefinition -> Maybe String
allowedValuesText = fmap (innerText . utText . X.unAllowedValues) . X.itdAllowedValues

-- | Choose between the domain a column declares for itself and the one it
-- inherits from its named type.
--
-- The column's own wins. DMN's model is that a derived type RESTRICTS its base,
-- and @\<inputValues\>@ on the column is the most derived statement there is —
-- so taking the inherited one instead could only widen what the column accepts,
-- and widening a declared domain is the defect 'domainErrors' exists to catch.
--
-- When the two disagree it warns rather than refusing. A refusal here would
-- reject documents that convert cleanly today, since before tier 1 the inherited
-- domain did not exist at all and could not conflict with anything. Whether a
-- column's own domain must be a SUBSET of its type's is a real check and a
-- deliberately separate one: it needs the two to be compared as parsed FEEL
-- rather than as text, which is 'DMN.DecisionTable.domainErrors' territory.
pickDomain :: (String -> String) -> String -> Maybe String -> Maybe String -> (Diagnostics, Maybe String)
pickDomain locate what own inherited = case (own, inherited) of
  (Nothing, inh)     -> ([], inh)
  (Just o,  Nothing) -> ([], Just o)
  (Just o,  Just i)
    | trim o == trim i -> ([], Just o)
    | otherwise ->
        ( [ warnAt . locate $
              "its " ++ what ++ " declares " ++ show o ++ " while its typeRef's"
                ++ " <allowedValues> declares " ++ show i ++ ". The column's own"
                ++ " list is used, because a derived declaration restricts rather"
                ++ " than widens — but dmnmd does not check that it actually is a"
                ++ " subset." ]
        , Just o )

-- | Map a DMN @typeRef@ onto a dmnmd column type.
--
-- DMN 1.3 §10.3.2.1 lists the built-in FEEL types as: number, string, boolean,
-- date, time, date and time, days and time duration, years and months duration.
--
-- The temporal five are a hard error, not a degradation to string. dmnmd has no
-- temporal type, and 'DMN.DecisionTable.mkF' on a string column stores the cell
-- text verbatim — so an input entry of @< date("2020-01-01")@ becomes an
-- equality test against the literal text @< date("2020-01-01")@ and the
-- generated code can never match. A table we cannot represent must not be
-- emitted in a form that silently never fires.
--
-- An unrecognised type (a user-defined @<itemDefinition>@, a vendor spelling)
-- is different: we do not know that we cannot represent it, so it warns and
-- leaves the column for inference, exactly as an absent @typeRef@ would.
convertType :: TypeRef -> (Diagnostics, Maybe T.DMNType)
convertType (TypeRef raw) = case canonical of
    "number"   -> ok T.DMN_Number
    "integer"  -> ok T.DMN_Number
    "int"      -> ok T.DMN_Number
    "long"     -> ok T.DMN_Number
    "short"    -> ok T.DMN_Number
    "double"   -> ok T.DMN_Number
    "float"    -> ok T.DMN_Number
    "decimal"  -> ok T.DMN_Number
    "string"   -> ok T.DMN_String
    "boolean"  -> ok T.DMN_Boolean
    "bool"     -> ok T.DMN_Boolean
    t | t `elem` temporalTypes ->
          ( [ errorAt $
                "typeRef " ++ show raw ++ " is a FEEL temporal type. dmnmd has no temporal"
                  ++ " column type, and reading it as a string would turn every guard in"
                  ++ " this column into a string comparison that can never match."
                  ++ " Refusing to convert this table." ]
          , Nothing )
    -- An unrecognised typeRef is an ERROR, not a warning, for exactly the reason the
    -- temporal branch above is: the document declares a domain we do not model, and
    -- falling back to inference-from-cells can silently produce a String column whose
    -- guards become literal string comparisons that never match. That is the
    -- never-matching-guard defect wearing a different hat — refusing beats guessing.
    -- Widening support is a matter of adding the type above, which the message says.
    _ ->
      ( [ errorAt $
            "unknown typeRef " ++ show raw ++ ". dmnmd does not model this type, and"
              ++ " inferring one from the cells risks turning every guard in this column"
              ++ " into a string comparison that can never match."
              ++ " Refusing to convert this table. Known types are: "
              ++ unwords knownTypes ++ "." ]
      , Nothing )
  where
    ok t = ([], Just t)
    -- Strip any namespace prefix ("feel:string", "xsd:integer") and normalise
    -- whitespace/case, so that "date and time" and "Date  And Time" agree.
    canonical = unwords . words . map toLower . dropPrefix $ raw
    dropPrefix s = case break (== ':') (reverse s) of
      (rev, "") -> reverse rev
      (rev, _)  -> reverse rev
    temporalTypes =
      [ "date", "time", "date and time", "datetime"
      , "days and time duration", "daytimeduration"
      , "years and months duration", "yearmonthduration", "yearsandmonthsduration"
      ]
    -- Every spelling the case above actually accepts, so the diagnostic does not
    -- send the reader looking for a type that is already supported.
    knownTypes =
      [ "number", "integer", "int", "long", "short", "double", "float", "decimal"
      , "string", "boolean", "bool"
      ]

-- * Small helpers

entryTextOfInput :: InputEntry -> String
entryTextOfInput (InputEntry { ieText = TextElement str }) = str

entryTextOfOutput :: OutputEntry -> String
entryTextOfOutput (OutputEntry { outputEntryText = TextElement str }) = str

class HasEntryText a where
  entryText :: a -> String

instance HasEntryText InputEntry where
  entryText = entryTextOfInput

instance HasEntryText OutputEntry where
  entryText = entryTextOfOutput

-- | How a rule is named in a diagnostic: its @id@ if it has one, else its
-- position.
ruleIdent :: Int -> Rule -> String
ruleIdent ix r = case dmnId (ruleLabel r) of
  Just i -> "rule " ++ show i
  Nothing -> "rule #" ++ show ix

firstNonEmpty :: [String] -> String
firstNonEmpty xs = case filter (not . null) xs of
  (x : _) -> x
  []      -> ""

plural :: Int -> String
plural 1 = ""
plural _ = "s"

unzip' :: [(Diagnostics, a)] -> (Diagnostics, [a])
unzip' xs = (concatMap fst xs, map snd xs)

-- | Row-major entry texts → column-major, with the column count fixed by the
-- table's own declarations. Callers only reach this once arity has been
-- checked, so the indexing is total.
columnTexts :: Int -> [[String]] -> [[String]]
columnTexts n rows = [[row !! j | row <- rows] | j <- [0 .. n - 1]]

-- | Column-major cells → row-major. 'transpose' cannot do this alone: a table
-- with rules but zero columns of a given kind transposes to @[]@ and would lose
-- every row.
byRow :: Int -> Int -> [[a]] -> [[a]]
byRow nrows ncols cols
  | ncols == 0 = replicate nrows []
  | otherwise = transpose cols

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4 as bs cs ds
zip4 _ _ _ _ = []

-- * Fixtures used from the REPL

outThing :: TableOutput
outThing =
  TableOutput
    { toutName = dmnLabeled "OuputClause_99999" "beverages",
      toutLabel = Just ColumnLabel {columnLabel = "Beverages"},
      toutTypeRef = Just TypeRef {typeRef = "string"},
      toutValues = Nothing,
      toutDefault = Nothing
    }

oeExample :: OutputEntry
oeExample =
  OutputEntry
    { outputEntryLabel = dmnWithId "LiteralExpression_1kr45vj"
    , outputEntryText = TextElement {innerText = "\"Aecht Schlenkerla Rauchbier\""}
    }

ieExample :: InputEntry
ieExample =
  InputEntry
    { ieLabel = dmnWithId "UnaryTests_03g3ci0"
    , ieText = TextElement { innerText = "\"Spareribs\"" }
    }
