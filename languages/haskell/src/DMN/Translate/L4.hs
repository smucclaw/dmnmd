{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-imports #-}

{-| Transpile a DMN 'DecisionTable' to L4 source text.

Implements BUILD-SPEC §1, §2: a pure @DecisionTable -> String@ pretty-printer
that emits a @GIVEN@/@GIVETH@ function whose body is a first-match @BRANCH@
closed by a synthesized @OTHERWISE@.

This milestone runs with @emitDitto = False@: arms are fully spelled out and
column-aligned (the THEN keyword lines up), but no @^@ ditto substitution is
performed yet (the ditto grid of BUILD-SPEC §3 is a later milestone). The data
model is in "DMN.Types"; the catch-all/output-header helpers come from
"DMN.DecisionTable". -}

module DMN.Translate.L4 where

import DMN.DecisionTable (getInputHeaders, getOutputHeaders, getCommentHeaders, outputOrder)
import DMN.Types
import Data.Char (isAlpha, isAlphaNum, ord, toUpper)
import Data.List (intercalate)
import Data.Maybe (catMaybes, mapMaybe)
import Numeric (floatToDigits)

-- | Options governing L4 emission (see BUILD-SPEC §4.1).
data L4Opts = L4Opts
  { emitDitto     :: Bool    -- ^ collapse repeated tokens to @^@ (not yet active this milestone)
  , useElem       :: Bool    -- ^ multi-value cells as @elem … (LIST …)@; default is OR-of-EQUALS
  , wrapMaybe     :: Bool    -- ^ @GIVETH A MAYBE …@, arms @JUST …@, @OTHERWISE NOTHING@
  , defaultResult :: String  -- ^ rendered L4 expr emitted after @OTHERWISE@ when no catch-all row
  , emitAsserts   :: Bool    -- ^ append @#EVAL@/@#ASSERT@ lines from the evalTable oracle (TODO)
  }

-- | Default options: ditto on (BUILD-SPEC §3), no @elem@, bare-typed @OTHERWISE@, no asserts.
defaultL4Opts :: L4Opts
defaultL4Opts = L4Opts
  { emitDitto     = True
  , useElem       = False
  , wrapMaybe     = False
  , defaultResult = ""
  , emitAsserts   = False
  }

-- * Entry point

-- | Transpile one decision table to L4 source text. First/Unique/Priority hit
-- policies map to a first-match @BRANCH@; Collect/Aggregate fail loudly
-- (BUILD-SPEC §1.6).
toL4 :: L4Opts -> DecisionTable -> String
toL4 opts dt =
  case hitpolicy dt of
    HP_Collect _   -> error "DMN.Translate.L4: Collect hit policy not yet supported"
    HP_Aggregate   -> error "DMN.Translate.L4: Aggregate hit policy not yet supported"
    -- OutputOrder/RuleOrder are list-valued (they return ALL matching rows, in a
    -- particular order). A first-match BRANCH returns a single scalar, so it would
    -- silently drop every match after the first — a cardinality mismatch. Gate them
    -- loudly until a LIST OF result is implemented (BUILD-SPEC §9 #4).
    HP_OutputOrder -> error "DMN.Translate.L4: OutputOrder hit policy not supported (list-valued: returns all matches, not a single BRANCH result)"
    HP_RuleOrder   -> error "DMN.Translate.L4: RuleOrder hit policy not supported (list-valued: returns all matches, not a single BRANCH result)"
    _              -> unlines (recordDecls ++ [givenBlock ins givethType] ++ [fnHeader, "  BRANCH"] ++ armLines ++ [otherwiseLine])
  where
    ins  = getInputHeaders  (header dt)
    outs = getOutputHeaders (header dt)

    -- Multiple output columns => a DECLAREd result record + a mk<Name> helper.
    multiOut    = length outs > 1
    recName     = pascal (tableName dt)
    mkName      = "mk" ++ recName
    recordDecls
      | multiOut  = declareRecord recName outs ++ [""] ++ ctorHelper recName mkName outs ++ [""]
      | otherwise = []

    -- The function's GIVETH type: scalar for one output column, else the record.
    baseGiveth = case outs of
      [o] -> type2l4 (vartype o)
      _   -> recName
    givethType
      | wrapMaybe opts = "MAYBE " ++ baseGiveth
      | otherwise      = baseGiveth

    -- <name> <arg> <arg> ... MEANS
    fnHeader = quoteVar (tableName dt)
            ++ concatMap (\h -> " " ++ quoteVar (varname h)) ins
            ++ " MEANS"

    -- Arm order + catch-all extraction.
    --   * HP_Priority: order arms by OUTPUT priority via dmnmd's own outputOrder
    --     (the very function evalTable uses), so a first-match BRANCH computes
    --     "highest output priority among matching rows". No catch-all is
    --     special-cased — an all-wildcard row participates in the priority sort
    --     like any other, landing at its output's priority position.
    --   * everything else (First/Unique/Any): row order, with a TRAILING
    --     all-wildcard row lifted into the OTHERWISE.
    (armRows, catchAll) = case hitpolicy dt of
      HP_Priority -> (outputOrder (header dt) (allrows dt), Nothing)
      _           -> case reverse (allrows dt) of
        (lastR : rest) | isCatchAll lastR -> (reverse rest, Just lastR)
        _                                 -> (allrows dt, Nothing)

    -- Render the arms through the ditto grid (BUILD-SPEC §3): one [Maybe Cell]
    -- per arm, column-aligned, with repeated guard tokens collapsed to ^ when
    -- emitDitto is set. Every returned guard line has the same width, so the
    -- trailing THEN keyword lines up across all arms.
    grid       = map (armCells opts ins) armRows
    guardLines = renderDittoGrid opts grid
    armLines   = zipWith mkArm guardLines armRows
    mkArm gl row =
      "    IF " ++ trueIfBlank gl ++ " THEN " ++ armResult opts multiOut mkName outs (row_outputs row)
                ++ commentSuffix (row_comments row)

    -- A vacuously-true guard (every input cell a wildcard — e.g. a non-trailing
    -- catch-all row) has an all-blank grid line; render it as TRUE, padded to the
    -- grid width so THEN stays aligned.
    trueIfBlank gl
      | all (== ' ') gl = rpad (length gl) "TRUE"
      | otherwise       = gl

    otherwiseLine = "    OTHERWISE " ++ otherwiseExpr opts multiOut mkName outs defaultRow defaultResultStr
    -- The synthesized OTHERWISE only ever returns an EXPLICIT catch-all row's
    -- output (the all-wildcard row, when present). With NO catch-all row the table
    -- says nothing about unmatched inputs, so we must NOT fabricate a value from a
    -- data row (that would give unmatched inputs a confidently-wrong answer —
    -- BUILD-SPEC §1.5). Fall through to L4Opts.defaultResult, else a type-default
    -- sentinel (BUILD-SPEC §4.3). For a faithful "no rule matched" use wrapMaybe.
    defaultRow = row_outputs <$> catchAll
    defaultResultStr = defaultResult opts

-- | A data row is a catch-all when every input cell is the wildcard @-@.
isCatchAll :: DTrow -> Bool
isCatchAll row = all (all (== FAnything)) (row_inputs row)

-- * The GIVEN/GIVETH block (BUILD-SPEC §1.1, §1.4)

-- | Emit the @GIVEN … IS A <type>@ lines plus the @GIVETH@ line. The GIVETH
-- type is computed by the caller (it needs the record name for multi-output).
givenBlock :: [ColHeader] -> String -> String
givenBlock ins givethType = intercalate "\n" (givens ++ ["GIVETH A " ++ givethType])
  where
    nameW = maximum (0 : map (length . quoteVar . varname) ins)
    line lead h = lead ++ rpad nameW (quoteVar (varname h)) ++ " IS A " ++ type2l4 (vartype h)
    givens = case ins of
      []       -> []
      (h0:hs)  -> line "GIVEN " h0 : map (line "      ") hs

-- | §1.1 type map: a DMN type to its L4 surface type.
type2l4 :: Maybe DMNType -> String
type2l4 (Just DMN_String)   = "STRING"
type2l4 (Just DMN_Number)   = "NUMBER"
type2l4 (Just DMN_Boolean)  = "BOOLEAN"
type2l4 (Just (DMN_List x)) = "LIST OF " ++ type2l4 (Just x)
type2l4 Nothing             = "STRING"

-- * Result records (BUILD-SPEC §1.4)

-- | @DECLARE <Name> HAS f1 IS A t1 …@
declareRecord :: String -> [ColHeader] -> [String]
declareRecord recName outs =
  ("DECLARE " ++ recName ++ " HAS")
  : map (\h -> "    " ++ rpad fw (fieldName h) ++ " IS A " ++ type2l4 (vartype h)) outs
  where fw = maximum (0 : map (length . fieldName) outs)

-- | The constructor helper: @mk<Name> v1 v2 … MEANS <Name> WITH …@ as a layout-sensitive block.
ctorHelper :: String -> String -> [ColHeader] -> [String]
ctorHelper recName mkName outs =
  givens
  ++ ["GIVETH A " ++ recName]
  ++ [mkName ++ concatMap (\p -> " " ++ p) params ++ " MEANS " ++ recName ++ " WITH"]
  ++ zipWith (\h p -> "    " ++ rpad fw (fieldName h) ++ " IS " ++ p) outs params
  where
    params = ["v" ++ show i | i <- [1 .. length outs]]
    fw     = maximum (0 : map (length . fieldName) outs)
    tw     = maximum (0 : map (length . type2l4 . vartype) outs)
    givens = case zip outs params of
      []            -> []
      ((h0,p0):rest) -> ("GIVEN " ++ pdecl h0 p0) : map (\(h,p) -> "      " ++ pdecl h p) rest
    pdecl h p = rpad pw p ++ " IS A " ++ type2l4 (vartype h)
    pw = maximum (0 : map length params)

-- | A record field name sanitized to a bare L4 identifier, then backtick-quoted
-- if it collides with an L4 reserved word (so @DECLARE Multi HAS `THEN` …@ and the
-- matching @WITH@ block both parse — BUILD-SPEC §9.6).
fieldName :: ColHeader -> String
fieldName h =
  let s = sanitizeIdent (var_name h)
  in if isReservedL4 s then "`" ++ s ++ "`" else s

-- * Guards (BUILD-SPEC §1.2, §1.3)

-- | Render one arm's guard by AND-joining the per-column conjuncts. A vacuously
-- true guard (every cell a wildcard) renders as @TRUE@.
renderGuard :: L4Opts -> [ColHeader] -> DTrow -> String
renderGuard opts ins row =
  case conjuncts of
    [] -> "TRUE"
    cs -> intercalate " AND " cs
  where conjuncts = catMaybes (zipWith (feel2l4In opts) ins (row_inputs row))

-- | One guard conjunct for a single input column cell. 'Nothing' when the cell
-- is the wildcard @-@ (conjunct omitted). A multi-value inner list becomes an
-- @OR@-of-@EQUALS@ (default) or @elem … (LIST …)@ under 'useElem'.
feel2l4In :: L4Opts -> ColHeader -> [FEELexp] -> Maybe String
feel2l4In opts ch cell =
  case filter (/= FAnything) cell of
    []   -> Nothing
    [fx] -> Just (oneFeel field fx)
    fxs
      | useElem opts -> Just ("elem " ++ field ++ " (LIST " ++ intercalate ", " (map feelValL4 fxs) ++ ")")
      | otherwise    -> Just ("(" ++ intercalate " OR " (map (oneFeel field) fxs) ++ ")")
  where field = quoteVar (varname ch)

-- | A single (non-multi) FEEL guard atom against a named field.
oneFeel :: String -> FEELexp -> String
oneFeel field = \case
  FSection Feq  v      -> field ++ " EQUALS " ++ showValL4 v
  FSection Flt  (VN n) -> field ++ " < "  ++ showNumL4 n
  FSection Flte (VN n) -> field ++ " <= " ++ showNumL4 n
  FSection Fgt  (VN n) -> field ++ " > "  ++ showNumL4 n
  FSection Fgte (VN n) -> field ++ " >= " ++ showNumL4 n
  FSection op   v      -> field ++ " EQUALS " ++ showValL4 v   -- non-numeric comparison: best-effort
  FInRange lo hi       -> "(" ++ field ++ " >= " ++ showNumL4 lo ++ " AND " ++ field ++ " <= " ++ showNumL4 hi ++ ")"
  FNullary v           -> field ++ " EQUALS " ++ showValL4 v
  FFunction fnf        -> field ++ " EQUALS " ++ fnf2l4 fnf
  FAnything            -> "TRUE"

-- | The bare value used inside an @elem … (LIST …)@ membership list.
feelValL4 :: FEELexp -> String
feelValL4 = \case
  FNullary v      -> showValL4 v
  FSection Feq v  -> showValL4 v
  FFunction fnf   -> fnf2l4 fnf
  other           -> oneFeel "?" other

-- * The ditto grid (BUILD-SPEC §3)

-- | A logical guard cell: a single token (field / operator / value / @AND@).
type Cell = String

-- | Lay out the BRANCH arm guards as a column-aligned block. Each arm is one row
-- of @Maybe Cell@ (one slot per logical column; 'Nothing' marks an absent token,
-- e.g. a @-@ column or a dropped conjunct). Every column is left-aligned to its
-- per-column max width and separated by a single-space gutter, so cell @j@ starts
-- at the identical absolute source column on every line — the precondition the L4
-- lexer needs to resolve @^@. With 'emitDitto', a guard cell that equals the
-- 'Just' cell directly above collapses to @^@ (which the lexer resolves
-- transitively); 'Nothing' cells emit spaces (copy nothing). Returns one guard
-- string per arm, all of equal width (so the trailing THEN lines up).
renderDittoGrid :: L4Opts -> [[Maybe Cell]] -> [String]
renderDittoGrid opts grid = [ renderRow i | i <- [0 .. nRows - 1] ]
  where
    nRows   = length grid
    nCols   = maximum (0 : map length grid)
    -- pad every row out to nCols with absent cells (defensive; arms are uniform)
    rows    = map (\r -> take nCols (r ++ repeat Nothing)) grid
    -- Widths and padding MUST be measured in the L4 lexer's column units, which
    -- count East-Asian-Wide / Fullwidth glyphs as 2 (a CJK merchant name like
    -- "中" advances the lexer two columns). Using code-point 'length' here would
    -- shift every token after a wide glyph, so a trailing @^@ would resolve to the
    -- wrong source token (or fail to resolve) — silently dropping guard conjuncts
    -- (BUILD-SPEC §3, risk §9 #2).
    cellLen = maybe 0 displayWidth
    widths  = [ maximum (0 : [ cellLen (rows !! i !! j) | i <- [0 .. nRows - 1] ])
              | j <- [0 .. nCols - 1] ]
    renderRow i = intercalate " " [ renderCell i j | j <- [0 .. nCols - 1] ]
    renderCell i j =
      let w = widths !! j in
      case rows !! i !! j of
        Nothing -> replicate w ' '
        Just s
          | emitDitto opts && i > 0 && dittoable i j -> rpadD w "^"
          | otherwise                                -> rpadD w s
    -- Ditto only against the IMMEDIATELY preceding line's token at this column.
    -- The lexer matches @^@ by exact start-column on the previous non-blank line,
    -- so a 'Nothing' directly above cannot be copied even if an earlier row had
    -- the same token; emit the literal in that case. A @^@ copies exactly ONE
    -- token, so a multi-token compound cell (a parenthesized OR-of-EQUALS chain,
    -- e.g. @(Category EQUALS "X" OR …)@) can never be dittoed — it must be spelled
    -- out on every arm (BUILD-SPEC §8). Guard on single-token-ness accordingly.
    dittoable i j = case (rows !! (i - 1) !! j, rows !! i !! j) of
      (Just above, Just here) -> above == here && singleToken here
      _                       -> False
    singleToken t = not (null t) && ' ' `notElem` t

-- | Decompose one arm into its grid cells: per input column a (field, operator,
-- value) triple, plus a leading @AND@ connector for every column after the first
-- present one. A wildcard column contributes 'Nothing's; a compound conjunct (a
-- range or a multi-value OR-chain) is kept whole in the field slot (best-effort
-- ditto — BUILD-SPEC §8).
armCells :: L4Opts -> [ColHeader] -> DTrow -> [Maybe Cell]
armCells opts ins row =
  concat [ colCells j ch cell | (j, ch, cell) <- zip3 [0 :: Int ..] ins (row_inputs row) ]
  where
    present = map (any (/= FAnything)) (row_inputs row)
    colCells j ch cell
      | j == 0    = conjSubCells opts ch cell
      | otherwise = connector j : conjSubCells opts ch cell
    connector j
      | (present !! j) && or (take j present) = Just "AND"
      | otherwise                             = Nothing

-- | The three guard sub-cells (field, operator, value) for one input column.
-- A simple comparison splits into its three tokens (ditto-friendly); a wildcard
-- is three 'Nothing's; anything compound (a range, or a multi-value OR-chain) is
-- kept whole in the field slot with the operator/value slots absent.
conjSubCells :: L4Opts -> ColHeader -> [FEELexp] -> [Maybe Cell]
conjSubCells opts ch cell =
  case filter (/= FAnything) cell of
    []                                            -> [Nothing, Nothing, Nothing]
    [fx] | Just (f, o, v) <- oneFeelCells field fx -> [Just f, Just o, Just v]
    _    -> case feel2l4In opts ch cell of
              Just whole -> [Just whole, Nothing, Nothing]
              Nothing    -> [Nothing, Nothing, Nothing]
  where field = quoteVar (varname ch)

-- | Split a simple guard atom into its (field, operator, value) tokens. 'Nothing'
-- for anything that is not a single field-op-value comparison (a range or wildcard
-- is rendered whole by 'conjSubCells'). The joined tokens reproduce 'oneFeel'.
oneFeelCells :: String -> FEELexp -> Maybe (String, String, String)
oneFeelCells field = \case
  FSection Feq  v      -> Just (field, "EQUALS", showValL4 v)
  FSection Flt  (VN n) -> Just (field, "<",  showNumL4 n)
  FSection Flte (VN n) -> Just (field, "<=", showNumL4 n)
  FSection Fgt  (VN n) -> Just (field, ">",  showNumL4 n)
  FSection Fgte (VN n) -> Just (field, ">=", showNumL4 n)
  FSection _    v      -> Just (field, "EQUALS", showValL4 v)
  FNullary v           -> Just (field, "EQUALS", showValL4 v)
  FFunction fnf        -> Just (field, "EQUALS", fnf2l4 fnf)
  _                    -> Nothing

-- * Outputs / arm results (BUILD-SPEC §1.4, §1.5)

-- | The expression returned by one BRANCH arm.
armResult :: L4Opts -> Bool -> String -> [ColHeader] -> [[FEELexp]] -> String
armResult opts multiOut mkName outs routs
  | wrapMaybe opts = "JUST " ++ parenWrap base
  | otherwise      = base
  where base = resultExpr multiOut mkName outs routs

-- | The expression returned by the synthesized @OTHERWISE@.
otherwiseExpr :: L4Opts -> Bool -> String -> [ColHeader] -> Maybe [[FEELexp]] -> String -> String
otherwiseExpr opts multiOut mkName outs defaultRow defaultStr
  | wrapMaybe opts        = "NOTHING"
  | not (null defaultStr) = defaultStr
  | Just routs <- defaultRow = resultExpr multiOut mkName outs routs
  | otherwise             = typeDefaultL4 multiOut mkName outs

-- | Build a result: a bare scalar for one output column, else @mk<Name> v1 v2 …@.
resultExpr :: Bool -> String -> [ColHeader] -> [[FEELexp]] -> String
resultExpr multiOut mkName outs routs
  | multiOut  = mkName ++ concatMap (\r -> " " ++ atomize r) rendered
  | otherwise = case rendered of
      (r:_) -> r
      []    -> "\"\""
  where rendered = zipWith renderOutCell outs routs

-- | Render one output cell (the first FEEL expression in its inner list),
-- consulting the column's declared type so a wildcard @-@ output cell yields a
-- type-appropriate default rather than the ill-typed @""@ literal (BUILD-SPEC
-- §1.2): under @GIVETH A NUMBER@ an empty/wildcard cell must be @0@, not @""@.
renderOutCell :: ColHeader -> [FEELexp] -> String
renderOutCell col = \case
  []      -> typeDefaultScalar (vartype col)
  (fx:_)  -> showFeelL4 (vartype col) fx

-- | Render an output-side FEEL expression to an L4 literal / arithmetic expr.
-- The wildcard @-@ (@FAnything@) in an output column has no value of its own, so
-- it renders as the column type's default ('typeDefaultScalar') — NOT a bare
-- @""@, which would be ill-typed under a non-STRING GIVETH.
showFeelL4 :: Maybe DMNType -> FEELexp -> String
showFeelL4 ty = \case
  FNullary v     -> showValL4 v
  FFunction fnf  -> fnf2l4 fnf
  FSection op v  -> showValL4 v
  FInRange lo _  -> showNumL4 lo
  FAnything      -> typeDefaultScalar ty

-- | A type-appropriate fallback for a totally empty table's OTHERWISE.
typeDefaultL4 :: Bool -> String -> [ColHeader] -> String
typeDefaultL4 multiOut mkName outs
  | multiOut  = mkName ++ concatMap (\h -> " " ++ atomize (typeDefaultScalar (vartype h))) outs
  | otherwise = case outs of
      (o:_) -> typeDefaultScalar (vartype o)
      []    -> "\"\""

typeDefaultScalar :: Maybe DMNType -> String
typeDefaultScalar = \case
  Just DMN_Number  -> "0"
  Just DMN_Boolean -> "FALSE"
  _                -> "\"\""

-- * Arithmetic (BUILD-SPEC §1.2 output side)

-- | Render a numeric FEEL function to infix L4 arithmetic.
fnf2l4 :: FNumFunction -> String
fnf2l4 = \case
  FNF0 v        -> showValL4 v
  FNF1 s        -> quoteVar s
  FNF3 l op r   -> "(" ++ fnf2l4 l ++ " " ++ fnOp2l4 op ++ " " ++ fnf2l4 r ++ ")"

fnOp2l4 :: FNOp2 -> String
fnOp2l4 = \case
  FNMul   -> "*"
  FNDiv   -> "/"
  FNPlus  -> "+"
  FNMinus -> "-"
  FNExp   -> "^"

-- * Value rendering (BUILD-SPEC §1.2)

-- | Render a DMN value as an L4 literal.
showValL4 :: DMNVal -> String
showValL4 = \case
  VS s -> showStrL4 s
  VN n -> showNumL4 n
  VB b -> if b then "TRUE" else "FALSE"

-- | Double-quote a string literal, escaping backslashes and quotes.
showStrL4 :: String -> String
showStrL4 s = '"' : concatMap esc s ++ "\""
  where esc '"'  = "\\\""
        esc '\\' = "\\\\"
        esc c    = [c]

-- | Render a 'Float' without a spurious @.0@ (@9.0@ -> @"9"@) and without
-- scientific notation (@0.02@ -> @"0.02"@).
--
-- Precision is NOT truncated: the shortest decimal that round-trips the 'Float'
-- is emitted via 'floatToDigits' (the same minimal-digit basis 'show' uses), then
-- formatted as plain (non-exponential) decimal. A fixed @showFFloat (Just 6)@
-- silently corrupted small magnitudes (@0.0000001@ collapsed to @0.0@, a nonzero
-- value rendered as zero — BUILD-SPEC §1.2 / §9.5).
showNumL4 :: Float -> String
showNumL4 n
  | isNaN n             = "0"            -- defensive: DMN cells never hold NaN/Inf
  | isInfinite n        = "0"
  | n == fromIntegral r = show r         -- integral floats: 9.0 -> "9", -3.0 -> "-3"
  | n < 0               = '-' : showNumL4 (negate n)
  | otherwise           = plainDecimal n
  where r = round n :: Integer

-- | Format a strictly-positive, non-integral 'Float' as plain decimal using its
-- minimal round-tripping digit sequence (@floatToDigits@: @x = 0.d1…dn * 10^e@).
plainDecimal :: Float -> String
plainDecimal x = format digits e
  where
    (ds, e) = floatToDigits 10 x
    digits  = concatMap show ds
    format dgs ex
      | ex <= 0            = "0." ++ replicate (negate ex) '0' ++ dgs
      | ex >= length dgs   = dgs ++ replicate (ex - length dgs) '0'  -- (integral; unreached)
      | otherwise          = let (a, b) = splitAt ex dgs in a ++ "." ++ b

-- * Small helpers

-- | Wrap an expression in parens if it contains a space (so @JUST (mk …)@ binds).
parenWrap :: String -> String
parenWrap s = if ' ' `elem` s then "(" ++ s ++ ")" else s

-- | Wrap a constructor argument in parens if it is a compound (already-paren'd
-- arithmetic stays as-is; bare tokens and quoted strings stay bare).
atomize :: String -> String
atomize s
  | null s                       = "\"\""
  | head s == '(' && last s == ')' = s
  | ' ' `elem` s                 = "(" ++ s ++ ")"
  | otherwise                    = s

-- | Trailing @-- comment@ for a row's annotation columns (BUILD-SPEC §3.5).
commentSuffix :: [Maybe String] -> String
commentSuffix cs = case catMaybes cs of
  []    -> ""
  notes -> "  -- " ++ intercalate "; " notes

-- | Backtick-quote a column/table name unless it is already a bare identifier
-- that is NOT an L4 reserved word. Names with spaces/punctuation are quoted (as
-- before); names that are bare-identifier-shaped but collide with an L4 keyword
-- (@OR@, @AND@, @THEN@, @IS@, @MEANS@, @GIVEN@, …) are ALSO quoted, otherwise the
-- emitted @GIVEN OR IS A STRING@ / @IF OR EQUALS …@ fails to parse (BUILD-SPEC
-- §9.6).
quoteVar :: String -> String
quoteVar s
  | isBareIdent s && not (isReservedL4 s) = s
  | otherwise                             = "`" ++ s ++ "`"

-- | A non-empty alpha-led all-identifier-char string (the shape L4 accepts bare).
isBareIdent :: String -> Bool
isBareIdent s = not (null s) && isAlpha (head s) && all isIdentChar s
  where isIdentChar c = isAlphaNum c || c == '_'

-- | L4 reserved words (the lexer keyword set, plus type/literal hazards). A
-- column/table/field name matching one must be backtick-quoted to parse.
isReservedL4 :: String -> Bool
isReservedL4 s = s `elem` reservedWordsL4

reservedWordsL4 :: [String]
reservedWordsL4 =
  -- keyword table from jl4-core/src/L4/Lexer.hs
  [ "GIVEN","GIVETH","GIVES","DECIDE","EXACTLY","MEANS","DECLARE","IF","BRANCH"
  , "THEN","ELSE","OTHERWISE","AND","OR","RAND","ROR","NOT","IS","ONE","OF","WITH"
  , "A","AN","HAS","THE","YIELD","CONSIDER","WHERE","LIST","ASSUME","WHEN","TYPE"
  , "PARTY","DO","DOES","MUST","MAY","SHANT","BREACH","BECAUSE","PROVIDED","WITHIN"
  , "HENCE","LEST","FUNCTION","FROM","TO","EQUALS","IMPLIES","PLUS","MINUS","TIMES"
  , "DIVIDED","MODULO","BY","GREATER","LESS","THAN","ABOVE","BELOW","AT","STARTING"
  , "LEAST","MOST","FOLLOWED","FOR","ALL","AKA","IMPORT","FETCH","POST","ENV"
  , "CONCAT","AS","LET","IN","BE","MEAN","UNLESS"
  -- type / literal names: parse-legal but semantic hazards in a name position
  , "NUMBER","STRING","BOOLEAN","TRUE","FALSE","MAYBE","NOTHING","JUST"
  ]

-- | Right-pad a string to a width with spaces (code-point width; for ASCII-only
-- layout such as the GIVEN/DECLARE blocks).
rpad :: Int -> String -> String
rpad w s = s ++ replicate (max 0 (w - length s)) ' '

-- | Right-pad to a DISPLAY width (East-Asian-Wide aware), matching the L4 lexer's
-- column counting. Used by the ditto grid where absolute column alignment is the
-- load-bearing invariant for @^@ resolution.
rpadD :: Int -> String -> String
rpadD w s = s ++ replicate (max 0 (w - displayWidth s)) ' '

-- | Display width of a string in L4 lexer columns: East-Asian-Wide and Fullwidth
-- code points count as 2, everything else (Latin, accents, regional-indicator
-- emoji) as 1.
displayWidth :: String -> Int
displayWidth = sum . map (\c -> if isWideChar c then 2 else 1)

-- | Is this code point East-Asian Wide (W) or Fullwidth (F)? Ranges mirror the
-- Unicode East_Asian_Width property's W/F classes (the CJK blocks plus fullwidth
-- forms); narrow/neutral/ambiguous code points — and astral-plane emoji such as
-- regional indicators — stay width 1, matching the observed lexer behaviour.
isWideChar :: Char -> Bool
isWideChar c = any (\(lo, hi) -> n >= lo && n <= hi) wideRanges
  where
    n = ord c
    wideRanges =
      [ (0x1100, 0x115F)   -- Hangul Jamo
      , (0x2329, 0x232A)   -- angle brackets
      , (0x2E80, 0x303E)   -- CJK radicals, Kangxi, CJK symbols/punctuation
      , (0x3041, 0x33FF)   -- Hiragana, Katakana, CJK symbols
      , (0x3400, 0x4DBF)   -- CJK Unified Ideographs Extension A
      , (0x4E00, 0x9FFF)   -- CJK Unified Ideographs
      , (0xA000, 0xA4CF)   -- Yi
      , (0xA960, 0xA97F)   -- Hangul Jamo Extended-A
      , (0xAC00, 0xD7A3)   -- Hangul Syllables
      , (0xF900, 0xFAFF)   -- CJK Compatibility Ideographs
      , (0xFE10, 0xFE19)   -- Vertical forms
      , (0xFE30, 0xFE6F)   -- CJK Compatibility Forms, Small Form Variants
      , (0xFF00, 0xFF60)   -- Fullwidth Forms
      , (0xFFE0, 0xFFE6)   -- Fullwidth signs
      , (0x1B000, 0x1B16F) -- Kana Supplement / Extended
      , (0x1F200, 0x1F251) -- Enclosed Ideographic Supplement
      , (0x20000, 0x3FFFD) -- CJK Unified Ideographs Extensions B–G
      ]

-- | Sanitize a name to a bare L4 identifier (record field / param).
sanitizeIdent :: String -> String
sanitizeIdent s = case map keep s of
  ""        -> "f"
  out@(c:_) -> if isAlpha c then out else 'f' : out
  where keep c = if isAlphaNum c || c == '_' then c else '_'

-- | PascalCase a (possibly multi-word) name for a record type.
pascal :: String -> String
pascal s = case concatMap cap (words (map sep s)) of
  ""           -> "Rec"
  p@(c:_)
    | isAlpha c -> p
    | otherwise -> 'R' : p
  where
    sep c = if c `elem` ("_-" :: String) then ' ' else c
    cap ""     = ""
    cap (c:cs) = toUpper c : cs
