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
import DMN.Diagnostic
import DMN.Types
import Data.Char (isAlpha, isAlphaNum, toUpper)
import Data.List (intercalate)
import Data.Maybe (isJust, isNothing, catMaybes, mapMaybe)
import Numeric (floatToDigits)
import Text.Megaparsec.Unicode (isWideChar)

-- | Options governing L4 emission (see BUILD-SPEC §4.1).
data L4Opts = L4Opts
  { emitDitto     :: Bool    -- ^ collapse repeated tokens to @^@ (not yet active this milestone)
  , useElem       :: Bool    -- ^ multi-value cells as @elem … (LIST …)@; default is OR-of-EQUALS
  , wrapMaybe     :: Bool    -- ^ @GIVETH A MAYBE …@, arms @JUST …@, @OTHERWISE NOTHING@
  , defaultResult :: String  -- ^ rendered L4 expr emitted after @OTHERWISE@ when no catch-all row
  , emitAsserts   :: Bool    -- ^ append @#EVAL@/@#ASSERT@ lines from the evalTable oracle (TODO)
  , enumEnv       :: Maybe EnumEnv
    -- ^ the file's sum-type name assignment. 'Nothing' means "this table is on
    -- its own": name types after my own columns and emit my own @DECLARE@s, which
    -- is what 'toL4' did before 'toL4File' existed and what @TranslateL4Spec@
    -- still exercises. 'Just' means the @DECLARE@s are already in the file
    -- preamble and this table must use the names agreed there.
  }

-- | @(column name, members) -> the L4 type name actually emitted.@
--
-- The key is the PAIR, not the name: two columns spelled alike with the same
-- members are one type, and with different members are two.
type EnumEnv = [((String, [String]), String)]

-- | Default options: ditto on (BUILD-SPEC §3), no @elem@, bare-typed @OTHERWISE@, no asserts.
defaultL4Opts :: L4Opts
defaultL4Opts = L4Opts
  { emitDitto     = True
  , useElem       = False
  , wrapMaybe     = False
  , defaultResult = ""
  , emitAsserts   = False
  , enumEnv       = Nothing
  }

-- * Entry points

-- | Transpile every table of one input file to a single L4 source text.
--
-- __The file, not the table, is the unit a correct L4 emitter works in.__ L4's
-- top-level scope is the whole file and is order-independent, so a @DECLARE@ one
-- table emits is visible to — and can collide with — every other table's. Two
-- tables that both declare a @category@ column over the same domain each emit
-- their own @DECLARE Category@, and @l4 check@ rejects the pair with
-- /multiple definitions for the identifier/ while @dmnmd@ exits 0
-- (@symptom\/l4-duplicate-declare-across-tables@).
--
-- 'toL4' is kept for a single table on its own — @TranslateL4Spec@ calls it in
-- 14 places, and a one-table file is exactly @toL4File@ over a singleton.
--
-- __Refusals are checked for the whole file before anything is emitted__, and a
-- single refused table suppresses the output of every other table. That is not
-- caution, it is the only way to keep a promise the single-table refusal cases
-- already make: @policy\/l4-refuses-outputorder@ pins that a refusal "emits
-- nothing whatsoever on stdout, so a redirect cannot leave a half-written file
-- behind". Emitting table by table cannot keep it, and did not — see
-- @symptom\/l4-multitable-refusal-partial-output-large@, where 4KB of a refused
-- run reached stdout. It was quiet only for outputs under one 2048-char buffer
-- chunk, which is a promise that holds for toy tables and breaks for real ones.
--
-- __Every sum type is declared ONCE, in a preamble__ ('assignEnumNames'), and
-- tables that genuinely share a domain share the L4 type — which is the point,
-- not a workaround: it is what makes @CardToUse (Categorize \"foodpanda\")@
-- typecheck with the two sides related rather than being unrelated @STRING@s.
toL4File :: L4Opts -> [DecisionTable] -> ([Diagnostic], String)
toL4File opts dts
  | not (null refusals) = (refusals, "")
  | otherwise           = (renameDiags, preamble ++ body)
  where
    refusals =
      [ errorAt ("table " ++ show (tableName dt) ++ ": " ++ why)
      | dt <- dts
      , Just why <- [l4Refusal dt] ]

    env      = assignEnumNames [ (enumRawName o, ms) | dt <- dts, (o, ms) <- enumColsOf dt ]
    optsEnv  = opts { enumEnv = Just env }
    preamble = unlines (concatMap declareEnum env)
    body     = concatMap (\dt -> toL4 optsEnv dt ++ "\n") dts

    -- A rename means two columns spelled the same declare DIFFERENT domains, so
    -- the reader is about to meet a type name that appears in no input file.
    renameDiags =
      [ warnAt $
          "two columns named " ++ show raw ++ " declare different domains, so their"
            ++ " L4 types cannot be the same type. The second is emitted as "
            ++ show emitted ++ ". Merging them would silently widen both domains,"
            ++ " which l4 cannot see and would accept."
      | ((raw, _), emitted) <- env
      , emitted /= raw ]

-- | Give every distinct declared domain in the file a unique L4 type name.
--
-- Keyed on the __pair__ (name, members), so the two cases separate cleanly:
--
--  * same name, same members — one entry, one @DECLARE@, and both tables refer to
--    it. Two tables sharing a domain SHOULD share a type.
--  * same name, different members — two entries; the second is renamed
--    @Category_2@. Verified against the real @l4@: two types with overlapping
--    constructor names resolve type-directedly at the use site and compute the
--    right answers.
--
-- __Merging the two domains into one type is not an option, and this was
-- measured rather than assumed.__ Give table A's output domain {Dining, Grocery}
-- and table B's input domain {Dining, Travel} a single merged type and @l4
-- check@ passes, @l4 run@ exits 0, and B confidently accepts a @Grocery@ it
-- declared it would not — assertion "satisfied". A widened domain is invisible to
-- l4, because after the merge it IS the declared domain. Two distinct types fail
-- loudly instead, naming both. That is the whole trade this backend exists to
-- make.
--
-- Distinct types with overlapping constructors are fine and need no mangling:
-- @DECLARE A IS ONE OF \`X\`, \`Y\`@ alongside @DECLARE B IS ONE OF \`X\`, \`Z\`@
-- typechecks and both @\`X\`@s resolve correctly.
assignEnumNames :: [(String, [String])] -> EnumEnv
assignEnumNames = go [] . nubOrd
  where
    go _ [] = []
    go used (k@(raw, _) : rest) =
      let name = fresh used raw
      in (k, name) : go (name : used) rest
    fresh used raw
      | raw `notElem` used = raw
      | otherwise = head [ cand | n <- [2 :: Int ..], let cand = raw ++ "_" ++ show n
                                , cand `notElem` used ]
    nubOrd = foldr (\x acc -> x : filter (/= x) acc) []

-- | One @DECLARE … IS ONE OF@ block, plus its trailing blank line.
declareEnum :: ((String, [String]), String) -> [String]
declareEnum ((_, ms), emitted) =
  ["DECLARE " ++ quoteVar emitted, "  IS ONE OF"] ++ map (("    " ++) . ctorL4) ms ++ [""]

-- | The output columns of a table that get a real L4 sum type, with their
-- members.
--
-- Hoisted out of 'toL4' so 'toL4File' can compute the file's whole set of
-- domains before any table is rendered. The gate is a property of the whole
-- column and both halves of it are about the @OTHERWISE@ rather than the type:
-- no cell may be a wildcard, since @-@ renders through 'typeDefaultScalar' to
-- @\"\"@, which is ill-typed under a sum-typed @GIVETH@.
enumOutsOf :: DecisionTable -> [(ColHeader, [String])]
enumOutsOf dt =
  [ (o, ms)
  | (i, o) <- zip [0 :: Int ..] (getOutputHeaders (header dt))
  , Just ms <- [enumCtorsOf o]
  , let cells = [ concat (drop i (take (i+1) (row_outputs r))) | r@DTrow{} <- allrows dt ]
  , not (any (elem FAnything) cells)
  , not (any null cells)
  ]

-- | The INPUT columns of a table that get a real L4 sum type, with their
-- members.
--
-- No cell gate, unlike 'enumOutsOf', and the asymmetry is real rather than an
-- oversight. An output wildcard has to render as SOMETHING and
-- 'typeDefaultScalar' gives it @""@, which is ill-typed under a sum-typed
-- GIVETH. An input wildcard renders as nothing at all — 'feel2l4In' drops the
-- conjunct — so it cannot be ill-typed. Every other cell on a @Just DMN_String@
-- column is an @FNullary (VS _)@ (§A.2), which is exactly a constructor.
enumInsOf :: DecisionTable -> [(ColHeader, [String])]
enumInsOf dt = [ (i, ms) | i <- getInputHeaders (header dt), Just ms <- [enumCtorsOf i] ]

-- | Every domained column of a table, inputs and outputs.
enumColsOf :: DecisionTable -> [(ColHeader, [String])]
enumColsOf dt = enumInsOf dt ++ enumOutsOf dt

-- | Why the L4 backend cannot emit this table, or 'Nothing' if it can.
--
-- All four refusals are the same cardinality mismatch: these hit policies are
-- list-valued (they return ALL matching rows, aggregated or ordered), and a
-- first-match @BRANCH@ returns a single scalar, so every match after the first
-- would be silently dropped. Refusing beats answering wrongly (BUILD-SPEC §1.6,
-- §9 #4); the gate lifts when a @LIST OF@ result exists.
--
-- Split out of 'toL4' so 'toL4File' can ask the question __without__ evaluating
-- the emission. The refusal used to be an @error@ raised part-way through
-- rendering, which meant the only way to find out a table was refused was to
-- start printing it.
l4Refusal :: DecisionTable -> Maybe String
l4Refusal dt = case hitpolicy dt of
  HP_Collect _   -> Just "Collect hit policy not yet supported by the L4 backend"
  HP_Aggregate   -> Just "Aggregate hit policy not yet supported by the L4 backend"
  HP_OutputOrder -> Just "OutputOrder hit policy not supported by the L4 backend (list-valued: returns all matches, not a single BRANCH result)"
  HP_RuleOrder   -> Just "RuleOrder hit policy not supported by the L4 backend (list-valued: returns all matches, not a single BRANCH result)"
  _              -> Nothing

-- | Transpile one decision table to L4 source text. First/Unique/Priority hit
-- policies map to a first-match @BRANCH@; the list-valued policies fail loudly
-- (BUILD-SPEC §1.6).
--
-- Still @error@s on a refusal, because it returns a bare 'String' and has no
-- other way to say no. Callers that can report a diagnostic should use
-- 'toL4File', which asks 'l4Refusal' first and never reaches this.
toL4 :: L4Opts -> DecisionTable -> String
toL4 opts dt =
  case l4Refusal dt of
    Just why -> error ("DMN.Translate.L4: " ++ why)
    Nothing  -> unlines (recordDecls ++ [givenBlock opts ins givethType] ++ [fnHeader, "  BRANCH"] ++ armLines ++ [otherwiseLine])
  where
    -- Parameters are renamed where they would capture a constructor. See
    -- 'renameParams' — this is the ONE place it happens, so everything
    -- downstream (the GIVEN block, every guard's field token, the ditto grid's
    -- widths) reads the renamed header and cannot disagree with itself.
    ins  = renameParams (ctorNames opts dt) (tableName dt) (getInputHeaders (header dt))
    outs = getOutputHeaders (header dt)

    -- Multiple output columns => a DECLAREd result record + a mk<Name> helper.
    multiOut    = length outs > 1
    recName     = pascal (tableName dt)
    mkName      = "mk" ++ recName

    -- Output columns that get a real L4 sum type instead of STRING.
    --
    -- A table with no catch-all row used to be excluded too, because the
    -- synthesized `OTHERWISE ""` is a *check error* against a sum-typed GIVETH
    -- and omitting OTHERWISE is a *parse error*. That restriction is gone:
    -- 'derivedMaybe' below wraps the result type instead.
    --
    -- The gate itself lives in 'enumOutsOf', because 'toL4File' has to apply
    -- exactly the same one when it collects the file's domains — a table whose
    -- column did not qualify must not contribute a DECLARE to the preamble.
    enumOuts = enumOutsOf dt

    -- The gate is applied ONCE, here, by blanking 'enums' on every output column
    -- that did not qualify. Everything downstream — 'renderOutCell',
    -- 'declareRecord', 'ctorHelper' — then asks 'enumCtorsOf' and gets the right
    -- answer without needing to know the gate exists.
    --
    -- The first version of this passed the gate to the DECLARE and the GIVETH but
    -- not to the cell renderer, which consults 'enumCtorsOf' directly. The result
    -- was `GIVETH A STRING` with arms returning `Stew` — a mixture that does not
    -- typecheck, caught by policy/l4-priority-reorders-arms.
    outs' = [ if o `elem` map fst enumOuts then o else o { enums = Nothing } | o <- outs ]

    -- Under a file environment the DECLAREs are already in the preamble, and
    -- repeating them here is precisely the duplicate-definition bug.
    enumDecls = case enumEnv opts of
      Just _  -> []
      Nothing -> concat [ declareEnum ((raw, ms), raw)
                        | (o, ms) <- nubOn (enumRawName . fst) (enumInsOf dt ++ enumOuts)
                        , let raw = enumRawName o ]
      where nubOn f = foldr (\x acc -> x : filter ((/= f x) . f) acc) []

    recordDecls
      | multiOut  = enumDecls ++ declareRecord opts recName outs' ++ [""] ++ ctorHelper opts recName mkName outs' ++ [""]
      | otherwise = enumDecls

    -- The function's GIVETH type: scalar for one output column, else the record.
    baseGiveth = case outs' of
      [o] -> colTypeL4 opts o
      _   -> recName

    -- A sum-typed result with nothing sensible to fall back on becomes
    -- `MAYBE T`, so "no rule matched" is NOTHING rather than a fabricated
    -- member. Fabricating one is forbidden by CLAUDE.md, and adding a sentinel
    -- to the enum is worse still: where one table's output domain is another's
    -- input domain, the sentinel silently widens the OTHER table's declared
    -- domain.
    --
    -- The condition mirrors 'otherwiseExpr' exactly: MAYBE is needed precisely
    -- when that function would fall through to 'typeDefaultL4', which is the
    -- branch that emits the ill-typed `""`. So it keys on `catchAll` and on
    -- `defaultResultStr`, the two things otherwiseExpr consults first.
    --
    -- An earlier attempt keyed on `any isCatchAll (allrows dt)` instead, to
    -- avoid handing HP_Priority a MAYBE it arguably does not need — Priority
    -- turns an all-wildcard row into a vacuously-true ARM, so the table is total
    -- and its OTHERWISE is unreachable. But Priority also hardwires
    -- catchAll = Nothing, so otherwiseExpr still took the typeDefaultL4 branch
    -- and emitted `OTHERWISE ""` under `GIVETH A Dish`. Caught by
    -- policy/l4-priority-reorders-arms.
    --
    -- The cost is that a total Priority table gets `MAYBE T`, so callers unwrap
    -- a result that can never be NOTHING. Fixing that means feeding the dead
    -- catch-all ARM's own result to otherwiseExpr, which changes non-enum
    -- Priority output too and is deliberately not bundled here.
    derivedMaybe = not (null enumOuts)
                && isNothing catchAll
                && null defaultResultStr

    -- SUBSUMES the caller's wrapMaybe rather than stacking with it: composed,
    -- the two emit `MAYBE MAYBE T`, whose arities do not match and which l4
    -- rejects.
    optsEff = opts { wrapMaybe = wrapMaybe opts || derivedMaybe }
    givethType
      | wrapMaybe optsEff = "MAYBE " ++ baseGiveth
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
      "    IF " ++ trueIfBlank gl ++ " THEN " ++ armResult optsEff multiOut mkName outs' (row_outputs row)
                ++ commentSuffix (row_comments row)

    -- A vacuously-true guard (every input cell a wildcard — e.g. a non-trailing
    -- catch-all row) has an all-blank grid line; render it as TRUE, padded to the
    -- grid width so THEN stays aligned.
    trueIfBlank gl
      | all (== ' ') gl = rpad (length gl) "TRUE"
      | otherwise       = gl

    otherwiseLine = "    OTHERWISE " ++ otherwiseExpr optsEff multiOut mkName outs' defaultRow defaultResultStr
    -- The synthesized OTHERWISE only ever returns an EXPLICIT catch-all row's
    -- output (the all-wildcard row, when present). With NO catch-all row the table
    -- says nothing about unmatched inputs, so we must NOT fabricate a value from a
    -- data row (that would give unmatched inputs a confidently-wrong answer —
    -- BUILD-SPEC §1.5). Fall through to L4Opts.defaultResult, else a type-default
    -- sentinel (BUILD-SPEC §4.3). For a faithful "no rule matched" use wrapMaybe.
    defaultRow = row_outputs <$> catchAll
    defaultResultStr = defaultResult opts

-- | A data row is a catch-all when every input cell is the wildcard @-@.
-- * Sum types for domained columns (BUILD-SPEC-dmnmd-l4-sumtype.md Part A)

-- | The constructors of a column that qualifies for an L4 sum type, else
-- 'Nothing'.
--
-- The boundary is narrow on purpose and it is airtight rather than merely
-- cautious: @mkFEither (Just DMN_String)@ ('DMN.DecisionTable') gives a String
-- column only @FNullary (VS _)@ cells, and only @FAnything@ besides. No
-- @FSection@ / @FInRange@ / @FFunction@ can appear on a candidate column, so
-- "what does @< 18@ become under an enum" cannot arise.
--
-- Numeric domains are excluded by l4, not by preference:
-- @DECLARE Bucket IS ONE OF 1, 2, 3@ is a parse error there.
enumCtorsOf :: ColHeader -> Maybe [String]
enumCtorsOf ch = case (vartype ch, enums ch) of
  (Just DMN_String, Just ms@(_:_)) -> traverse asVS ms
  _                                -> Nothing
  where
    asVS (FNullary (VS s)) = Just s
    asVS _                 = Nothing

-- | The L4 type name for a domained column: the column name in PascalCase.
--
-- Named after the column rather than after the DMN type because dmnmd has no
-- DMN type to name it after yet — @\<itemDefinition\>@ is still discarded
-- (BUILD-SPEC-dmnmd-l4-sumtype.md Part B). When that lands this should prefer
-- the author's own type name.
enumTypeNameOf :: L4Opts -> ColHeader -> String
enumTypeNameOf opts ch = quoteVar (emitted (enumRawName ch))
  where
    -- Under a file environment the name was agreed in the preamble; look it up
    -- by the (name, members) pair, because a same-named column with a DIFFERENT
    -- domain is a different type and was given a different name.
    emitted raw = case enumEnv opts of
      Nothing  -> raw
      Just env -> maybe raw id (lookup (raw, maybe [] id (enumCtorsOf ch)) env)

-- | A domained column's type name BEFORE the file-level uniquing runs.
enumRawName :: ColHeader -> String
enumRawName = pascal . varname

-- | Every domain member visible in this file, as l4 sees the name.
--
-- File-wide under 'toL4File', because l4's top level is; a table on its own can
-- only be captured by its own members.
ctorNames :: L4Opts -> DecisionTable -> [String]
ctorNames opts dt = case enumEnv opts of
  Just env -> concat [ ms | ((_, ms), _) <- env ]
  Nothing  -> concat [ ms | (_, ms) <- enumColsOf dt ]

-- | Rename input parameters that would capture a constructor (or each other, or
-- the function) by appending underscores until fresh.
--
-- __This is the only defence against the worst failure this backend has.__ A
-- @GIVEN@ parameter spelled like a constructor silently shadows it: the emitted
-- guard @IF Route EQUALS \`Route\`@ becomes a TAUTOLOGY, because both sides
-- resolve to the parameter, so the first arm fires for every input.
-- @l4 check@ exits 0 and says nothing. Measured — the design had claimed l4
-- catches this when the types differ, and it does not: NUMBER, STRING and an
-- unrelated sum type all typecheck clean and give the wrong answer, because
-- after capture there are no longer two things to disagree about.
--
-- The PARAMETER is renamed, never the constructor: a domain member is the
-- author's word and appears in their source document, while a parameter is a
-- binder we invented from the column name.
--
-- Compared on the RAW name, not the emitted token, because __backticks are
-- purely lexical in l4__ (measured: all four quoting combinations behave
-- identically). So bare @Dining@ and @\`Dining\`@ are the same identifier, and
-- comparing emitted tokens would miss exactly the collisions that matter.
--
-- A single underscore is not enough: a domain member literally named @cat_@
-- re-creates the capture, and @cat__@ defeats two. Hence a loop rather than a
-- suffix. Members are the author's words and can be anything.
renameParams :: [String] -> String -> [ColHeader] -> [ColHeader]
renameParams ctors fnName = go (fnName : ctors)
  where
    go _ [] = []
    go used (h:hs) =
      let nm = fresh used (varname h)
      in h { varname = nm } : go (nm : used) hs
    fresh used nm
      | nm `notElem` used = nm
      | otherwise         = fresh used (nm ++ "_")

-- | A domain member, as an L4 constructor. __Always backticked, never
-- 'quoteVar'__, and that is load-bearing twice over.
--
-- Firstly it keeps the ditto grid byte-identical: @\"Dining\"@ and
-- @`Dining`@ are both 8 display columns, whereas a bare @Dining@ is 6.
-- 'renderDittoGrid' resolves @^@ by __absolute source column__, so a width
-- change there makes a caret silently copy the wrong token — the failure this
-- whole backend is most careful about.
--
-- Secondly, domain members routinely are not identifiers: @README.md@'s own
-- Example 3 declares @LEVEL 2, LEVEL 1, NONE@.
ctorL4 :: String -> String
ctorL4 s = "`" ++ s ++ "`"

isCatchAll :: DTrow -> Bool
isCatchAll row = all (all (== FAnything)) (row_inputs row)

-- * The GIVEN/GIVETH block (BUILD-SPEC §1.1, §1.4)

-- | Emit the @GIVEN … IS A <type>@ lines plus the @GIVETH@ line. The GIVETH
-- type is computed by the caller (it needs the record name for multi-output).
givenBlock :: L4Opts -> [ColHeader] -> String -> String
givenBlock opts ins givethType = intercalate "\n" (givens ++ ["GIVETH A " ++ givethType])
  where
    nameW = maximum (0 : map (length . quoteVar . varname) ins)
    line lead h = lead ++ rpad nameW (quoteVar (varname h)) ++ " IS A " ++ colTypeL4 opts h
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

-- | A column's L4 type: its sum type if it has a usable declared domain, else
-- the plain scalar mapping. Used by the record declaration and its constructor
-- helper so a multi-output table's field types agree with its GIVETH.
colTypeL4 :: L4Opts -> ColHeader -> String
colTypeL4 opts h = maybe (type2l4 (vartype h)) (const (enumTypeNameOf opts h)) (enumCtorsOf h)

-- | @DECLARE <Name> HAS f1 IS A t1 …@
declareRecord :: L4Opts -> String -> [ColHeader] -> [String]
declareRecord opts recName outs =
  ("DECLARE " ++ recName ++ " HAS")
  : map (\h -> "    " ++ rpad fw (fieldName h) ++ " IS A " ++ colTypeL4 opts h) outs
  where fw = maximum (0 : map (length . fieldName) outs)

-- | The constructor helper: @mk<Name> v1 v2 … MEANS <Name> WITH …@ as a layout-sensitive block.
ctorHelper :: L4Opts -> String -> String -> [ColHeader] -> [String]
ctorHelper opts recName mkName outs =
  givens
  ++ ["GIVETH A " ++ recName]
  ++ [mkName ++ concatMap (\p -> " " ++ p) params ++ " MEANS " ++ recName ++ " WITH"]
  ++ zipWith (\h p -> "    " ++ rpad fw (fieldName h) ++ " IS " ++ p) outs params
  where
    params = ["v" ++ show i | i <- [1 .. length outs]]
    fw     = maximum (0 : map (length . fieldName) outs)
    tw     = maximum (0 : map (length . colTypeL4 opts) outs)
    givens = case zip outs params of
      []            -> []
      ((h0,p0):rest) -> ("GIVEN " ++ pdecl h0 p0) : map (\(h,p) -> "      " ++ pdecl h p) rest
    -- colTypeL4, NOT type2l4: the parameter types must agree with the FIELD
    -- types that 'declareRecord' emitted, and a domained column's field is its
    -- sum type. Emitting STRING here produced `GIVEN v1 IS A STRING` against
    -- `dish IS A Dish`, and l4 reported the mismatch as an AMBIGUITY on the
    -- record name rather than as a type error — because `Plan` names both the
    -- record constructor and the decision function, and with the parameter types
    -- wrong neither candidate fits, so l4 could not say which was meant.
    pdecl h p = rpad pw p ++ " IS A " ++ colTypeL4 opts h
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
    [fx] -> Just (oneFeel ch field fx)
    fxs
      | useElem opts -> Just ("elem " ++ field ++ " (LIST " ++ intercalate ", " (map (feelValL4 ch) fxs) ++ ")")
      | otherwise    -> Just ("(" ++ intercalate " OR " (map (oneFeel ch field) fxs) ++ ")")
  where field = quoteVar (varname ch)

-- | A single (non-multi) FEEL guard atom against a named field.
oneFeel :: ColHeader -> String -> FEELexp -> String
oneFeel ch field = \case
  FSection Feq  v      -> field ++ " EQUALS " ++ showValIn ch v
  FSection Flt  (VN n) -> field ++ " < "  ++ showNumL4 n
  FSection Flte (VN n) -> field ++ " <= " ++ showNumL4 n
  FSection Fgt  (VN n) -> field ++ " > "  ++ showNumL4 n
  FSection Fgte (VN n) -> field ++ " >= " ++ showNumL4 n
  FSection op   v      -> field ++ " EQUALS " ++ showValIn ch v   -- non-numeric comparison: best-effort
  FInRange lo hi       -> "(" ++ field ++ " >= " ++ showNumL4 lo ++ " AND " ++ field ++ " <= " ++ showNumL4 hi ++ ")"
  FNullary v           -> field ++ " EQUALS " ++ showValIn ch v
  FFunction fnf        -> field ++ " EQUALS " ++ fnf2l4 fnf
  FAnything            -> "TRUE"

-- | The bare value used inside an @elem … (LIST …)@ membership list.
feelValL4 :: ColHeader -> FEELexp -> String
feelValL4 ch = \case
  FNullary v      -> showValIn ch v
  FSection Feq v  -> showValIn ch v
  FFunction fnf   -> fnf2l4 fnf
  other           -> oneFeel ch "?" other

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
    [fx] | Just (f, o, v) <- oneFeelCells ch field fx -> [Just f, Just o, Just v]
    _    -> case feel2l4In opts ch cell of
              Just whole -> [Just whole, Nothing, Nothing]
              Nothing    -> [Nothing, Nothing, Nothing]
  where field = quoteVar (varname ch)

-- | Split a simple guard atom into its (field, operator, value) tokens. 'Nothing'
-- for anything that is not a single field-op-value comparison (a range or wildcard
-- is rendered whole by 'conjSubCells'). The joined tokens reproduce 'oneFeel'.
oneFeelCells :: ColHeader -> String -> FEELexp -> Maybe (String, String, String)
oneFeelCells ch field = \case
  FSection Feq  v      -> Just (field, "EQUALS", showValIn ch v)
  FSection Flt  (VN n) -> Just (field, "<",  showNumL4 n)
  FSection Flte (VN n) -> Just (field, "<=", showNumL4 n)
  FSection Fgt  (VN n) -> Just (field, ">",  showNumL4 n)
  FSection Fgte (VN n) -> Just (field, ">=", showNumL4 n)
  FSection _    v      -> Just (field, "EQUALS", showValIn ch v)
  FNullary v           -> Just (field, "EQUALS", showValIn ch v)
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
  -- A domained column's values are constructors of its own sum type, not
  -- strings. Everything else about the column renders as before.
  (FNullary (VS s) : _) | Just ms <- enumCtorsOf col, s `elem` ms -> ctorL4 s
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

-- | Render a DMN value AS SEEN FROM a particular column: a member of that
-- column's declared domain is a CONSTRUCTOR of its sum type, everything else is
-- a plain literal.
--
-- The single place a guard value and an output value agree on what a domained
-- cell means. 'renderOutCell' asks the same question on the output side; keeping
-- both in terms of 'enumCtorsOf' is what stops the two sides drifting into
-- @IF cat EQUALS \"Dining\"@ against @GIVEN cat IS A Cat@, which does not
-- typecheck.
showValIn :: ColHeader -> DMNVal -> String
showValIn ch v@(VS s)
  | Just ms <- enumCtorsOf ch, s `elem` ms = ctorL4 s
  | otherwise                              = showValL4 v
showValIn _ v = showValL4 v

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

-- | Display width of a string in L4 lexer columns. The lexer advances columns
-- via megaparsec's @TraversableStream Text@ instance, whose per-char step is
-- @if isWideChar c then 2 else 1@ ('Text.Megaparsec.Unicode', @since@ megaparsec
-- 9.7.0). We defer to that EXACT function rather than a parallel East_Asian_Width
-- table, so the emitted ditto grid measures columns identically to the lexer that
-- resolves @^@. (A prior hand-rolled table silently diverged on the BMP
-- pictographs megaparsec counts as wide — U+231A ⌚, U+2728 ✨, U+2B50 ⭐, … —
-- placing them at width 1 and misaligning ditto.)
displayWidth :: String -> Int
displayWidth = sum . map (\c -> if isWideChar c then 2 else 1)

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
