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

import DMN.DecisionTable (getInputHeaders, getOutputHeaders, getCommentHeaders, outputOrder, evalTable)
import DMN.Diagnostic
import DMN.Regions (regionMap, noMatchRegions, regionAt, regionCount, regionInput, regionTopInput, firstRegionOf, rmColumns, cbBlocks, blockRep, blockTop, Region, Unsupported (..))
import DMN.Types
import Control.Applicative ((<|>))
import Data.Char (isAlpha, isAlphaNum, toUpper)
import Data.List (intercalate, isPrefixOf, nub, sort)
import Data.Maybe (isJust, isNothing, catMaybes, mapMaybe)
import DMN.Number (showNumPlain)
import Data.Scientific (Scientific)
import Text.Megaparsec.Unicode (isWideChar)

-- | Options governing L4 emission (see BUILD-SPEC §4.1).
--
-- There is no @wrapMaybe@ any more. It was the opt-in, off by default, that
-- rendered "no rule matched" as @NOTHING@, and @DECISIONS.md@ D-22 rule 3 made
-- that the default. What the option did beyond that, wrap a table that no input
-- can fall through as well, is what rule 3 rules out ("so total tables stay
-- bare"), and no caller ever set it, so it was removed rather than kept with its
-- meaning inverted. Whether a table's result is a @MAYBE@ is now decided by the
-- table: see 'noRuleMayMatch'.
data L4Opts = L4Opts
  { emitDitto     :: Bool    -- ^ collapse repeated tokens to @^@ (not yet active this milestone)
  , useElem       :: Bool    -- ^ multi-value cells as @elem … (LIST …)@; default is OR-of-EQUALS
  , defaultResult :: String
    -- ^ rendered L4 expr emitted after @OTHERWISE@ when the table has neither a
    -- catch-all row nor a declared default. A caller that sets it has said what
    -- "no rule matched" answers, so the result stays bare. Empty by default, and
    -- no caller sets it.
  , emitAsserts   :: Bool
    -- ^ after the table (after the whole file, under 'toL4File'), append one
    -- @#ASSERT@ per region of "DMN.Regions", saying that the emitted function at
    -- 'DMN.Regions.regionInput' equals what 'DMN.DecisionTable.evalTable' answers
    -- there, so that @l4 run@ cross-checks this emitter against the interpreter.
    -- See 'assertLines'. A table 'DMN.Regions.regionMap' cannot analyse gets one
    -- comment line saying why instead, never a sample.
  , assertCap     :: Int
    -- ^ the most regions of one table 'emitAsserts' asserts. Over it, that many
    -- are chosen evenly through the whole sequence of regions and a comment says
    -- how many there are. ASSUMED, NOT RULED (D-21 step 2).
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

-- | Default options: ditto on (BUILD-SPEC §3), no @elem@, no caller-supplied
-- @OTHERWISE@, no asserts (and a cap of 200 regions a table if they are asked
-- for).
defaultL4Opts :: L4Opts
defaultL4Opts = L4Opts
  { emitDitto     = True
  , useElem       = False
  , defaultResult = ""
  , emitAsserts   = False
  , assertCap     = 200
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
    preamble = unlines (containsHelperFor dts ++ concatMap declareEnum env)
    parts    = [ toL4Parts optsEnv dt | dt <- dts ]
    -- Every table's assertions come after every table: they need the sum types
    -- of the preamble and the functions of all the tables, and nothing else.
    body     = concat [ b ++ "\n" | (b, _) <- parts ]
            ++ concat [ unlines as ++ "\n" | (_, as) <- parts, not (null as) ]

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
        -- The §8.2.11 default output value renders through the same OTHERWISE
        -- as a catch-all row's cell, so it is gated identically: a wildcard
        -- slice there (a column with no declared default) would render "" under
        -- a sum-typed GIVETH just as a row wildcard would.
        dcells = [ concat (drop i (take (i+1) d)) | Just d <- [dtDefaultOutput dt] ]
  , not (any (elem FAnything) (cells ++ dcells))
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
toL4 opts dt = body ++ (if null asserts then "" else "\n" ++ unlines asserts)
  where (body, asserts) = toL4Parts opts dt

-- | 'toL4', in two pieces: the table's declarations and function, and the
-- @#ASSERT@ block 'emitAsserts' asks for (empty when it is not set).
--
-- The pieces are separate so that 'toL4File' can put every table's assertions
-- after every table, at the end of the file. L4's top level is order-independent
-- (an @#ASSERT@ above the function it calls typechecks and runs), but a
-- generated file reads better, and is easier to strip, with its assertions
-- together. They are built inside this function because they have to agree with
-- values computed here: whether the result is a @MAYBE@, the output columns as
-- the function types them, and the record constructor.
toL4Parts :: L4Opts -> DecisionTable -> (String, [String])
toL4Parts opts dt =
  case l4Refusal dt of
    Just why -> error ("DMN.Translate.L4: " ++ why)
    Nothing  ->
      ( unlines (recordDecls ++ [givenBlock opts ins givethType] ++ [fnHeader, "  BRANCH"] ++ armLines ++ [otherwiseLine])
      , if emitAsserts opts
          then assertLines opts dt AssertShape { shapeOuts = outs', shapeMulti = multiOut, shapeMk = mkName
                                               , shapeMaybe = maybeResult }
          else [] )
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
    -- 'maybeResult' below wraps the result type where an input may match no rule,
    -- and a table proven total promotes its last arm to the OTHERWISE instead.
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
      Nothing -> containsHelperFor [dt]
              ++ concat [ declareEnum ((raw, ms), raw)
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

    -- The result is `MAYBE T`, its arms `JUST v` and its OTHERWISE `NOTHING`,
    -- where some input may match no rule ('noRuleMayMatch', D-22 rule 3). The
    -- OTHERWISE is then reachable, and the typed sentinel it used to return
    -- (`0`, `""`, `FALSE`, `EMPTY`) was an answer the table never gave, at exit
    -- 0. A table no input can fall through stays bare, as the ruling says. A
    -- caller that supplied the OTHERWISE ('defaultResult') has said what no
    -- match answers, so the result stays bare then too.
    --
    -- A total table's OTHERWISE is dead, but it must still typecheck, and a
    -- sum-typed result is the one type with no value to put there: `""` is a
    -- check error, fabricating a member is forbidden by CLAUDE.md, and adding a
    -- sentinel member to the enum is worse still, because where one table's
    -- output domain is another's input domain it silently widens the OTHER
    -- table's declared domain. Until D-22 part 4 such a table, one with a
    -- sum-typed result, no catch-all row and no declared default, kept a
    -- `MAYBE T` whose NOTHING could never happen
    -- (policy/l4-priority-reorders-arms was one).
    --
    -- Part 4 (ASSUMED, NOT RULED) PROMOTES THE LAST ARM: when 'noRuleMayMatch'
    -- has proved the table total, the last arm of 'branchRows' supplies the
    -- OTHERWISE value and its guard is dropped ('promoted'). That is the same
    -- function for a total table: an input that reaches the OTHERWISE matched
    -- no earlier arm, and some arm matches it, so the last one does. "Last" is
    -- last in emission order, so under HP_Priority it is the last of the arms
    -- sorted by 'outputOrder'. Where the table is NOT proven total, the MAYBE
    -- stays: promoting an arm there would answer an unmatched input with a
    -- confident value the table never gave, which is what rule 3 removes.
    --
    -- 'needsValue' keys on `defaultRow`, not on `catchAll`, because since D-16
    -- phase 2 a table may carry a §8.2.11 default output value with no catch-all
    -- row (what the XML reader builds from <defaultOutputEntry>), and
    -- otherwiseExpr consults defaultRow, so this must too.
    mayFallThrough = noRuleMayMatch dt defaultRow branchRows
    needsValue     = null defaultResultStr && not (null enumOuts) && isNothing defaultRow
    -- The second disjunct is a guard that should never fire: with no arm and no
    -- default row the table has no rules, and a table with no rules is never
    -- proven total. If it ever did, there would be no arm to promote and the
    -- OTHERWISE would have no value of the type, so the answer is MAYBE.
    maybeResult = null defaultResultStr
               && (mayFallThrough || (needsValue && isNothing promoted))

    givethType
      | maybeResult = "MAYBE " ++ typeAtom baseGiveth
      | otherwise   = baseGiveth

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
    (branchRows, catchAll) = case hitpolicy dt of
      HP_Priority -> (outputOrder (header dt) (allrows dt), Nothing)
      _           -> case reverse (allrows dt) of
        (lastR : rest) | isCatchAll lastR -> (reverse rest, Just lastR)
        _                                 -> (allrows dt, Nothing)

    -- D-22 part 4: the arms that are rendered with a guard. Same as
    -- 'branchRows', less the last arm when a proven-total sum-typed table
    -- promotes it to the OTHERWISE ('promoted'). The ditto grid below is built
    -- from these alone, so dropping the last arm re-measures every column and
    -- THEN still lines up. Nothing in the grid refers forward, so the arms
    -- above the dropped one render as they would have.
    (armRows, promoted) = case reverse branchRows of
      (lastR : rest) | needsValue && not mayFallThrough -> (reverse rest, Just lastR)
      _                                                 -> (branchRows, Nothing)

    -- Render the arms through the ditto grid (BUILD-SPEC §3): one [Maybe Cell]
    -- per arm, column-aligned, with repeated guard tokens collapsed to ^ when
    -- emitDitto is set. Every returned guard line has the same width, so the
    -- trailing THEN keyword lines up across all arms.
    grid       = map (armCells opts ins) armRows
    guardLines = renderDittoGrid opts grid
    armLines   = zipWith mkArm guardLines armRows
    mkArm gl row =
      "    IF " ++ trueIfBlank gl ++ " THEN " ++ armResult maybeResult multiOut mkName outs' (row_outputs row)
                ++ commentSuffix (row_comments row)

    -- A vacuously-true guard (every input cell a wildcard — e.g. a non-trailing
    -- catch-all row) has an all-blank grid line; render it as TRUE, padded to the
    -- grid width so THEN stays aligned.
    trueIfBlank gl
      | all (== ' ') gl = rpad (length gl) "TRUE"
      | otherwise       = gl

    otherwiseLine = "    OTHERWISE " ++ otherwiseExpr maybeResult multiOut mkName outs' otherwiseRow defaultResultStr
                 ++ maybe "" (commentSuffix . row_comments) promoted
    -- The synthesized OTHERWISE only ever returns an EXPLICIT catch-all row's
    -- output (the all-wildcard row, when present) or the declared default. With
    -- neither, the table says nothing about unmatched inputs, so we must NOT
    -- fabricate a value from a data row (that would give unmatched inputs a
    -- confidently-wrong answer — BUILD-SPEC §1.5). Where an input can get there,
    -- it says NOTHING (D-22 rule 3, 'maybeResult'), unless the caller set
    -- L4Opts.defaultResult, which it then returns. Where none can, the table is
    -- total and the OTHERWISE is dead: a type-default sentinel (BUILD-SPEC
    -- §4.3) only has to typecheck, except for a sum-typed result, which has no
    -- sentinel, and which returns the value of the last arm ('promoted', D-22
    -- part 4). The one value taken from a data row is therefore the last arm's,
    -- for a table proven total, where it cannot be wrong.
    -- An explicit catch-all row wins over the table-level default only in the
    -- sense that the two cannot coexist honestly — a table with both has an
    -- unreachable default, and the catch-all is the statement nearer the rules.
    defaultRow = (row_outputs <$> catchAll) <|> dtDefaultOutput dt
    defaultResultStr = defaultResult opts
    -- The output row the OTHERWISE returns: `defaultRow`, else the promoted last
    -- arm's. A promoted arm is a real rule, not a catch-all, so its row comment
    -- goes with it (above).
    otherwiseRow = defaultRow <|> (row_outputs <$> promoted)

-- | Can some input reach the synthesized @OTHERWISE@ with no rule matching it?
-- @DECISIONS.md@ D-22 rule 3 renders exactly that case as @NOTHING@, under a
-- @MAYBE@ result, and leaves every other table bare.
--
-- Given the table, the output row its @OTHERWISE@ returns (a trailing catch-all
-- row, else the §8.2.11 default output value), and the rows its arms are made
-- from. In order:
--
-- * A default row: 'False'. The @OTHERWISE@ is the table's own answer for
--   every input no rule matches, which is §8.2.11's reading and D-22 rule 1's.
-- * An all-wildcard arm, which renders as @IF TRUE@: 'False'. It answers every
--   input that reaches it. Under @P@ it is sorted in among the other arms, and
--   under @F@ it need not be last, so it is not always a @defaultRow@.
-- * Otherwise 'DMN.Regions.regionMap' decides, by asking whether the table has
--   a no-match region. Its default is always 'NoDefault' here, because the two
--   cases above take every table it would give a default to.
-- * __Where 'regionMap' cannot analyse the table, 'True'.__
--   'DMN.Regions.UnsupportedKind' lists what it declines; the ones that reach
--   here include a collection column, a String cell holding FEEL test syntax
--   and a short row. For those, totality is not proven, and the two ways to be
--   wrong are not equal: a @MAYBE@ on a total table costs a caller an unwrap
--   whose @NOTHING@ never comes, while a bare result on a partial one is a
--   sentinel answer the table never gave, at exit 0, which is the defect rule 3
--   exists to remove.
noRuleMayMatch :: DecisionTable -> Maybe [[FEELexp]] -> [DTrow] -> Bool
noRuleMayMatch dt defaultRow armRows
  | isJust defaultRow       = False
  | any isCatchAll armRows  = False
  | otherwise               = either (const True) (not . null . noMatchRegions) (regionMap dt)

-- * Oracle assertions (D-21 step 2, BUILD-SPEC §7.4)

-- | What 'toL4Parts' knows about the function it has just emitted and an
-- assertion has to agree with. 'assertLines' reads these, and does not work them
-- out again, because a second derivation of 'shapeMaybe' would be a place for it
-- to drift from the first.
data AssertShape = AssertShape
  { shapeOuts  :: [ColHeader]
    -- ^ the output columns as the function types them: @outs'@ in 'toL4Parts',
    -- with a column that did not qualify for a sum type already blanked
  , shapeMulti :: Bool    -- ^ the result is a record built by 'shapeMk'
  , shapeMk    :: String  -- ^ that record's constructor helper
  , shapeMaybe :: Bool    -- ^ the result is a @MAYBE@ ('noRuleMayMatch')
  }

-- | One @#ASSERT@ per region of the table, saying that the emitted function
-- applied to the region's input equals what 'evalTable' answers there; and, in
-- a region with a Number block that names a higher point than its
-- representative, a second one at the highest such point.
--
-- __What the check can and cannot see.__ The expected value is the interpreter's:
-- which rule answers (hit policy, priority, the unique catch-all as a default,
-- a declared default), whether anything does ('NOTHING'), and the arithmetic of a
-- computed cell, which 'evalTable' has already evaluated to a literal and which
-- the emitter has rendered as L4 arithmetic. How a /value/ is spelled is shared
-- with the emitter ('renderOutCell', 'showValIn'), so a bug in the spelling of
-- a literal would be on both sides of the @EQUALS@ and is not what this finds.
--
-- __Regions are the unit__ (DMN.Regions): inside one every rule matches
-- throughout or not at all, so the interpreter gives the same rules at every
-- input of a region, and one input per region is every distinguishable behaviour
-- of the /table/. It is not every behaviour of the emitter's /function/, which
-- can be right at one input of a region and wrong at another: @<@ written for
-- @<=@ differs from it at the bound and nowhere else. A region has one
-- representative per column, the lowest value a cell names in the block, and a
-- block can name several, so the first of two inputs is 'regionInput' and the
-- second, where it differs, is 'regionTopInput': every column at the highest
-- value its block names. That reaches a closed upper bound, and the lower bound
-- already was the representative. A region's inputs stay at most two, not a
-- product of the columns', because one input moves every column at once.
--
-- It still cannot see a defect confined to an input that is neither a block's
-- representative nor its highest named value: the interior of a block, a named
-- point between two others in it, or an input in no region's block named by a
-- cell. Region @k@ is the @k@th of 'DMN.Regions.regions' (from 0).
--
-- __Nothing is dropped without a line saying so.__
--
-- * A table 'regionMap' cannot analyse gets one comment, with its reason, and no
--   assertions. There is no sampling fallback: a sample of a table whose regions
--   dmnmd cannot say would be a guess.
-- * Over 'assertCap' regions, 'assertPositions' picks regions, and a comment says
--   how many there are. 'assertCap' bounds the assertions, so where a block can
--   name a higher point a quarter of it is kept back for those second inputs,
--   and the first inputs are chosen from the rest.
-- * A second input that does not fit under 'assertCap' is left out, evenly
--   through the ones there are, and a comment says how many.
-- * A region whose input has no L4 spelling, or where 'evalTable' refuses (a
--   division by zero), gets a comment naming it and why.
-- * A region where the interpreter answers null and the function is not a
--   @MAYBE@ gets a comment and an @#ASSERT FALSE@, so that @l4 run@ reports it
--   as failed. The two cannot disagree once 'noRuleMayMatch' has proved the
--   table total, so this is the case that says it did not.
--
-- A second input is marked by a trailing comment, @-- region k, highest named
-- value@, and in the header's count of what was asserted.
--
-- A table that supplies its own 'defaultResult' gets a comment and no assertions,
-- since its no-match answer is the caller's and not the interpreter's.
assertLines :: L4Opts -> DecisionTable -> AssertShape -> [String]
assertLines opts dt sh
  | not (null (defaultResult opts)) =
      [ none "the caller supplied the OTHERWISE (defaultResult), so what an unmatched input answers is not the interpreter's" ]
  | null (shapeOuts sh) = [ none "the table has no output column" ]
  | otherwise = case regionMap dt of
      Left u   -> [ none (oneLine (dropPrefix ("table " ++ show (tableName dt) ++ ": ") (unsupportedMessage u))) ]
      Right rm ->
        let n         = regionCount rm
            cap       = toInteger (assertCap opts)
            -- A second input needs a slot of its own under the cap. When every region
            -- fits there is room for as many as are left over; when they do not, and
            -- a block could name a higher point, a quarter of the cap is kept back.
            mayTop    = any (\b -> blockTop b /= blockRep b) (concatMap cbBlocks (rmColumns rm))
            budget    | n <= cap || not mayTop = cap
                      | otherwise              = cap - cap `div` 4
            picks     = assertPositions budget n
                          (mapMaybe (firstRegionOf rm) [0 .. length (allrows dt) - 1])
            regs      = [ (k, regionAt rm k) | k <- picks ]
            primaries = [ (k, maybe (Skipped "no such region") (outcomeFor . regionInput) mr) | (k, mr) <- regs ]
            wanted    = [ (k, regionTopInput r) | (k, Just r) <- regs, regionTopInput r /= regionInput r ]
            room      = cap - toInteger (length picks)
            chosen    = spreadOut room wanted
            seconds   = [ (k, outcomeFor input) | (k, input) <- chosen ]
            linesFor k = concat [ outcomeLines tableLabel k False o | (k', o) <- primaries, k' == k ]
                      ++ concat [ outcomeLines tableLabel k True o | (k', o) <- seconds, k' == k ]
            nAsserted = length [ () | (_, o) <- primaries ++ seconds, isAsserted o ]
            nSecond   = length [ () | (_, o) <- seconds, isAsserted o ]
            nSkipped  = length [ () | (_, Skipped _) <- primaries ]
            nSkipped2 = length [ () | (_, Skipped _) <- seconds ]
            nLeftOut  = length wanted - length chosen
            sampled   = n > toInteger (length picks)
        in headerLines n nAsserted nSecond nSkipped nSkipped2 nLeftOut picks sampled
             ++ concatMap linesFor picks
  where
    tableLabel = "table " ++ quoted (tableName dt)
    prefix     = "-- emitAsserts: "
    none why   = prefix ++ "no asserts for " ++ tableLabel ++ ": " ++ why

    fn      = quoteVar (tableName dt)
    insH    = getInputHeaders (header dt)
    call as = unwords (fn : as)

    headerLines n nAsserted nSecond nSkipped nSkipped2 nLeftOut picks sampled =
      [ prefix ++ tableLabel ++ ": " ++ plural n "region" ++ ", " ++ show nAsserted ++ " asserted"
          ++ parenthesised
               ( [ show nSecond ++ " at the highest value that a cell names in a block, each marked" | nSecond > 0 ]
              ++ [ "assertCap is " ++ show (assertCap opts) ++ "; the regions asserted are the first region in which each"
                     ++ " of the first rules is live, and the rest evenly spaced through positions 0 .. "
                     ++ show (n - 1) ++ " of the sequence of regions, first and last included, so the other "
                     ++ show (n - toInteger (length picks)) ++ " regions are not asserted"
                 | sampled ] )
          ++ "." ]
      ++ [ prefix ++ tableLabel ++ ": " ++ show nSkipped ++ " of the chosen regions "
             ++ (if nSkipped == 1 then "is" else "are") ++ " not asserted, as each says below."
         | nSkipped > 0 ]
      ++ [ prefix ++ tableLabel ++ ": " ++ show nSkipped2 ++ " of the inputs at the highest value that a cell names "
             ++ (if nSkipped2 == 1 then "is" else "are") ++ " not asserted, as each says below."
         | nSkipped2 > 0 ]
      ++ [ prefix ++ tableLabel ++ ": " ++ plural (toInteger nLeftOut) "input" ++ " at the highest value that a cell names"
             ++ " could not be asserted for want of room under assertCap " ++ show (assertCap opts) ++ "."
         | nLeftOut > 0 ]

    parenthesised [] = ""
    parenthesised ps = " (" ++ intercalate "; " ps ++ ")"

    -- At most @room@ of the elements, evenly spaced through them, first and last
    -- included; all of them when they fit.
    spreadOut room xs
      | toInteger (length xs) <= room = xs
      | room <= 0                     = []
      | otherwise                     = [ xs !! fromInteger i | i <- assertPositions room (toInteger (length xs)) [] ]

    outcomeFor :: [FEELexp] -> Outcome
    outcomeFor input = case mapM (uncurry renderInputVal) (zip insH input) of
      Left why   -> Skipped why
      Right args -> case evalTable dt input of
        Left why -> Skipped ("the interpreter refuses this input: " ++ oneLine (takeWhile (/= '\n') why))
        Right []
          | shapeMaybe sh -> Asserted (call args ++ " EQUALS NOTHING")
          | otherwise     -> Diverged "the interpreter answers null here, but this function's result is not a MAYBE"
        Right [routs]
          | length routs /= length (shapeOuts sh) -> Skipped "the interpreter's row has a different number of cells from the output columns"
          | otherwise -> Asserted (call args ++ " EQUALS " ++ expectedValue sh routs)
        Right _ -> Skipped "the interpreter answered with more than one row under a single-hit policy"

-- | What 'assertLines' did with one region.
data Outcome
  = Asserted String        -- ^ the assertion, after the @#ASSERT@
  | Skipped String         -- ^ not asserted, and why
  | Diverged String        -- ^ asserted, and known false: the interpreter and the function disagree

isAsserted :: Outcome -> Bool
isAsserted = \case Asserted _ -> True; Diverged _ -> True; Skipped _ -> False

outcomeLines :: String -> Integer -> Bool -> Outcome -> [String]
outcomeLines tbl k second o = case o of
  Asserted a -> [ "#ASSERT " ++ a ++ marker ]
  Skipped why -> [ "-- emitAsserts: " ++ tbl ++ ": region " ++ show k ++ at ++ " not asserted: " ++ oneLine why ]
  Diverged why -> [ "-- emitAsserts: " ++ tbl ++ ": DIVERGENCE at region " ++ show k ++ at ++ ": " ++ oneLine why
                  , "#ASSERT FALSE" ++ marker ]
  where
    -- the second input of a region, at the highest value a cell names in a block
    marker = if second then "  -- region " ++ show k ++ ", highest named value" else ""
    at     = if second then " (at the highest value that a cell names in a block)" else ""

-- | The positions, among @n@ regions numbered from 0, that at most @cap@
-- assertions are made at, given the position of the first region in which each
-- rule is live (the @witnesses@, which 'DMN.Regions.firstRegionOf' computes).
--
-- All of them when they fit. Otherwise the witnesses of the first @cap \`div\` 2@
-- rules, so that a sample exercises the rules (a table of a million regions that
-- are almost all no-match would otherwise be sampled where no rule fires), and
-- the rest of the @cap@ evenly spaced through @0 .. n-1@, the first and last
-- included, rounded down. Sorted and without repeats, so a spaced position that
-- lands on a witness costs one fewer assertion, not a duplicate.
--
-- ASSUMED, NOT RULED (D-21 step 2): the cap, the even spread, and the witnesses
-- are each a choice. Taking the first @cap@ regions would be cheaper to describe
-- and wrong, since they all share their first column's first blocks.
assertPositions :: Integer -> Integer -> [Integer] -> [Integer]
assertPositions cap n witnesses
  | n <= cap  = [0 .. n - 1]
  | cap <= 0  = []
  | otherwise = sort (nub (chosen ++ spread))
  where
    chosen = take (fromInteger (cap `div` 2)) (nub witnesses)
    room   = cap - toInteger (length chosen)
    spread | room == 1 = [0]
           | otherwise = [ (i * (n - 1)) `div` (room - 1) | i <- [0 .. room - 1] ]

-- | A region's value for one input column, as an argument to the function, or
-- why it has none. A negative number is parenthesised, since @f -1@ is a
-- subtraction; a member of a declared domain is its constructor.
renderInputVal :: ColHeader -> FEELexp -> Either String String
renderInputVal ch = \case
  FNullary (VN n) | Just DMN_Number <- vartype ch -> Right (parenNeg (showNumL4 n))
  FNullary v@(VB _) | Just DMN_Boolean <- vartype ch -> Right (showValL4 v)
  FNullary v@(VS s)
    | isStringTyped (vartype ch) -> case enumCtorsOf ch of
        Just ms | s `notElem` ms -> Left (here ++ "the region's value " ++ showStrL4 s ++ " is not a member of the declared domain")
        _                         -> Right (showValIn ch v)
  other -> Left (here ++ "the region's value " ++ oneLine (show other) ++ " has no L4 spelling for the column's type")
  where
    here = "column " ++ quoted (varname ch) ++ ": "
    isStringTyped = \case Just DMN_String -> True; Nothing -> True; _ -> False
    parenNeg t = if "-" `isPrefixOf` t then "(" ++ t ++ ")" else t

-- | The value the function should answer, as the right-hand side of an
-- @EQUALS@: @NOTHING@ is handled by the caller; a match is the record or scalar
-- the interpreter's row spells, under @JUST@ when the result is a @MAYBE@.
expectedValue :: AssertShape -> [[FEELexp]] -> String
expectedValue sh routs = (if shapeMaybe sh then ("JUST " ++) else id) base'
  where
    cells = zipWith renderOutCell (shapeOuts sh) routs
    base' | shapeMulti sh = parenAtom (unwords (shapeMk sh : map parenAtom cells))
          | otherwise     = parenAtom (concat (take 1 cells))

-- | Parenthesise unless the text is already one token: a string literal, a
-- backticked constructor, or a word or number that does not start with a sign.
parenAtom :: String -> String
parenAtom t
  | isStringLit t || isCtor t = t
  | otherwise = case t of
      c : _ | ' ' `notElem` t && c /= '-' && c /= '(' -> t
      _                                               -> "(" ++ t ++ ")"
  where
    isStringLit = \case '"' : rest@(_ : _) -> last rest == '"'; _ -> False
    isCtor      = \case '`' : rest@(_ : _) -> last rest == '`' && length (filter (== '`') rest) == 1; _ -> False

-- | Text for the inside of a one-line L4 comment or an error message.
oneLine :: String -> String
oneLine = map (\c -> if c `elem` ("\r\n" :: String) then ' ' else c)

quoted :: String -> String
quoted s = "\"" ++ oneLine s ++ "\""

plural :: Integer -> String -> String
plural 1 w = "1 " ++ w
plural n w = show n ++ " " ++ w ++ "s"

dropPrefix :: String -> String -> String
dropPrefix p s = if p `isPrefixOf` s then drop (length p) s else s

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

-- | A data row is a catch-all when every input cell is the wildcard @-@.
isCatchAll :: DTrow -> Bool
isCatchAll row = all (all (== FAnything)) (row_inputs row)

-- | A cell that matches every value: the wildcard @-@, alone or among other
-- tests. The interpreter's 'DMN.DecisionTable.fEvals' is @or . map fEval@ and
-- 'DMN.DecisionTable.fEval' answers 'True' to the wildcard, so @4, -@ matches
-- every value, and @--to=ts@ renders it @(x === 4.0 || (true||x))@. This
-- backend used to filter the dash out and keep the 4, which narrowed the rule
-- to one value (@symptom\/l4-multivalue-dash-dropped@). An empty cell is the
-- same: nothing is left to test.
isWildCell :: [FEELexp] -> Bool
isWildCell cell = null cell || FAnything `elem` cell

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
    _ | isWildCell cell -> Nothing
    -- A collection column's cell is MEMBERSHIP, whatever the arity. The comma
    -- still means OR, so `clerk, teller` is "contains either".
    fxs | isListCol ch ->
            Just (parenIf (length fxs > 1)
                    (intercalate " OR " (map (contains field . feelValL4 ch) fxs)))
    [fx] -> Just (oneFeel ch field fx)
    fxs
      -- Was a bare `elem`, which is a prelude name and does not resolve without
      -- an IMPORT the emitter never wrote. Points at the self-contained helper
      -- instead, so the option stops emitting something that cannot typecheck.
      | useElem opts -> Just (contains field ("(LIST " ++ intercalate ", " (map (feelValL4 ch) fxs) ++ ")"))
      | otherwise    -> Just ("(" ++ intercalate " OR " (map (oneFeel ch field) fxs) ++ ")")
  where
    field = quoteVar (varname ch)
    contains xs x = containsFn ++ " " ++ xs ++ " " ++ x
    parenIf True  s = "(" ++ s ++ ")"
    parenIf False s = s

-- | The membership predicate emitted for collection columns.
--
-- __Self-contained, deliberately: no @IMPORT prelude@.__ l4's prelude does
-- define @elem@, but importing it drags ~100 names into scope, any of which can
-- collide with a column or table name taken from the author's markdown — and an
-- @IMPORT@ that fails to resolve makes every emitted file unusable rather than
-- just this guard. A nine-line definition costs nothing and cannot break.
--
-- Argument order is __FEEL's__ (@list contains(list, element)@), not prelude
-- @elem@'s reversed one, so a DMN reader reads the emitted guard forwards.
containsHelper :: [String]
containsHelper =
  [ "GIVEN a IS A TYPE"
  , "      xs IS A LIST OF a"
  , "      x IS AN a"
  , "GIVETH A BOOLEAN"
  , containsFn ++ " xs x MEANS"
  , "  CONSIDER xs"
  , "  WHEN EMPTY THEN FALSE"
  , "  WHEN y FOLLOWED BY ys THEN x EQUALS y OR " ++ containsFn ++ " ys x"
  , ""
  ]

-- | Backticked because it contains spaces; that also keeps it clear of every
-- identifier a markdown table could produce.
containsFn :: String
containsFn = "`dmnmd list contains`"

-- | The helper, but only when some table actually needs it.
containsHelperFor :: [DecisionTable] -> [String]
containsHelperFor dts
  | any (any isListCol . getInputHeaders . header) dts = containsHelper
  | otherwise                                          = []

-- | A single (non-multi) FEEL guard atom against a named field.
oneFeel :: ColHeader -> String -> FEELexp -> String
oneFeel ch field = \case
  FSection Feq  v      -> field ++ " EQUALS " ++ showValIn ch v
  FSection Flt  (VN n) -> field ++ " < "  ++ showNumL4 n
  FSection Flte (VN n) -> field ++ " <= " ++ showNumL4 n
  FSection Fgt  (VN n) -> field ++ " > "  ++ showNumL4 n
  FSection Fgte (VN n) -> field ++ " >= " ++ showNumL4 n
  FSection op   v      -> field ++ " EQUALS " ++ showValIn ch v   -- non-numeric comparison: best-effort
  FInRange lk lo hi rk -> "(" ++ field ++ geOf lk ++ showNumL4 lo
                          ++ " AND " ++ field ++ leOf rk ++ showNumL4 hi ++ ")"
    where geOf BClosed = " >= "
          geOf BOpen   = " > "
          leOf BClosed = " <= "
          leOf BOpen   = " < "
  FNullary v           -> field ++ " EQUALS " ++ showValIn ch v
  FFunction fnf        -> field ++ " EQUALS " ++ fnf2l4 fnf
  FAnything            -> "TRUE"
  -- Parenthesised TWICE, and both pairs are load-bearing in opposite directions.
  --
  -- The inner pair guards the operand: @FInRange@ expands to an @AND@ chain, so
  -- @NOT x <= 5 AND x <= 8@ must not read as @NOT x <= 5@ conjoined with the
  -- rest of the range.
  --
  -- The OUTER pair guards what follows, and this is the one that was missing.
  -- @NOT@ binds LOOSER than @AND@ in L4 — measured, not assumed:
  --
  -- >>> loose MEANS NOT (a) AND b        -- a=FALSE b=FALSE  ==>  TRUE
  -- >>> tight MEANS (NOT (a)) AND b      -- a=FALSE b=FALSE  ==>  FALSE
  --
  -- so @NOT (…)@ swallows every conjunct to its right. In a row with more than
  -- one input column, the unparenthesised form negates the WHOLE remaining
  -- guard: the emitted L4 typechecks, @l4 check@ succeeds, and the row matches
  -- inputs it must not — a silently inverted guard at exit 0, which is the exact
  -- class D-9 exists to remove, in the one backend nothing executes.
  --
  -- An earlier version of this comment asserted the opposite precedence and
  -- shipped only the inner pair. It was wrong; the two-line probe above is how
  -- to re-check it rather than reasoning about it. @l4 check@ cannot catch this
  -- — both forms typecheck — so only @l4 run@ or an emitted-code test will.
  -- @NOT@ is already in 'reservedWordsL4'.
  FNot inner           -> "(NOT (" ++ oneFeel ch field inner ++ "))"

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
    present = map (not . isWildCell) (row_inputs row)
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
    _ | isWildCell cell                           -> [Nothing, Nothing, Nothing]
    -- A collection column never takes the three-token split. Its guard is a
    -- CALL, `\`dmnmd list contains\` roles "admin"`, whose pieces are not a
    -- (field, operator, value) triple — splitting it would put the helper name
    -- in the operator column and misalign every other row. Going through
    -- 'feel2l4In' keeps the whole call in the field slot.
    --
    -- The cost is that collection guards never ditto: a `^` copies exactly one
    -- token and this is three. That is the accepted trade — ditto is a
    -- nice-to-have, and a wrong emission to preserve it would not be.
    _ | isListCol ch                              -> whole
    [fx] | Just (f, o, v) <- oneFeelCells ch field fx -> [Just f, Just o, Just v]
    _                                             -> whole
  where
    field = quoteVar (varname ch)
    whole = case feel2l4In opts ch cell of
      Just w  -> [Just w, Nothing, Nothing]
      Nothing -> [Nothing, Nothing, Nothing]

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

-- | The expression returned by one BRANCH arm; the first argument says whether
-- the result is a @MAYBE@ (D-22 rule 3).
armResult :: Bool -> Bool -> String -> [ColHeader] -> [[FEELexp]] -> String
armResult maybeResult multiOut mkName outs routs
  | maybeResult = "JUST " ++ parenWrap base
  | otherwise   = base
  where base = resultExpr multiOut mkName outs routs

-- | The expression returned by the synthesized @OTHERWISE@; the first argument
-- says whether the result is a @MAYBE@ (D-22 rule 3).
otherwiseExpr :: Bool -> Bool -> String -> [ColHeader] -> Maybe [[FEELexp]] -> String -> String
otherwiseExpr maybeResult multiOut mkName outs defaultRow defaultStr
  | maybeResult           = "NOTHING"
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
  -- A collection OUTPUT cell is a VALUE — the whole comma-separated list, not
  -- its head. This arm must come BEFORE the enum arm below, which is head-only
  -- and would otherwise win and silently drop every member after the first.
  cells | isListCol col -> case filter (/= FAnything) cells of
            []  -> "EMPTY"
            fxs -> "LIST " ++ intercalate ", " (map (showFeelL4 (elemType (vartype col))) fxs)
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
  FInRange _ lo _ _ -> showNumL4 lo
  FAnything      -> typeDefaultScalar ty
  -- Unreachable: 'DMN.DecisionTable.structuralErrors' refuses a negation in an
  -- output cell before any backend is reached, because it selects a set rather
  -- than naming a value. Loud rather than a guess — every other arm here has a
  -- value it can honestly return, and this one does not.
  FNot inner     -> error $ "dmn error: negation " ++ show (FNot inner)
                         ++ " reached the L4 output emitter; it should have been"
                         ++ " refused by structuralErrors as a unary test in an"
                         ++ " output cell."

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
  -- The empty list, not @""@, which is ill-typed under @GIVETH A LIST OF …@.
  Just (DMN_List _) -> "EMPTY"
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
  -- A 'VL' is a runtime argument to the interpreter and never reaches emission:
  -- no cell can hold one, and a collection OUTPUT cell is a list of ELEMENT
  -- values rendered by 'renderOutCell', not a single VL. Rendered anyway rather
  -- than left partial, since the L4 spelling is unambiguous.
  VL vs -> case vs of
    [] -> "EMPTY"
    _  -> "LIST " ++ intercalate ", " (showValL4 <$> vs)

-- | Double-quote a string literal, escaping backslashes and quotes.
showStrL4 :: String -> String
showStrL4 s = '"' : concatMap esc s ++ "\""
  where esc '"'  = "\\\""
        esc '\\' = "\\\\"
        esc c    = [c]

-- | Render a number without a spurious @.0@ (@9.0@ -> @"9"@) and without
-- scientific notation (@0.02@ -> @"0.02"@).
--
-- Now a one-line delegation to 'DMN.Number.showNumPlain', which is the whole
-- point of that module: the L4 backend and the diagnostics need the identical
-- spelling for the identical reason (a human reads it and expects the table's
-- own text), and they used to arrive at it by two different routes — this
-- function, and a bare @show@ in 'DMN.DecisionTable.showDomainMember' that did
-- not agree with it.
--
-- What was here before was @floatToDigits@, picking the shortest decimal that
-- round-trips a 'Float'. That could not be ported, and did not need to be:
-- 'floatToDigits' is a 'RealFloat' method and 'Scientific' has no such instance,
-- because a 'Scientific' /is/ its exact digits and there is no round trip to be
-- shortest about. The earlier @showFFloat (Just 6)@ that @floatToDigits@
-- replaced is still the cautionary tale — it collapsed @0.0000001@ to @0.0@
-- (BUILD-SPEC §1.2 / §9.5) — and 'showNumPlain' does not, for a stronger reason
-- than before: it is exact by construction rather than by digit count.
--
-- The @isNaN@/@isInfinite@ guards are gone. Their comment called them
-- "defensive: DMN cells never hold NaN/Inf", and that is now structurally true
-- rather than hopeful: 'Scientific' can represent neither. They were not
-- harmless while they lasted — @1 \/ 0@ produced @Infinity@ and this function
-- rendered it as the plain, innocent-looking @0@.
showNumL4 :: Scientific -> String
showNumL4 = showNumPlain

-- * Small helpers

-- | Wrap an expression in parens if it contains a space (so @JUST (mk …)@ binds).
parenWrap :: String -> String
parenWrap s = if ' ' `elem` s then "(" ++ s ++ ")" else s

-- | A type as the argument of @MAYBE@. @LIST OF T@ is the one multi-token type
-- this backend writes, and @MAYBE LIST OF STRING@ is a parse error in l4
-- (measured: "unexpected LIST"), so it is parenthesised. Every other type it
-- writes is one token: a keyword, a record name from 'pascal', or a sum-type
-- name through 'quoteVar', whose backticks make even a spaced name one token.
typeAtom :: String -> String
typeAtom t
  | "LIST OF " `isPrefixOf` t = "(" ++ t ++ ")"
  | otherwise                 = t

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
