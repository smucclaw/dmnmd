{-# LANGUAGE LambdaCase, RecordWildCards, NoOverloadedStrings #-}

module DMN.DecisionTable where

{-| Given an ASCII decision table, parse it and transpile it to operational languages like JS and Python. -}

import Control.Arrow ( (<<<), (>>>) )
import Prelude hiding (takeWhile)
import DMN.ParseCell ( parseNumberCell, thousandsGrouped, namedRefusal )
import Data.List (intercalate, dropWhileEnd, transpose, nub, sortOn, sortBy, elemIndex, find, isInfixOf)
import Data.List.Split ( splitOn )
import Data.Maybe ( catMaybes, fromJust, listToMaybe )
import Data.Char (toLower, isDigit)
import Text.Read (readMaybe)
import Debug.Trace ( trace )
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Scientific (Scientific)
import DMN.Number ( divideFeel, powerFeel, showNumPlain, spellable )
import DMN.ParsingUtils ( parseOnly )
import DMN.Diagnostic ( Diagnostic, anyErrors, errorAt )
import DMN.Types

-- main = do
--     putStrLn $ show example1_dish
--     putStrLn $ show $ evalTable example1_dish [VS "Fall"]

evalTable :: DecisionTable -> [FEELexp] -> Either String [[[FEELexp]]]
evalTable table given_input = do
  let symtab = Map.fromList $ zip (varname <$> filter ((DTCH_In==).label) (header table)) given_input
      matched = filter ((given_input `matches`) . row_inputs) (datarows table)
  -- evaluate any FFunctions. This traverses in the Either monad rather than
  -- mapping purely, because arithmetic can now fail with something to say: a
  -- division by zero has no decimal answer (see 'DMN.Number.divideFeel'), and
  -- under 'Float' it silently produced @Infinity@ at exit 0 for every backend
  -- to print.
  outputs <- traverse (\row -> (\os -> row { row_outputs = os })
                               <$> traverse (evalFunctions symtab) (row_outputs row)) matched
  -- §8.2.11: the default output value answers when NO rule matches, and only
  -- then — so it is consulted here, after matching, never merged into the rows.
  -- 'rowsPlusDefault' (the backends' materialisation) must not be used in this
  -- function: under Any a materialised always-matching row would collide with a
  -- real match, and under Collect it would contribute to every result.
  -- Single-hit policies only; a Collect with no matching row already has its
  -- answer (the empty collection), and a default would quietly replace it.
  evaledDefault <- traverse (traverse (evalFunctions symtab)) (dtDefaultOutput table)
  let singleHit = hitpolicy table `elem` [HP_Unique, HP_Any, HP_First, HP_Priority]
  case (null outputs, evaledDefault) of
   (True, Just d) | singleHit -> Right [d]
   _ -> case hitpolicy table of
    HP_Unique -> case length outputs of
                   0 -> Left "no rows returned -- a unique table should have one result!"
                   1 -> Right (row_outputs <$> outputs)
                   _ -> Left $ "multiple rows returned -- this was supposed to be a unique table!\n" ++ show outputs
    -- ANY is single-hit. DMN allows several rows to match, but requires them to
    -- agree; if they do, the table has exactly ONE answer, and if they do not it
    -- is ill-defined. So the arm dispatches on the nub, which says both things at
    -- once and needs no partial function to read the survivor out.
    --
    -- Both halves of this were wrong and had to be fixed together (D-5). The
    -- guard was @not (null (nub …))@, which is never false once the empty case
    -- has been split off, so every A table took the conflict branch. And the
    -- success branch was @row_outputs <$> outputs@ — one entry per matched row,
    -- which is the LIST-VALUED shape HP_OutputOrder, HP_RuleOrder and
    -- HP_Collect_All use — so repairing only the guard would have printed the
    -- answer once per matching row, app/Main.hs emitting one line per element.
    -- Pinned from both sides by @eval-hp-any-two-rows-{agree,disagree}@.
    HP_Any    -> case nub (row_outputs <$> outputs) of
                   []       -> Left "no rows returned"
                   [agreed] -> Right [agreed]
                   _        -> Left ("multiple distinct rows returned -- an Any lookup may return multiple matches but they should all be the same!\n" ++ show outputs)
    HP_Priority    -> Right [row_outputs $ head0 table (outputOrder (header table) outputs)]
    HP_First       -> Right [row_outputs $ head0 table outputs]
    HP_OutputOrder -> Right (row_outputs <$> outputOrder (header table) outputs) -- order according to enums in subheaders.
    HP_RuleOrder   -> Right (row_outputs <$> sortOn row_number outputs)
    HP_Collect Collect_All -> trace ("outputs has length " ++ show (length outputs)) $ Right (row_outputs <$> outputs)
    HP_Collect Collect_Cnt -> trace ("outputs has length " ++ show (length outputs)) $ Right [[[FNullary (VN (fromIntegral (length outputs) :: Scientific))]]]
    HP_Collect Collect_Min -> Right [[[FNullary (VN (minimum $ [ x | (FNullary (VN x)) <- concat $ concat (row_outputs <$> outputs) ]))]]]
    HP_Collect Collect_Max -> Right [[[FNullary (VN (maximum $ [ x | (FNullary (VN x)) <- concat $ concat (row_outputs <$> outputs) ]))]]]
    HP_Collect Collect_Sum -> Right [[[FNullary (VN (    sum $ [ x | (FNullary (VN x)) <- concat $ concat (row_outputs <$> outputs) ]))]]]
    _ -> Left ("don't know how to evaluate hit policy " ++ show (hitpolicy table))
  where
    evalFunctions :: SymbolTable -> [FEELexp] -> Either String [FEELexp]
    evalFunctions symtab = traverse $ \case
      FFunction f -> FNullary <$> fNEval symtab f
      x           -> pure x

head0 :: DecisionTable -> [p] -> p
head0 dt mylist = if not (null mylist) then head mylist else
  error $ "dmn error: table " ++ tableName dt ++ " expected at least one row to match, but none did; hit policy " ++ show (hitpolicy dt) ++ " unable to operate."

outputOrder :: [ColHeader] -> [DTrow] -> [DTrow]
outputOrder chs =
  sortBy (mySort chs)
  -- we want to sort the dtrows by the enums left to right
  -- for instance, each DTrow in [DTrow] contain row_outputs; each [FEELexp] in row_outputs belongs to a column ColHeader which has an enums :: Maybe [FEELexp]; that [FEELexp] list is sorted.
  -- we know that the [FEELexp] in row_outputs contains 0 or 1 FEELexps.
  -- so we need to compute, for use by sortOn, a map from each value of the nums [FEELexp], to its elemIndex; and if two elements have the same index, we recursively sort using the next column, until we are able to return an Ordering LT EQ GT
  -- so, we want to sort the list of DTrows on 


mySort :: [ColHeader] -> DTrow -> DTrow -> Ordering
mySort colhs rowa rowb =
  let colenums = enums <$> filter ((DTCH_Out==).label) colhs
  in myS colenums (row_outputs rowa) (row_outputs rowb)
  -- return a function that cursors through all the output rows sorting the FEELexps by the enums in the respective ColHeaders until able to return LT EQ GT

myS :: (Eq a, Show a) => [Maybe [a]] -> [[a]] -> [[a]] -> Ordering
myS orders as bs = -- trace ("myS: will compare " ++ (show as) ++ " with " ++ (show bs))
                   firstNonEQ "  " $ zipWith3 sortCol orders as bs

sortCol :: (Eq a, Show a) => Maybe [a] -> [a] -> [a] -> Ordering
sortCol colenum cola colb = -- trace ("sortCol: comparing the elements within " ++ (show cola) ++ " and " ++ (show colb))
                            maybe EQ (\enumlist -> firstNonEQ "    " $ zipWith (sortCell enumlist) cola colb) colenum
sortCell :: (Eq a, Show a) => [a] -> a -> a -> Ordering
sortCell cellenums a b = -- trace ("  sortCell: comparing the locations of elements " ++ (show a) ++ " and " ++ (show b) ++ " in " ++ (show cellenums))
                         compare (elemIndex a cellenums) (elemIndex b cellenums)
firstNonEQ :: String -> [Ordering] -> Ordering
firstNonEQ spaces args = let remaining = dropWhile (EQ==) args
                         in -- trace (spaces ++ "firstNonEQ: returning first non-EQ element of " ++ (show args))
                            (if null remaining then EQ else head remaining)


matches :: [FEELexp] -> [[FEELexp]] -> Bool
-- matches tableInput testInput = 
matches inpts tableInputs = all (==True) $ zipWith fEvals inpts tableInputs


fe2dval :: FEELexp -> DMNVal
fe2dval (FNullary dmnval) = dmnval
fe2dval fexp = error ("fe2dval can't extract a DMNVal from " ++ show fexp)

-- static analysis phase, input validation of table, should identify scenarios where the variable name does not exist in the input props.
-- feel like we should convert this to a ReaderT so the symtab gets hidden?
--
-- Returns 'Either' rather than a bare 'DMNVal' because arithmetic over a decimal
-- has two answers a binary float did not: a division by zero has no @Infinity@
-- to fall into, and @**@ has no meaning at a fractional exponent. Both used to
-- be silent — @1 \/ 0@ gave @Infinity@ at exit 0, which the L4 backend then
-- printed as a plain @0@ — so they are refusals now, and they travel up through
-- 'evalTable'\'s existing @Left@ channel, which names the table and the input.
fNEval :: SymbolTable -> FNumFunction -> Either String DMNVal
fNEval _      (FNF0 dmnval)  = Right dmnval
fNEval symtab (FNF1 varname) =
  maybe (Left $ "function unable to resolve variable " ++ varname) (Right . fe2dval)
        (Map.lookup varname symtab)
fNEval symtab (FNF3 fnf1 fnop2 fnf3) = do
  lhs <- fromVN =<< fNEval symtab fnf1
  rhs <- fromVN =<< fNEval symtab fnf3
  -- @*@, @+@ and @-@ over 'Scientific' are exact decimal arithmetic, which is
  -- the point: a Collect Sum over cent-precision cells no longer accumulates
  -- binary error. @/@ and @**@ are the two that need a stated policy — see
  -- 'divideFeel' and 'powerFeel', which carry it.
  VN <$> case fnop2 of
    FNMul   -> Right (lhs * rhs)
    FNPlus  -> Right (lhs + rhs)
    FNMinus -> Right (lhs - rhs)
    FNDiv   -> located (divideFeel lhs rhs)
    FNExp   -> located (powerFeel  lhs rhs)
  where
    located = either (Left . (++ " — in " ++ showFNumFunction (FNF3 fnf1 fnop2 fnf3))) Right

-- | The located-message house style, in one place: @column "C": row N: @, or
-- @column "C": @ when there is no row.
--
-- Deliberately carries NO table name and no @error:@ prefix. That is the rule
-- recorded above 'domainErrors'' @msg@: each reader frames a diagnostic its own
-- way — the markdown path prepends @error: table "X": @ on its way to @error@,
-- the XML path hands it to 'DMN.XML.XmlToDmnmd.errorAt'. This function exists so
-- that the three places printing that prefix ('locate', @msg@, 'showSite')
-- cannot drift apart; it is not an invitation to give the shared locators a
-- table name.
columnRow :: String -> Maybe Int -> String
columnRow col rn = concat
  [ "column ", show col, maybe "" (\n -> ": row " ++ show n) rn, ": " ]

-- | Where a cell came from, for a diagnostic raised while building it.
--
-- Three fields, because that is exactly what the house style prints: a table
-- name plus 'columnRow'. There is deliberately no kind word — 'structuralErrors'
-- says "the input cell reads" in the message BODY and leaves the prefix alone,
-- and a second spelling of the same prefix in the same binary is how a house
-- style stops being one. (The kind word in 'DMN.XML.XmlToDmnmd' is that
-- reader's own convention, and it does not follow that this one wants it.)
--
-- What this location does NOT promise, all three of them already recorded as
-- symptoms:
--
--   * __uniqueness__ — two columns may share a name
--     (@symptom\/struct-dup-column-names@), two headings may clean to the same
--     table name (@symptom\/struct-tablename-collision@), and an unheaded table
--     is called @f1@ with every unheaded table in the first file getting that
--     same name (@symptom\/struct-no-heading-default-name@). Nothing rejects a
--     repeated rule number either. The message names what the author wrote; it
--     does not guarantee that only one cell answers to it.
--
--   * __a file name__ — still not here, and now it does not need to be.
--     'DMN.ParseTable.parseTableD' is not given one; its caller is
--     (@ParseMarkdown.parseChunk@ binds @infile@), and since D-7 that caller
--     prefixes every 'Diagnostic' coming back with it, exactly as it already
--     did for the two sibling diagnostics either side of the call. So the
--     markdown path DOES print a file name — this haddock recorded the
--     file-less state as "a deferral with a named cost", and the cost has been
--     paid at the right layer. Putting it in the 'CellSite' would be wrong: the
--     cell layer is shared with the XML reader, which has its own single file
--     name to attach and attaches it at its own top level.
data CellSite = CellSite
  { siteTable  :: String
  , siteColumn :: String
    -- | The rule number the author WROTE in the leftmost cell — not an index,
    -- which is why gaps and repeats survive into the message and why it agrees
    -- with the row comment @--to=ts@ emits. 'Nothing' is the sub-header row,
    -- which has no rule number; it renders as no row segment at all, exactly as
    -- 'domainErrors' already renders a sub-header complaint. (The XML reader
    -- stores a 1-based position in the same 'DTrow' field, so the two readers
    -- mean different things by "row".)
  , siteRow    :: Maybe Int
  }
  deriving (Show, Eq)

-- | A 'CellSite' rendered in the house style, ready to prefix a cell message.
--
-- Carries the @table "T": @ framing but __not__ the @error: @ word, which
-- 'DMN.Diagnostic.renderDiagnostic' supplies — the same division of labour the
-- XML reader already uses, where @convTable@ builds @inTable@ and lets
-- 'errorAt' say how bad it is. It used to include @error: @ itself, because it
-- was prefixing a message that went straight to GHC's @error@ and had nobody
-- else to frame it.
--
-- Do not make 'locate' or the 'domainErrors' @msg@ call this — those are shared
-- with the XML reader, which frames them itself, and handing them a table name
-- would change every XML diagnostic. The shared part is 'columnRow'.
showSite :: CellSite -> String
showSite st = concat
  [ "table ", show (siteTable st), ": ", columnRow (siteColumn st) (siteRow st) ]

mkFs :: Maybe DMNType -> String -> [FEELexp]
mkFs dmntype args = either error id (mkFsEither dmntype args)

-- | 'mkFs', with the cell's location attached to any refusal — as a returned
-- 'Diagnostic', which is D-7.
--
-- CLAUDE.md's bar for a diagnostic is loud AND located, and this used to meet
-- the second half only: it prepended the site and then reported through GHC's
-- @error@, so a markdown refusal arrived with a Haskell @CallStack@ and a
-- four-frame @HasCallStack backtrace:@ of ghc-internal positions attached. One
-- rule, two mechanisms. Now there is one mechanism, and it is the XML reader's.
--
-- Pass 1 of two. This is the entry point for __every__ markdown cell — the
-- sub-header row and every data cell, single- and multi-valued alike. 'mkFAt' is
-- pass 2, the type-inference re-pass. (This haddock, @CLAUDE.md@,
-- @test\/corpus\/README.md@ and @run-corpus.sh@ all used to call the two "the
-- multi-value and single-value cell paths"; that is retracted, and inverted in
-- the very pair cited as its proof.)
--
-- 'mkFs' stays for the test suite. It no longer stays "for the XML reader": its
-- two 'DMN.XML.XmlToDmnmd' call sites were the last unlocated user-facing
-- @error@ in the tool and now go through 'mkFsEither' with the locator those
-- sites already had.
mkFsAt :: CellSite -> Maybe DMNType -> String -> Either Diagnostic [FEELexp]
mkFsAt st dmntype args = locateCell st (mkFsEither dmntype args)

-- | The one place a cell-layer 'Left' becomes a located 'Diagnostic'.
--
-- Shared by 'mkFsAt' and 'mkFAt' so the two cannot drift, which is the failure
-- the retracted "keep them at distinct source lines" rule was gesturing at.
locateCell :: CellSite -> Either String a -> Either Diagnostic a
locateCell st = either (Left . errorAt . (showSite st ++)) Right

-- | 'mkFs' as a total function.
--
-- The markdown reader is happy to die on a malformed cell: it has already told
-- the user which file and table it was reading. The XML reader is not — it has
-- to name the table, column and rule, and refuse just that table. So the real
-- work lives in 'mkFEither'; 'mkF' is the @error@-ing wrapper, retained for the
-- test suite and for XML call sites that frame their own message. Since D-7 the
-- markdown path does NOT go through it — it uses 'mkFsAt'/'mkFAt', which return
-- a located 'Diagnostic'. Do not reintroduce a second copy of these guards elsewhere:
-- a validator that drifts from the constructor is worse than no validator.
mkFsEither :: Maybe DMNType -> String -> Either String [FEELexp]
mkFsEither dmntype args
  -- Checked BEFORE the split and independently of the column type, because the
  -- split happens before anything knows the type — a String column is shredded
  -- identically. See 'DMN.ParseCell.thousandsGrouped'.
  | thousandsGrouped args = Left (thousandsMsg args)
  | otherwise = traverse (mkFEither dmntype) (unquoteCell (trim <$> splitOn "," args))

-- | Note what this message does NOT say: that the old parse was wrong.
--
-- It was not. FEEL has no grouping production — rule 31 admits no separator and
-- rules 32-33 are @digit = [0-9]@ and @digits = digit , {digit}@ — so @1,000@ is
-- legally TWO unary tests, @1@ and @000@, and @000@ is a well-formed literal
-- denoting 0. dmnmd's old output for @>= 1,000@,
-- @(Amount >= 1.0 || Amount === 0.0)@, was therefore /conformant/, and the two
-- corpus recordings that called it a misparse were wrong about the parse.
--
-- What was unambiguously wrong is the SILENCE. Nobody writing @1,000@ in a
-- threshold column means "at least 1, or exactly 0". So dmnmd refuses the shape
-- rather than inventing a thousands separator — which would be a markdown-surface
-- extension governed by BUILD-SPEC-dmnmd-extensions.md §6, and could not be added
-- without breaking policy/md-multivalue-cell and test/golden/miles-card-dmn.md,
-- whose @4111, 4112@ is the same shape.
thousandsMsg :: String -> String
thousandsMsg args = concat
  [ "the cell reads ", show (trim args)
  , " — FEEL has no thousands separator (DMN 1.3 §9.2 rule 31 admits none, and"
  , " rules 32-33 are digit = [0-9] and digits = digit , {digit}), so a comma in a"
  , " cell is rule 11's OR. Written out, this cell means its comma-separated parts"
  , " as ALTERNATIVES, which is almost certainly not what was meant."
  , " Drop the separator (1000), or, if you really do mean two alternatives,"
  , " put a space after the comma (1, 000)." ]


-- TODO: add a state monad to allow type inference to span all input rows;
-- if any row contains a string, that entire column becomes a string not a num;
-- if all the columns contain nums or bools, then they're that;
-- but we can only make that decision after viewing the entire table.
-- maybe we use a multi-pass strategy ... where we allow the cells to remain untyped ... and then we review the entire table
-- after it's been fully parsed once.
mkF :: Maybe DMNType -> String -> FEELexp
mkF dmntype arg = either error id (mkFEither dmntype arg)

-- | The type-inference re-pass's cell wrapper: 'reprocessRows' calls this when
-- a column's type arrives after pass 1 has already read the cell as a string.
-- 'mkFsAt' is the pass-1 wrapper and handles every markdown cell; this one is
-- not "the single-value path", which is what this haddock used to claim — see
-- 'mkFsAt' for the retraction.
mkFAt :: CellSite -> Maybe DMNType -> String -> Either Diagnostic FEELexp
mkFAt st dmntype arg = locateCell st (mkFEither dmntype arg)

-- | The single definition of "what does this cell mean, given this column type".
--
-- Every failure is a 'Left' carrying a message that names the offending text;
-- 'mkF' turns those into @error@ (unchanged markdown behaviour), the XML reader
-- turns them into a located diagnostic. In particular the numeric literals go
-- through 'readMaybe', so a Number column holding @not("Fall"@ no longer dies
-- with a bare @Prelude.read: no parse@.
mkFEither :: Maybe DMNType -> String -> Either String FEELexp
mkFEither _ ""  = Right FAnything
mkFEither _ "_" = Right FAnything
mkFEither _ "-" = Right FAnything
-- in a numeric column, an FFunction is detected by the presence of an numeric operator
-- A collection column's cell is parsed at its ELEMENT type: `5` in a
-- @[Number]@ column is the same 'FNullary' it would be in a @Number@ column.
-- What differs is what that cell MEANS — membership rather than equality —
-- and that lives in the column type, which every consumer can already see. So
-- 'FEELexp' gains nothing here.
--
-- __Do not turn this arm into a 'Left'.__ It is the obvious way to make an
-- ambiguous list cell refuse, and it crashes on ordinary tables:
-- 'reprocessRows' calls @mkFAt (vartype ch)@ with the FULL column type on two
-- live paths — a list-typed OUTPUT column's cells, and 'retypeEnums' rebuilding
-- a declared domain. Since D-7 a 'Left' there no longer aborts the process; it
-- becomes an Error diagnostic, which means the table is not emitted. That is a
-- better failure but it is the same bug: a legitimate @[0..150]@ domain would
-- be refused rather than crashed. Refusals belong in
-- 'structuralErrors', which walks 'allrows' and can therefore see which row,
-- which column, and whether the cell is an input, an output or a domain member.
mkFEither (Just (DMN_List t)) x    = mkFEither (Just t) x
mkFEither Nothing  arg1 = -- trace ("mkF Nothing shouldn't happen -- type inference should have found some type for this column. coercing to string: " ++ arg1)
  Right (FNullary (VS (trim arg1)))

-- strings are tricky because they could be FEEL expression variable names like "Dish Name"
-- or just literal strings like "Lentil Soup"

mkFEither (Just DMN_String)  arg1 = Right (FNullary (VS (trim arg1)))
mkFEither (Just DMN_Boolean) arg1 = FNullary <$> mkVB arg1
  where
    -- The false list used to read ["false","no","t","y","negative"] — a
    -- copy-paste of the true list's short forms, so "t" and "y" were dead there
    -- (the true guard catches them first) and "f" and "n" appeared in NEITHER
    -- list. So `n` was accepted by inference as a boolean word and then failed to
    -- build, at exit 1, on both the inferred and the declared path.
    -- Recorded as infer-boolean-n-crash and infer-declared-boolean-n-crash.
    -- This is not D-2: inference resolved a y/n column to Boolean, and it was
    -- RIGHT to. The vocabulary just has to agree with itself.
    mkVB arg
      | (toLower <$> arg) `elem` ["true","yes","t","y","positive"] = Right (VB True)
      | (toLower <$> arg) `elem` ["false","no","f","n","negative"] = Right (VB False)
      | otherwise = Left $  "unable to parse an alleged boolean: " ++ arg
-- The nineteen lines this replaces were not a grammar but a chain of six
-- mutually blind string tests over the same raw text, so the accepted language
-- was whatever fell out of the guard ORDERING. See "DMN.ParseCell", which is
-- that language written down once, anchored, with the rule numbers.
mkFEither (Just DMN_Number)  arg1 = parseNumberCell arg1

-- | Parse a runtime ARGUMENT — as opposed to a table cell, which is a TEST.
--
-- 'mkFEither' builds a test: @>= 5@ in a cell is a section that matches a range
-- of values. That is exactly wrong for something typed at a prompt, where
-- @>= 5@ is not a value at all, and the @-q@ REPL accepting it has always been
-- a category error. This is the value-side counterpart, and it is the only
-- producer of 'VL'.
--
-- A collection is written the way FEEL writes one, @[a, b, c]@, with @[]@ for
-- the empty collection. The split is bracket-depth aware, so it does not shred
-- a nested literal the way a bare @splitOn ","@ would.
mkInputValue :: Maybe DMNType -> String -> Either String FEELexp
mkInputValue ty raw = FNullary <$> go ty (trim raw)
  where
    go (Just (DMN_List t)) s = case stripBrackets s of
      Just inner
        | null (trim inner) -> Right (VL [])
        | otherwise         -> VL <$> traverse (go (Just t)) (splitArgs inner)
      Nothing -> Left $ concat
        [ "this column is a collection, so its value must be written [a, b, c]"
        , " (or [] for none) — got ", show s ]
    -- The accepted language moves with the type, in both directions, and both
    -- are improvements. @read \@Float@ accepted @0x10@, @0o17@, @Infinity@ and
    -- @NaN@ — all four of which 'DMN.ParseCell'\'s own refusal message already
    -- names as things a FEEL number is not — and @read \@Scientific@ refuses
    -- them. It does accept @+5@, which rule 31 does not have; that is the one
    -- new divergence and it is a value at the prompt, not a cell in a table.
    -- 'spellable' is the magnitude guard: @read@ is happy to build @1e1000000@,
    -- and printing it would allocate a million digits.
    go (Just DMN_Number) s = case readMaybe (trim s) :: Maybe Scientific of
      Nothing              -> Left $ "expected a number, got " ++ show s
      Just n | spellable n -> Right (VN n)
             | otherwise   -> Left $ "number is too large to represent: " ++ show s
    go (Just DMN_Boolean) s
      | (toLower <$> trim s) `elem` ["true","yes","t","y"]  = Right (VB True)
      | (toLower <$> trim s) `elem` ["false","no","f","n"]  = Right (VB False)
      | otherwise = Left $ "expected a boolean, got " ++ show s
    go _ s = Right (VS (head (unquoteCell [trim s])))

-- | @[a, b]@ to @a, b@. 'Nothing' when the text is not bracketed at all.
stripBrackets :: String -> Maybe String
stripBrackets s = case trim s of
  ('[':rest) | not (null rest), last rest == ']' -> Just (init rest)
  _                                              -> Nothing

-- | Split on commas at bracket depth zero.
--
-- The REPL's own @splitOn ","@ predates types and shreds @[1,2,3]@ into three
-- arguments before anything knows the column is a collection. This does not.
-- It governs the ARGUMENT split only; a table cell's comma still always means
-- OR, and 'mkFsEither' is untouched.
splitArgs :: String -> [String]
splitArgs = go 0 ""
  where
    go :: Int -> String -> String -> [String]
    go _ acc []       = [reverse acc]
    go d acc (c:cs)
      | c == ',' && d == 0 = reverse acc : go d "" cs
      | c `elem` ("[(" :: String) = go (d + 1) (c:acc) cs
      | c `elem` ("])" :: String) = go (d - 1) (c:acc) cs
      | otherwise                 = go d (c:acc) cs

-- | Unwrap S-FEEL string literals across a whole cell, __all or nothing__.
--
-- @"Fall"@ denotes the four-character value @Fall@; the quotes are not part of
-- it. This matters well beyond hand-written markdown, because DMN XML writes
-- every string quoted — @\<text\>"adult"\<\/text\>@ — so before this, any table
-- arriving through @-f xml@ acquired cells that compiled to a comparison
-- against the quote characters, and could only match input that literally
-- contained quotes. Silent, and exit 0. See BUILD-SPEC-dmnmd-e4.md §2.2.
--
-- __Why all-or-nothing, and do not change this to per-fragment.__ 'mkFsEither'
-- splits a cell on commas before anything looks at what the cell means, so the
-- single FEEL negation @not("Fall", "Winter", "Spring", "Summer")@ arrives here
-- already shredded into four fragments, of which the middle two happen to be
-- well-formed literals and the outer two carry unbalanced quotes and
-- parentheses. Unquoting each fragment on its own merits yields
-- @not("Fall@ \/ @Winter@ \/ @Spring@ \/ @"Summer")@ — a mixture that is
-- neither the source text nor a parse of it, and that reads as half-parsed.
--
-- That has been tried and reverted once already; the warning is recorded at
-- @test\/DmnXmlSpec.hs@ above the frozen expectation for that very cell. So a
-- cell is unquoted only when __every__ fragment is a well-formed literal, which
-- keeps the honest cases (@"Fall"@, and a genuine multi-value @"Fall", "Winter"@)
-- and leaves a shredded one verbatim. Fixing the shred is the comma-split
-- defect, recorded at @test\/corpus\/cases\/symptom\/xml-comma-split-negation@.
--
-- 'isSFeelLiteral' is deliberately conservative, since it sees every cell:
--
--  * @5' 10"@ ends with a quote but does not start with one.
--  * @\"a\" and \"b\"@ starts and ends with a quote but is not one literal, so
--    the interior-quote test rejects it rather than yielding @a" and "b@.
--  * a lone @\"@ is length 1. ('inferType' does classify it as DMN_String,
--    since its @head == last@ test is satisfied by the same character, so this
--    case is reachable.)
--  * @\"\"@ is the empty string, which is correct.
--
-- Escape sequences are not interpreted; a cell containing @\\\"@ keeps it. That
-- is a gap, not a decision — it needs the real S-FEEL grammar, not a special
-- case here.
unquoteCell :: [String] -> [String]
unquoteCell frags
  | not (null frags), all isSFeelLiteral frags = unquote <$> frags
  | otherwise                                  = frags
  where
    unquote s = drop 1 (init s)

isSFeelLiteral :: String -> Bool
isSFeelLiteral s =
  length s >= 2 && head s == '"' && last s == '"' && '"' `notElem` drop 1 (init s)

fromVN :: DMNVal -> Either String Scientific
fromVN (VN n)     = Right n
fromVN (VB True)  = Right 1
fromVN (VB False) = Right 0
fromVN v = Left ("type error: tried to read a number out of " ++ show v)

trim :: String -> String
trim = dropWhile (==' ') . dropWhileEnd (==' ')
trimLeft :: String -> String
trimLeft = dropWhile (==' ')
trimRight :: String -> String
trimRight = dropWhileEnd (==' ')
 
fEvals :: FEELexp -> [FEELexp] -> Bool
fEvals arg exps = or $ (`fEval` arg) <$> exps

-- recognize ? as a placeholder for the current input value, in case the FEEL expression is more complex.
-- reinventing Ord and Eq typeclasses here. we also deal with "< x" vs "x <" -- we canonicalize order to "op val"
-- column in table -> input parameter -> is there a match?
fEval :: FEELexp -> FEELexp -> Bool
fEval FAnything    _                  = True
-- §9.2 rule 12.b. Placed FIRST among the scalar arms, and before the collection
-- arms deliberately: negating a membership test is meaningful ("the collection
-- does not contain this"), so the recursive call should reach them rather than
-- be pre-empted.
--
-- This arm is what stops 'FNot' from being a silent wrong answer. Without it a
-- negated cell parses, is emitted by all four backends, and then never matches
-- anything at run time — the catch-all below would raise a "type error in …"
-- naming an expression the author wrote correctly. 'domainErrors' also decides
-- membership through 'fEval', so a declared domain constrains a negated cell for
-- free, by the same rule CLAUDE.md gives for every other cell shape.
fEval (FNot test) val                 = not (fEval test val)
-- A collection ARGUMENT against a plain cell is MEMBERSHIP — the same meaning
-- the four backends emit (`.includes`, ` in `, `dmnmd list contains`). Placed
-- after the FAnything arm, so `-` still matches any collection including the
-- empty one, and before every scalar arm, so a VL cannot reach the error
-- catch-all below.
--
-- Membership over the empty collection is False, uniformly: node
-- `[].includes(x)` is false, python `x in []` is False, and the emitted L4
-- helper answers FALSE for EMPTY. There is no vacuous-truth case anywhere,
-- because dmnmd has no universal test to have one.
fEval (FNullary v) (FNullary (VL vs)) = v `elem` vs
-- A collection where a scalar test expects a scalar. Loud, and naming both
-- sides: silently answering False would be a wrong answer that exits 0.
fEval test (FNullary (VL vs)) = error $ unwords
  [ "type error: the value supplied is a collection,", showDomainMember (FNullary (VL vs))
  , ", but the cell", show (showDomainMember test), "is not a membership test."
  , "A collection argument belongs to a column declared [T]." ]
fEval (FNullary (VL vs)) val = error $ unwords
  [ "type error: the cell holds a collection,", showDomainMember (FNullary (VL vs))
  , ", which cannot happen — a collection is a runtime argument, never a cell."
  , "Supplied value was", show (showDomainMember val) ]
  -- alternative phrasing without arrows: (snd . fromJust . (find ((== f) . fst))
fEval (FSection f    (VN rhs)) (FNullary (VN lhs)) = (find ((== f) <<< fst) >>> fromJust >>> snd)
                                                      [(Flt,(<)), (Flte,(<=)), (Fgt,(>)), (Fgte,(>=)), (Feq,(==))]
                                                     lhs rhs
fEval (FInRange lk lower upper rk) (FNullary (VN lhs)) =
  cmp lk lower lhs && cmp rk lhs upper
  where cmp BClosed a b = a <= b
        cmp BOpen   a b = a <  b
fEval (FSection Feq  (VB rhs)) (FNullary (VB lhs)) = lhs == rhs
fEval (FSection Feq  (VS rhs)) (FNullary (VS lhs)) = lhs == rhs
fEval (FNullary (VS rhs)) (FNullary (VS lhs)) = lhs == rhs
fEval (FNullary (VB rhs)) (FNullary (VB lhs)) = lhs == rhs
fEval (FNullary (VN rhs)) (FNullary (VN lhs)) = lhs == rhs
fEval rhs lhs                                 = error $ unwords [ "type error in ", show lhs, " ~ ", show rhs]


-- From the S-FEEL specification:
-- Given an expression o to be tested and two endpoint e1 and e2:
--  is in the interval (e1..e2), also notated ]e1..e2[, if and only if o > e1 and o < e2
--  is in the interval (e1..e2], also notated ]e1..e2], if and only if o > e1 and o ≤ e2
--  is in the interval [e1..e2] if and only if o ≥ e1 and o ≤ e2
--  is in the interval [e1..e2), also notated [e1..e2[, if and only if o ≥ e1 and o < e2
-- An expression to be tested satisfies an instance of simple unary tests (grammar rule 12) if and only if, either the
-- expression is a list and the expression satisfies at least one simple unitary test in the list, or the simple unitary tests is “-”.
-- 


-- perform type inference to resolve colheader values based on a review of the rows
-- | Type inference, re-typing, and every reason to refuse the result — as
-- @([Diagnostic], 0-or-1 tables)@, the same shape
-- 'DMN.XML.XmlToDmnmd.convTable' has always returned.
--
-- __The list is the gate.__ An 'DMN.Diagnostic.Error' means the table list is
-- empty, so a caller cannot emit a table it was told to refuse merely by
-- forgetting to look at the diagnostics. That property used to be supplied by
-- @error@ — and, at the CLI, by the accident that @app\/Main.hs@'s
-- @tableWarnings@ loop and @--pick@'s @tableName@ filter both forced every
-- table to WHNF before anything was written. Both accidents are gone; this is
-- the design that replaces them.
--
-- __Cell diagnostics short-circuit 'tableErrors'.__ Same reasoning as
-- 'tableErrors' running 'structuralErrors' first: a cell whose meaning we could
-- not read cannot meaningfully be checked against a domain, and the follow-on
-- complaints would be about the placeholder 'reprocessRows' left behind rather
-- than about anything the author wrote.
mkDTable :: String -> HitPolicy -> [ColHeader] -> [DTrow] -> ([Diagnostic], [DecisionTable])
mkDTable origname orighp origchs origdtrows =
--  Debug.Trace.trace ("mkDTable: starting; origchs = " ++ show origchs) $
  let newchs   = zipWith inferTypes (getInputHeaders origchs ++ getOutputHeaders origchs)
                                     (transpose $ [ row_inputs r ++  row_outputs r | r@DTrow{} <- origdtrows])
      (enumDiags, typedchs) =
        (\pairs -> (concatMap fst pairs, snd <$> pairs))
          (retypeEnums origname <$> (if not (null newchs) then newchs ++ getCommentHeaders origchs else origchs))
      rowResults =
        (\case
            (DTrow rn ri ro rc) ->
              let (di, ri') = reprocessRows origname rn (getInputHeaders typedchs)  ri
                  (dobs, ro') = reprocessRows origname rn (getOutputHeaders typedchs) ro
              in (di ++ dobs, DTrow rn ri' ro' rc)) <$> origdtrows
      cellDiags = enumDiags ++ concatMap fst rowResults
      built = DTable origname orighp typedchs (snd <$> rowResults) Nothing
  in -- Debug.Trace.trace ("mkDTable: finishing...\n" ++
        --                 "origchs = " ++ show(origchs) ++ "\n" ++
           --             "newchs = " ++ show(newchs) ++ "\n" )
    -- A cell outside the domain its own sub-header row declares is a typo, not
    -- a new domain member, and a rule built from it can never match. Emitting it
    -- would be a silently-widened table that exits 0. See BUILD-SPEC-dmnmd-e4.md
    -- §8. The XML reader calls 'domainErrors' directly, because 'convTable'
    -- bypasses this function on purpose; both readers now report the same way.
    if anyErrors cellDiags
    then (cellDiags, [])
    else case tableErrors built of
      []   -> (cellDiags, [built])
      errs -> ( cellDiags ++ (errorAt . (("table " ++ show origname ++ ": ") ++) <$> errs)
              , [] )

-- | Every reason to refuse a table, in one place, for both readers.
--
-- 'structuralErrors' is about the table's SHAPE — a cell whose meaning dmnmd
-- will not guess at. 'domainErrors' is about a cell disagreeing with the domain
-- the table itself declares. Structural first, because a cell that has no
-- meaning cannot meaningfully be checked against a domain.
tableErrors :: DecisionTable -> [String]
tableErrors dt = structuralErrors dt ++ inferenceErrors dt ++ domainErrors dt
                 ++ uniquenessErrors dt

-- | D-13. Two rows of a @U@ table with identical guards: the second can never
-- match, and every backend emits it as dead code at exit 0.
--
-- __What a guard is.__ A row's guard is its whole input side —
-- @row_inputs :: [[FEELexp]]@, every input column conjoined. Two rows collide
-- only when EVERY input cell corresponds; one matching column is not a
-- duplicate. The output side is irrelevant: two rows with the same guard have a
-- dead row whether or not their outputs agree.
--
-- __Parsed values, not source text.__ Comparison is on the 'FEELexp' the parser
-- built, which is why @1.1@ and @1.10@ collide — 'Scientific'\'s 'Eq' compares
-- on value and ignores scale, so both cells are @FNullary (VN 1.1)@ and are not
-- even distinguishable by 'show'. A source-text comparison would miss the
-- recorded symptom outright.
--
-- __A multi-value cell is a set.__ 'fEvals' is @or . map (fEval arg)@, so
-- @Fall, Winter@ and @Winter, Fall@ select exactly the same rows, as do @Fall@
-- and @Fall, Fall@. Cells are therefore compared by mutual containment rather
-- than by list 'Eq' — deliberately, so the static check agrees with the
-- runtime matcher. (Nothing in the tree exercises this, measured; it is decided
-- on the semantics, not on a fixture.)
--
-- __Soundness, and what is deliberately NOT here.__ 'fEval' dispatches on
-- constructor structure alone, so equal guards imply identical matching
-- behaviour for every input: a static refusal here can never contradict
-- 'evalTable'\'s @HP_Unique@ arm, which reports the same collision at run time
-- as "multiple rows returned". The check is an under-approximation — @[1..5]@
-- and @[3..8]@ overlap without being equal and are NOT refused. That is full
-- overlap analysis, deferred by name in D-13.
--
-- __@U@ only.__ @A@ legitimately permits overlapping rows that agree (D-5) and
-- @P@, @O@, @R@ and @Collect@ order or accumulate them on purpose;
-- @policy\/struct-outputorder-enum-honoured@ is an @O@ table with two
-- all-wildcard rows whose outputs are both live.
--
-- __Zero input columns.__ Then every guard is the empty conjunction and every
-- pair of rows is vacuously identical. Unreachable from markdown
-- ('DMN.ParseTable.reviseInOut' guarantees an input column) but legal DMN, so
-- the check declines rather than refusing every input-less table.
uniquenessErrors :: DecisionTable -> [String]
uniquenessErrors dt = case hitpolicy dt of
  HP_Unique | not (null ins) ->
    [ dupMsg earlier (i, r)
    | (i, r) <- numbered
    , earlier <- take 1 [ e | e@(j, p) <- numbered
                            , j < i
                            , sameGuard (row_inputs p) (row_inputs r) ]
    ]
  _ -> []
  where
    ins      = getInputHeaders (header dt)
    numbered = zip [1 :: Int ..] [ r | r@DTrow{} <- allrows dt ]

    sameGuard as bs = length as == length bs && and (zipWith sameCell as bs)
    -- Mutual containment: a multi-value cell is an OR, hence a set. No 'Ord'
    -- instance exists for 'FEELexp' and cells are tiny, so this is not sorted.
    sameCell a b = all (`elem` b) a && all (`elem` a) b

    -- The row numbers are the ones the AUTHOR wrote on the markdown path
    -- (gaps and repeats survive) and a 1-based index on the XML path — the
    -- existing documented divergence, matched here rather than replaced. A
    -- blank rule number has no author number to print, so the position is
    -- named as a position and says so.
    rowLabel (i, r) = case row_number r of
      Just n  -> "row " ++ show n
      Nothing -> "the unnumbered row at position " ++ show i

    dupMsg first dup = concat
      [ rowLabel first, " and ", rowLabel dup
      , " have the same input cells (", guardOf (snd dup), "), so "
      , rowLabel dup, " can never match: a table with hit policy Unique must not"
      , " contain overlapping rules (DMN 1.3 §8.2.10). Delete one of the two"
      , " rows, or change an input cell of ", rowLabel dup, " so that the two"
      , " rules select different inputs. dmnmd compares the values the cells"
      , " denote, not the text: two cells spelled differently for the same value"
      , " (1.1 and 1.10, say) are the same guard." ]

    guardOf r = intercalate "; "
      [ varname ch ++ ": " ++ intercalate ", " (showDomainMember <$> cells)
      | (ch, cells) <- zip ins (row_inputs r) ]

-- | D-2. A column dmnmd could not type, refused instead of guessed.
--
-- __Why here and not in 'inferTypes'.__ 'mkDTable' transposes the rows into
-- columns before calling 'inferTypes', dropping the rule numbers on the way, so
-- a refusal raised there could not name a row — every other cell-layer
-- diagnostic can. Walking 'allrows' gets the row numbers back by construction,
-- puts this in the same place as 'structuralErrors' and 'domainErrors', and
-- gives the XML reader a located 'DMN.XML.XmlToDmnmd.Diagnostic' for free
-- rather than an @error@ it cannot catch.
--
-- __Only untyped columns are considered__, which is how a declared type keeps
-- short-circuiting inference: 'inferTypes' returns a declared header untouched,
-- so @vartype@ is @Just@ and this never looks at it. A column that RESOLVED is
-- likewise @Just@ and skipped. What is left is exactly the three ways to end up
-- untyped, and only two of them are errors — 'VNone', an all-wildcard column, is
-- a legitimate shape and stays silent.
inferenceErrors :: DecisionTable -> [String]
inferenceErrors dt =
  [ err
  | (ch, cells) <- zip ins (colsOf row_inputs) ++ zip outs (colsOf row_outputs)
  , Nothing <- [vartype ch]
  , err <- verdictErrs ch cells
  ]
  where
    ins  = getInputHeaders  (header dt)
    outs = getOutputHeaders (header dt)
    rows = [ r | r@DTrow{} <- allrows dt ]
    colsOf f = transpose [ [ (row_number r, cell) | cell <- f r ] | r <- rows ]

    -- No row segment in the prefix: a conflict is a property of the COLUMN, and
    -- the rows that disagree are named in the body, where there is room for more
    -- than one of them. 'domainErrors' already prints a row-less complaint this
    -- way. An ambiguous cell is a property of one cell and is located at it.
    verdictErrs ch cells = case columnVerdict (snd <$> cells) of
      VNone       -> []
      VType _     -> []
      VConflict ts ->
        [ columnRow (varname ch) Nothing ++ concat
          [ "dmnmd cannot infer a type for this column: "
          , intercalate ", and " [ witness ty | ty <- sortOn showType ts ]
          , ". An undeclared column has to agree with itself, and dmnmd will not"
          , " pick a winner and read the losing cells as literal text — which is"
          , " what it used to do, silently. Declare the column: "
          , show (varname ch ++ " : String"), " reads every cell as the text the"
          , " author wrote; ", show (varname ch ++ " : Number"), " requires every"
          , " cell to be a number, a comparison, an interval or arithmetic." ]
        ]
      VAmbiguous ss ->
        [ columnRow (varname ch) (firstRowOf s) ++ concat
          [ "the cell reads ", show s
          , " — a leading zero has no numeric meaning, so this is either the"
          , " number ", unpadded s, " written oddly or an identifier that happens"
          , " to be digits, and dmnmd will not choose."
          , " Declare the column: ", show (varname ch ++ " : String"), " keeps "
          , show s, " exactly as written; ", show (varname ch ++ " : Number")
          , " reads it as ", unpadded s, "." ]
        | s <- ss ]
      where
        witness ty = case [ (rn, showDomainMember c)
                          | (rn, cs) <- cells, c <- cs, inferEvidence c == EType ty ] of
          ((rn, txt):_) -> maybe "" (\n -> "row " ++ show n ++ " ") rn
                           ++ show txt ++ " reads as " ++ showType ty
          []            -> showType ty  -- unreachable: ty came from these cells
        -- The value the padded literal denotes, spelled the way FEEL would.
        -- "000" strips to nothing, and the message has to say 0.
        unpadded s = case span (`elem` "+-") s of
          (sgn, rest) -> case dropWhile (== '0') rest of
            ""          -> sgn ++ "0"
            r@('.':_)   -> sgn ++ "0" ++ r
            r           -> sgn ++ r
        firstRowOf s = case [ rn | (rn, cs) <- cells
                                 , c <- cs, inferEvidence c == EAmbiguous s ] of
          (rn:_) -> rn
          []     -> Nothing

-- | Cell shapes dmnmd refuses rather than guessing at, mostly about collections.
--
-- __Why this lives here and not in 'mkFEither'.__ Refusing inside the cell
-- constructor is the obvious implementation and it is wrong three times over:
--
--  * 'mkFEither' cannot tell an input cell from an output cell from a
--    __sub-header domain member__ — 'DMN.ParseTable.parseTable' builds @enums@
--    through the same 'mkFsAt'. A range domain @[0..150]@ on a @[Number]@ column
--    works today; refusing tests in the constructor would make it unwritable.
--  * it knows no row number and no column name, so the message could not locate
--    the offending cell.
--  * 'reprocessRows' calls 'mkFAt' with the full column type on live paths, so
--    a 'Left' there refuses ordinary tables. (Before D-7 it crashed them; the
--    located wrappers were @either error id@ underneath. Now it is a diagnostic
--    and the table is dropped instead — different blast radius, same defect.)
--
-- Walking 'allrows' fixes all three: the sub-header row is excluded __by
-- construction__ rather than by a special case that could rot.
--
-- Returned rather than thrown, exactly as 'domainErrors' is, so the markdown
-- path can @error@ and the XML path can locate and refuse one table.
structuralErrors :: DecisionTable -> [String]
structuralErrors dt = concat
  [ nestedCols, listInputErrs, listOutputErrs, listArithErrs, hitPolicyErrs
  , inputArithErrs, openRangeOutErrs, outputNegationErrs, defaultEntryErrs ]
  where
    ins  = getInputHeaders  (header dt)
    outs = getOutputHeaders (header dt)

    -- R1. `isCollection` in DMN is a flag, not a depth (DMN 1.3 Table 26), and
    -- dmnmd has no cell syntax for a list of lists. Column-level, so an
    -- all-wildcard nested column is refused too.
    nestedCols =
      [ concat [ "column ", show (varname ch)
               , ": dmnmd supports a list of scalars, not a list of lists."
               , " DMN's isCollection is a flag, not a depth (DMN 1.3 Table 26)."
               -- Suggest the FLATTENED form. Naming `inner` here would echo the
               -- nested type straight back at the author as the repair.
               , " Declare the column \": [", showType (innermost inner), "]\"." ]
      | ch <- ins ++ outs
      , Just (DMN_List inner@(DMN_List _)) <- [vartype ch]
      ]

    innermost (DMN_List t) = innermost t
    innermost t            = t

    listInputErrs =
      [ msg
      | r@DTrow{} <- allrows dt
      , (ch, cells) <- zip ins (row_inputs r)
      , isListCol ch
      , not (nested ch)
      , msg <- inputCellErrs ch (row_number r) cells
      ]

    listOutputErrs =
      [ locate ch rn (concat
          [ "the output cell reads ", show (showDomainMember cell)
          , ". An output column produces a VALUE, and a collection column's value"
          , " is a comma-separated list of members. A test is not a value." ])
      | r@DTrow{} <- allrows dt
      , let rn = row_number r
      , (ch, cells) <- zip outs (row_outputs r)
      , isListCol ch
      , not (nested ch)
      , cell <- cells
      , not (isPlainOrWild cell)
      ]

    -- R2/R3/R4, per cell of a collection INPUT column.
    inputCellErrs ch rn cells = concat
      [ -- R4 first: a mixed cell explains the others away.
        [ locate ch rn (concat
            [ "this cell mixes \"-\" with values. \"-\" already matches every"
            , " collection, so the other values cannot change the answer."
            , " Write \"-\" alone, or drop it."
            , " A trailing or doubled comma is the usual cause." ])
        | any (== FAnything) cells, length cells > 1 ]
      , [ locate ch rn (junkMsg s)
        | length cells == 1 || not (any (== FAnything) cells)
        , FNullary (VS s) <- cells, any (`elem` ("()[]" :: String)) s ]
      , [ locate ch rn (ambiguousMsg ch cell)
        | not (any (== FAnything) cells)
        , cell <- cells, not (isPlainOrWild cell) ]
      ]

    -- R2. The one that matters. Measured in node: `[5] > 3` is true, `[10] > 3`
    -- is true, `[1,2] > 3` is false — JS stringifies the array and coerces, so
    -- the emitted guard fires on some qualifying lists and not others. Refusing
    -- beats picking a quantifier, because the two readings are different rules
    -- and DMN gives the comparison no meaning at all: §10.3.2.10 reduces
    -- satisfaction to FEEL(e in (t)) and Table 54 defines < and > over scalars.
    ambiguousMsg ch cell = concat
      [ "column ", show (varname ch), " is a collection ("
      , maybe "?" showType (vartype ch), ") and the cell reads "
      , show (showDomainMember cell)
      , ". A test against a collection is ambiguous — \"some element "
      , showDomainMember cell, "\" and \"every element ", showDomainMember cell
      , "\" are different rules — and DMN gives it no meaning, so a conformant"
      , " engine yields null and the rule never fires. dmnmd will not guess."
      , " A collection column's cell may be a plain value, meaning the"
      , " collection CONTAINS it, a comma-separated list of them, meaning \"any"
      , " of these\", or \"-\". Aggregate the collection to a scalar before the"
      , " table, or split the column." ]

    -- R3. This is the shape a FEEL construct arrives in after 'mkFsEither'
    -- splits the cell on commas: `not("Fall", "Winter")` becomes fragments with
    -- unbalanced parens, and `list contains(?, "RED")` and `["a","b"]` likewise.
    -- Without this they are junk members that typecheck and never fire.
    junkMsg s = concat
      [ "the cell reads ", show s
      , " — a collection column's cell must be a plain member value or \"-\"."
      -- Since D-9 negation IS implemented, so the old wording ("dmnmd does not
      -- implement … negation …") named a true refusal with a false reason. In a
      -- COLLECTION column it stays refused for the reason above — a test over a
      -- collection is ambiguous — not for want of an IR.
      , " A collection column takes members, not tests, so negation is refused"
      , " here even though dmnmd implements it elsewhere; FEEL function calls and"
      , " list literals are not implemented at all. The comma split has already"
      , " broken this cell into fragments."
      , " If the value really is that text, quote the whole cell." ]

    -- R5. Without this a collection reaches 'fNEval'/'fromVN', whose errors name
    -- no table, column or row. Keyed on 'varname' exactly as 'evalTable' keys
    -- its symbol table.
    listArithErrs =
      [ concat [ maybe "" (\n -> "row " ++ show n ++ ": ") (row_number r)
               , "the expression refers to column ", show v
               , ", which is a collection (", maybe "?" showType (vartype ch)
               , "). dmnmd has no arithmetic over collections." ]
      | r@DTrow{} <- allrows dt
      , FFunction f <- concat (row_inputs r ++ row_outputs r)
      , v <- fnVars f
      , ch <- ins ++ outs
      , varname ch == v
      , isListCol ch
      ]

    -- R8. An input entry is a unary test (§9.2 rule 12): a value, a comparison,
    -- an interval, or "-". There is NO arithmetic production for one. An output
    -- entry is a rule 3 `simple expression`, which DOES include arithmetic
    -- (§9.5.3, §8.2.9, and the XSD's inputEntry=tUnaryTests /
    -- outputEntry=tLiteralExpression split), so policy/md-arith-output stays
    -- legal and is untouched. Independently: 'fEval' has no FFunction arm, so an
    -- arithmetic input cell can never match anything at all; it can only reach a
    -- backend, where `40 - 50` becomes the JS guard `(40.0 - 50.0)` — no input
    -- variable mentioned, and unconditionally truthy.
    inputArithErrs =
      [ locate ch (row_number r) (concat
          [ "the input cell reads ", show (showDomainMember cell)
          , ". An input entry is a unary test (DMN 1.3 §9.2 rule 12): a value, a"
          , " comparison, an interval, or \"-\". Arithmetic is a rule 3 simple"
          , " expression and is legal only in an OUTPUT cell. A dash-written range"
          , " is arithmetic, not an interval — write [40..50], not 40 - 50."
          -- Name the column the author actually wrote, not a stock example:
          -- 'ch' is in scope here, and a message that says "Season" to someone
          -- whose column is called "Amount" reads as a bug in the tool.
          -- D-15: the closing sentence used to stop at ": String", and following
          -- it is ACCEPTED — it turns the cell into a string test against its own
          -- source text, which can never fire, at exit 0. A refusal that hands
          -- the author a silent wrong answer is worse than one that hands them
          -- nothing, so the sentence now says what the repair actually does.
          , " If this column is not numeric, declare it (\"", varname ch
          , " : String\") — but a String column compares this cell as literal"
          , " text rather than computing it, so a test written that way can never"
          , " match. With no declaration dmnmd infers Number from a column"
          , " whose cells all read as numeric." ])
      | r@DTrow{} <- allrows dt
      , (ch, cells) <- zip ins (row_inputs r)
      , cell@(FFunction _) <- cells
      ]

    -- R9. An OUTPUT cell may hold a range (README "Extensions"), and
    -- 'DMN.Translate.L4.showFeelL4' renders one by dropping the upper bound.
    -- That is already wrong for a CLOSED range and is recorded separately as
    -- symptom/l4-output-range-upper-bound-dropped; for an OPEN bound it would
    -- emit a value that is not even in the interval. Refused here rather than in
    -- 'showFeelL4', which returns a String and cannot express a refusal.
    openRangeOutErrs =
      [ locate ch (row_number r) (concat
          [ "the output cell reads ", show (showDomainMember cell)
          , ". dmnmd can emit a range as an output VALUE only when both endpoints"
          , " are included: an excluded endpoint has no value to name."
          , " Write a closed range [a..b], or move the test to an input column." ])
      | r@DTrow{} <- allrows dt
      , (ch, cells) <- zip outs (row_outputs r)
      , cell@(FInRange lk _ _ rk) <- cells
      , lk == BOpen || rk == BOpen
      ]

    -- R10. Negation is a unary TEST (§9.2 rule 12.b), so it has no meaning in an
    -- output cell: @not([1..5])@ selects a set of values rather than naming one,
    -- and there is nothing for a backend to return.
    --
    -- This refusal is what keeps D-9 from being a regression. Before 'FNot'
    -- existed, an output cell reading @not(…)@ was refused loudly by
    -- 'DMN.ParseCell.parseNumberCell', which has no idea whether it is looking at
    -- an input or an output and refused both. Now it BUILDS, so without this arm
    -- the cell would sail through to a backend and hit a catch-all: an @error@ in
    -- 'DMN.Translate.FEELhelpers.showFeel' for js/ts/py, and — worse — the L4
    -- emitter is the one that would have to invent a value. Turning a located
    -- refusal into a crash is not a fix.
    --
    -- Mirrors 'inputArithErrs', which is the same argument the other way up:
    -- arithmetic is legal only in an output cell, negation only in an input one.
    outputNegationErrs =
      [ locate ch (row_number r) (concat
          [ "the output cell reads ", show (showDomainMember cell)
          , ". Negation is a unary test (DMN 1.3 §9.2 rule 12.b) and selects a SET"
          , " of values, so it cannot name the one value an output column must"
          , " return. It is legal only in an INPUT cell."
          , " Write the value you want returned, or move the negation into an"
          , " input column and leave \"", varname ch, "\" a plain value." ])
      | r@DTrow{} <- allrows dt
      , (ch, cells) <- zip outs (row_outputs r)
      , cell@(FNot _) <- cells
      ]

    -- R6/R7. A collection has no position in an element-level domain, so every
    -- comparison is EQ and the ordering silently degrades to row order; and
    -- there is no aggregate over collections, so Collect would flatten every
    -- matching row's elements into one number with no diagnostic.
    hitPolicyErrs = case hitpolicy dt of
      HP_Priority    -> orderErr "Priority"
      HP_OutputOrder -> orderErr "OutputOrder"
      HP_Collect op | op /= Collect_All ->
        [ concat [ "hit policy Collect ", showCollect op, " aggregates output column "
                 , show (varname ch), ", which is a collection ("
                 , maybe "?" showType (vartype ch)
                 , "). dmnmd has no aggregate over collections — the elements of"
                 , " every matching row would be flattened into one number with no"
                 , " diagnostic. Use Collect All, or a scalar output column." ]
        | ch <- outs, isListCol ch ]
      _ -> []

    orderErr hp =
      [ concat [ "hit policy ", hp, " orders rows by output column "
               , show (varname ch), "'s declared domain, but ", show (varname ch)
               , " is a collection (", maybe "?" showType (vartype ch)
               , ") whose domain is element-level — a collection value has no"
               , " position in it, so every comparison is EQ and the ordering"
               , " silently degrades to row order." ]
      | ch <- outs, isListCol ch, Just (_:_) <- [enums ch] ]

    -- A default output value (§8.2.11) is a VALUE, exactly as an output cell
    -- is, so it refuses the same test-shaped content 'openRangeOutErrs' and
    -- 'outputNegationErrs' refuse in rows — phrased at the default, because
    -- there is no row to name. Only the XML reader can populate the slot today,
    -- and its 'mkCells' happily builds @< 5@ at a Number column, so the shapes
    -- are reachable, not hypothetical. @FAnything@ is not checked: in this slot
    -- it is the spelling of "no default declared for this column".
    defaultEntryErrs =
      [ concat
          [ "column ", show (varname ch), ": default output value: the cell reads "
          , show (showDomainMember cell)
          , ". A default output value is a literal expression — a VALUE"
          , " (DMN 1.3 §8.2.11) — and a comparison, a negation or an open range"
          , " selects values rather than naming one." ]
      | Just defs <- [dtDefaultOutput dt]
      , (ch, cells) <- zip outs defs
      , cell <- cells
      , isTestish cell
      ]

    isTestish FSection{}           = True
    isTestish FNot{}               = True
    isTestish (FInRange lk _ _ rk) = lk == BOpen || rk == BOpen
    isTestish _                    = False

    nested ch = case vartype ch of
      Just (DMN_List (DMN_List _)) -> True
      _                            -> False

    isPlainOrWild FAnything    = True
    isPlainOrWild (FNullary _) = True
    isPlainOrWild _            = False

    locate ch rn body = columnRow (varname ch) rn ++ body

-- | Every variable an arithmetic cell mentions.
fnVars :: FNumFunction -> [String]
fnVars (FNF0 _)         = []
fnVars (FNF1 v)         = [v]
fnVars (FNF3 l _ r)     = fnVars l ++ fnVars r

-- | A column type, spelled as it would be written in a markdown header.
showType :: DMNType -> String
showType DMN_String     = "String"
showType DMN_Number     = "Number"
showType DMN_Boolean    = "Boolean"
showType (DMN_List t)   = "[" ++ showType t ++ "]"

-- | A hit policy as the author wrote it in the table's top-left cell, rather
-- than as @show@ spells the constructor.
showHitPolicy :: HitPolicy -> String
showHitPolicy HP_Unique       = "U (Unique)"
showHitPolicy HP_Any          = "A (Any)"
showHitPolicy HP_Priority     = "P (Priority)"
showHitPolicy HP_First        = "F (First)"
showHitPolicy HP_OutputOrder  = "O (OutputOrder)"
showHitPolicy HP_RuleOrder    = "R (RuleOrder)"
showHitPolicy (HP_Collect op) = "C (Collect " ++ showCollect op ++ ")"
-- No parser produces this: 'mkHitPolicy_' has no letter for it and
-- 'mkHitPolicy_C' only builds HP_Collect. Kept nameable rather than partial.
showHitPolicy HP_Aggregate    = "Aggregate"

showCollect :: CollectOperator -> String
showCollect Collect_Sum = "Sum"
showCollect Collect_Min = "Min"
showCollect Collect_Max = "Max"
showCollect Collect_Cnt = "Count"
showCollect Collect_All = "All"

-- | Things worth saying out loud that are not grounds for refusal.
--
-- Printed on stderr; the exit status is unaffected, because it answers only
-- "did something we were asked to read fail to read?".
tableWarnings :: DecisionTable -> [String]
tableWarnings dt = overlapWarn
  where
    ins = getInputHeaders (header dt)
    listIns = [ varname ch | ch <- ins, isListCol ch ]

    -- W2. Membership tests over DISJOINT values still overlap: rules `admin`
    -- and `clerk` both fire on ["admin","clerk"]. So a table a DMN validator
    -- would call fine can report "multiple rows returned" at run time.
    overlapWarn
      | null listIns = []
      | hitpolicy dt `elem` [HP_Unique, HP_Any] =
          [ concat [ "hit policy ", showHitPolicy (hitpolicy dt), " with collection input column(s) "
                   , intercalate ", " (show <$> listIns)
                   , ": membership tests over disjoint values still overlap — a"
                   , " collection holding two of them matches both rules — so this"
                   , " table can report multiple matches on input a DMN validator"
                   , " would accept." ]
          ]
      | otherwise = []

-- | Every way a table's cells violate the domains its sub-header row declares.
--
-- Empty means the table is consistent with what it says about itself. Returned
-- rather than thrown so both readers can use it: 'mkDTable' turns it into an
-- @error@, which is how the markdown path already reports a bad cell, and the
-- XML reader can turn the same list into located 'DMN.Diagnostic.Diagnostic's
-- and refuse just the one table. Do not grow a second copy of this rule
-- anywhere — a validator that drifts from the constructor is worse than none.
--
-- __Membership is decided by 'fEval', deliberately.__ A domain member is a cell
-- like any other, so @LOW@ is @FNullary (VS "LOW")@ and @[0..150]@ is
-- @FInRange 0 150@, and asking "is this value in the domain" is exactly asking
-- "would a rule written with that domain member match this value". Reusing the
-- evaluator means a declared numeric range constrains numeric cells for free,
-- and — more importantly — the check can never disagree with what matching
-- actually does at run time.
--
-- Only __plain values__ are checked. A cell holding a test (@< 18@, @[18..65]@,
-- @-@, an arithmetic expression) is not a member of the domain; it selects a
-- subset of it, so checking it against a list of values would be a category
-- error. That is why this matches 'FNullary' and lets every other constructor
-- through.
domainErrors :: DecisionTable -> [String]
domainErrors dt = malformedDomains ++ violations ++ defaultViolations
  where
    -- Columns whose declared domain is itself broken. Checked FIRST and
    -- separately, because such a domain silently disables the check below
    -- rather than failing it — see 'emptyMemberMsg'.
    malformedDomains =
      [ emptyMemberMsg ch
      | ch <- getInputHeaders (header dt) ++ getOutputHeaders (header dt)
      , Just domain <- [enums ch]
      , length domain > 1
      , any (== FAnything) domain
      ]

    violations =
      [ msg ch rn cell
      | r@DTrow{} <- allrows dt
      , let rn = row_number r
      , (ch, cells) <- zip (getInputHeaders  (header dt)) (row_inputs  r)
                    ++ zip (getOutputHeaders (header dt)) (row_outputs r)
      , domain <- maybe [] pure (enums ch)
      , not (null domain)
      , not (any (== FAnything) domain)   -- already reported as malformed
      , cell@(FNullary _) <- cells
      , not (fEvals cell domain)
      ]

    -- The default output value is a plain VALUE like any output cell, so it is
    -- checked as a member exactly as 'violations' checks one — same 'fEvals',
    -- same plain-value gate. Its @FAnything@ spelling means "no default
    -- declared for this column" and is let through by the 'FNullary' match.
    defaultViolations =
      [ concat
          [ "column ", show (varname ch), ": default output value: "
          , "value outside the column's declared domain {"
          , intercalate ", " (showDomainMember <$> domain)
          , "} — the cell reads ", showDomainMember cell ]
      | Just defs <- [dtDefaultOutput dt]
      , (ch, cells) <- zip (getOutputHeaders (header dt)) defs
      , domain <- maybe [] pure (enums ch)
      , not (null domain)
      , not (any (== FAnything) domain)
      , cell@(FNullary _) <- cells
      , not (fEvals cell domain)
      ]

    -- | A domain with a wildcard member among real ones constrains NOTHING, and
    -- does so silently: 'fEval' 'FAnything' matches anything, so 'fEvals'
    -- returns True for every cell and the whole column stops being checked.
    --
    -- The usual cause is a one-character typo. @Dining, Grocery,@ and
    -- @Dining,, Grocery@ both split (in 'mkFsEither') to a fragment that is the
    -- empty string, and @mkFEither _ "" = Right FAnything@ turns that into a
    -- wildcard. Measured before the fix, three inputs differing by one comma:
    --
    -- > Dining, Grocery    TOTALLY BOGUS   exit 1, refused
    -- > Dining, Grocery,   TOTALLY BOGUS   exit 0, accepted
    -- > Dining,, Grocery   TOTALLY BOGUS   exit 0, accepted
    --
    -- So a stray comma bought silent acceptance — which is the exact failure
    -- class E4 exists to remove, reintroduced by E4 itself. Refusing is right
    -- rather than dropping the wildcard and checking the rest, because we cannot
    -- tell a typo from a deliberate "anything goes" and the two want opposite
    -- treatment; making the author say which costs them one character.
    --
    -- A domain of ONLY @-@ is not this case and never reaches here:
    -- 'DMN.ParseTable.parseTable' already turns a wholly-@FAnything@ sub-header
    -- cell into @enums = Nothing@, i.e. no declared domain. Hence @length > 1@.
    emptyMemberMsg ch = concat
      [ "column ", show (varname ch)
      , ": the declared domain has a wildcard member, so it constrains nothing"
      , " — check for a stray or doubled comma in the sub-header row"
      ]

    -- No table name and no "error:" prefix: each reader frames this its own
    -- way. The markdown path prepends `error: table "X": ` on its way to
    -- @error@; the XML path hands it to 'DMN.XML.XmlToDmnmd.errorAt' through
    -- that module's own `inTable`, which also knows the rule id. One rule, two
    -- framings — rather than one rule and two implementations. The column/row
    -- half is 'columnRow', shared with 'locate' and 'showSite' so the three
    -- spellings of a location cannot drift.
    msg ch rn cell = concat
      [ columnRow (varname ch) rn
      , "value outside the column's declared domain {"
      , intercalate ", " (showDomainMember <$> fromJust (enums ch))
      , "} — the cell reads ", showDomainMember cell
      ]

-- | A domain member or cell, as it would have been written in the table.
-- Only used to build the diagnostic in 'domainErrors'.
showDomainMember :: FEELexp -> String
showDomainMember (FNullary (VS s)) = s
showDomainMember (FNullary (VN n)) = showNumPlain n
showDomainMember (FNullary (VB b)) = toLower <$> show b
showDomainMember (FInRange lk lo hi rk) =
  openBracket lk ++ showNumPlain lo ++ ".." ++ showNumPlain hi ++ closeBracket rk
  where openBracket  BClosed = "["
        openBracket  BOpen   = "("
        closeBracket BClosed = "]"
        closeBracket BOpen   = ")"
showDomainMember  FAnything        = "-"
-- Round-trips to the source spelling, like every other arm: the author wrote
-- @not([1..5])@ and a refusal must quote that back, not @FNot (FInRange …)@.
showDomainMember (FNot inner)      = "not(" ++ showDomainMember inner ++ ")"
-- A refusal quotes the cell back at the author, so these have to read like the
-- table did — @"> 3"@, not @"FSection Fgt (VN 3.0)"@ and not @"> 3.0"@ either.
-- The second half of that promise was broken from the day it was written: this
-- function rendered numbers with 'show', so a cell reading @40 - 50@ was quoted
-- back as @"40.0 - 50.0"@ in the same sentence that correctly quoted the source
-- as @40 - 50@. 'showNumPlain' is what makes the comment true, and it is the
-- reason five @policy\/@ recordings changed when 'VN' became 'Scientific'.
-- Before 'structuralErrors'
-- nothing but 'FNullary' and 'FInRange' could reach here, so the @show e@
-- fallthrough was never seen; now it can be, and a diagnostic that names a
-- Haskell constructor is a diagnostic the author cannot act on.
-- Unreachable: a 'VL' is a runtime argument, never a cell, so it can never be a
-- domain member. Spelled out anyway, because the alternative is Haskell
-- constructor syntax leaking into a diagnostic.
showDomainMember (FNullary (VL vs)) =
  "[" ++ intercalate ", " (showDomainMember . FNullary <$> vs) ++ "]"
showDomainMember (FSection op v)   = showBinOp op ++ " " ++ showDomainMember (FNullary v)
showDomainMember (FFunction f)     = showFNumFunction f

showBinOp :: FBinOp -> String
showBinOp Flt  = "<"
showBinOp Flte = "<="
showBinOp Fgt  = ">"
showBinOp Fgte = ">="
showBinOp Feq  = "="

-- | Render an arithmetic cell back to the author, for a diagnostic.
--
-- Parentheses go round a nested operator application and __not__ round the
-- whole expression — the same rule, and for the same reason, as
-- 'DMN.Translate.XML.showArith', whose haddock named this function as the flat
-- copy that had not adopted it. Flat is a __misquote__: 'DMN.ParseFEEL.parseFNF3'
-- accepts one top-level operator whose operands may be parenthesised, so
-- @(Age + 1) * (Age + 2)@ came back to the author as @Age + 1 * Age + 2@ — text
-- they did not write, that dmnmd itself refuses, and that is a different number.
-- Not round the top level, because a parenthesised whole cell has no production
-- either, and quoting one back would suggest a repair that is also refused.
showFNumFunction :: FNumFunction -> String
showFNumFunction (FNF0 v)       = showDomainMember (FNullary v)
showFNumFunction (FNF1 v)       = v
showFNumFunction (FNF3 l op r)  =
  operand l ++ showFNOp2' op ++ operand r
  where
    operand f@FNF3{} = "(" ++ showFNumFunction f ++ ")"
    operand f        = showFNumFunction f
    showFNOp2' FNMul   = " * "
    showFNOp2' FNDiv   = " / "
    showFNOp2' FNPlus  = " + "
    showFNOp2' FNMinus = " - "
    showFNOp2' FNExp   = " ** "

-- | Rebuild a column's declared domain at the type inference settled on.
--
-- 'DMN.ParseTable.parseTable' builds @enums@ from the sub-header row using the
-- type __as literally written in the header__, which for an undeclared column is
-- 'Nothing' — so the domain becomes a list of strings. Inference then runs here,
-- in 'mkDTable', and 'reprocessRows' re-types only the data cells. The domain
-- was never revisited, so a table like
--
-- > | O | Age | > Score (out) |
-- > |---|-----|---------------|
-- > |   |     | 30, 10, 20    |
--
-- ended up comparing numeric cells against string domain members. Nothing ever
-- matched, 'elemIndex' returned 'Nothing' for every value, and hit policy @O@
-- silently degraded to row order — recorded as
-- @test\/corpus\/cases\/symptom\/struct-outputorder-enum-untyped@.
--
-- This reuses 'reprocessRows' rather than repeating its logic, so the domain and
-- the cells are re-typed by the same code under the same guard: a domain is only
-- rebuilt when every member is still an unconverted @FNullary (VS _)@. A domain
-- on a column whose type stays 'Nothing' is left alone, as are @FAnything@ and
-- anything already converted.
--
-- Takes the table name only to locate a refusal ('reprocessRows' calls 'mkFAt');
-- the row is 'Nothing' because a sub-header row has no rule number.
retypeEnums :: String -> ColHeader -> ([Diagnostic], ColHeader)
retypeEnums tbl ch = case enums ch of
  Nothing -> ([], ch)
  Just es -> let (ds, rows) = reprocessRows tbl Nothing [ch] [es]
             in (ds, ch { enums = listToMaybe rows })

-- | The table name and row number are here for one reason: to build the
-- 'CellSite' that locates a refusal from 'mkFAt'. The column half of the site
-- comes from the 'ColHeader' this already has in hand.
--
-- __A refused cell is left exactly as pass 1 read it__, and the 'Diagnostic'
-- rides out alongside it. Nothing is fabricated to fill the hole, because a
-- fabricated value that reached a backend would be the silent wrong answer this
-- whole layer exists to prevent — and it cannot reach one: 'mkDTable' emits no
-- table at all when any of these is an 'DMN.Diagnostic.Error'.
reprocessRows :: String -> Maybe Int -> [ColHeader] -> [[FEELexp]] -> ([Diagnostic], [[FEELexp]])
reprocessRows tbl rn chs rows = unzipCols (reprocessRows_ tbl rn chs rows)
  where unzipCols cols = (concatMap fst cols, snd <$> cols)

reprocessRows_ :: String -> Maybe Int -> [ColHeader] -> [[FEELexp]] -> [([Diagnostic], [FEELexp])]
reprocessRows_ tbl rn =
  -- bang through all columns where the header vartype is Just something, and if the body is FNullary VS, then re-'mkFAt' it using the new type info
  zipWith (\ch cells ->
             -- Debug.Trace.trace ("** reprocessRows: have the option to reprocess cells to " ++ show (vartype ch) ++ ": " ++ show cells) $
               -- DMN_String used to be excluded here alongside Nothing, because
               -- re-running a string cell at DMN_String was a no-op: both the
               -- Nothing arm and the DMN_String arm of mkFEither produced
               -- FNullary (VS (trim arg)). Since 'unquoteSFeel' it is no longer
               -- a no-op, and this is the pass where an inferred string column
               -- gets its quotes stripped — inferType classifies "Fall" as
               -- DMN_String, but nothing re-ran the cell at that type.
               -- Idempotent for an explicitly-declared : String column, whose
               -- cells were already unquoted in pass 1.
               --
               -- Rewritten ELEMENT-WISE. It used to guard on EVERY cell of the
               -- multi-value list still being an unconverted VS, and then rebuild
               -- with a list COMPREHENSION over `FNullary (VS x) <- cells`, which
               -- is a filter: without the guard the comprehension silently
               -- DELETED the FAnything out of a `4, -` cell rather than leaving
               -- it alone, so the guard was accidentally protecting against a
               -- worse bug one line below it and the two had to change together.
               -- With the guard, `4, -` in a Number column left the string "4"
               -- inside it (symptom/infer-multivalue-dash-not-reprocessed), as
               -- did the far likelier trailing-comma typo `4,`. Nothing to do
               -- with D-2: inference typed that column correctly.
               if vartype ch /= Nothing
               then partitionCells
                      (map (\case c@(FNullary (VS x)) ->
                                    either (\d -> (Just d, c)) ((,) Nothing)
                                           (mkFAt (CellSite tbl (varname ch) rn) (vartype ch) x)
                                  other -> (Nothing, other)) cells)
               else ([], cells))
  where partitionCells cs = (catMaybes (fst <$> cs), snd <$> cs)
                         
  
getInputHeaders :: [ColHeader] -> [ColHeader]
getInputHeaders = getWantedHeaders DTCH_In

getOutputHeaders :: [ColHeader] -> [ColHeader]
getOutputHeaders = getWantedHeaders DTCH_Out

getCommentHeaders :: [ColHeader] -> [ColHeader]
getCommentHeaders = getWantedHeaders DTCH_Comment

getWantedHeaders :: DTCH_Label -> [ColHeader] -> [ColHeader]
getWantedHeaders wantedLabel = filter ((wantedLabel==).label)

-- | Resolve a column's type from its cells — or leave it untyped, which
-- 'inferenceErrors' will then turn into a located refusal.
--
-- __A declared type always wins, and this function must never overwrite one.__
-- Before D-2 that held by accident: the disagreement branch happened to return
-- the header unchanged. It is now the first thing tested, which also removes a
-- trap the old shape had — a @roles : [String]@ column's cells infer
-- @DMN_String@, never @DMN_List DMN_String@, so EVERY declared collection column
-- (including @README.md@'s own) took the disagreement branch on every run.
inferTypes :: ColHeader   -- in or out header column
           -> [[FEELexp]] -- body column of expressions corresponding to that column
           -> ColHeader   -- revised header column with vartype set
inferTypes origch origrows
  | Just _ <- vartype origch = origch
  | VType t <- columnVerdict origrows = origch { vartype = Just t }
  | otherwise = origch

-- | What a whole column's cells say about its type, aggregated.
--
-- 'VNone' and the two failures all leave @vartype@ at 'Nothing', so this type
-- exists to let 'inferenceErrors' tell them apart — and telling them apart is
-- the whole of D-2. A column of nothing but @-@ is 'VNone' and is silent,
-- because a wildcard column is a legitimate and common shape; a column whose
-- cells disagree is 'VConflict' and is refused.
data ColumnVerdict
  = VNone                  -- ^ no cell said anything: an all-wildcard column.
  | VType DMNType          -- ^ resolved.
  | VConflict [DMNType]    -- ^ cells disagree; dmnmd will not pick a winner.
  | VAmbiguous [String]    -- ^ a cell reads as a number it does not spell.
  deriving (Eq, Show)

columnVerdict :: [[FEELexp]] -> ColumnVerdict
columnVerdict rows
  | not (null ambig) = VAmbiguous ambig
  | otherwise = case hard of
      [t] -> VType t
      []  -> if soft then VType DMN_Number else VNone
      ts  -> VConflict ts
  where
    evs   = inferEvidence <$> concat rows
    ambig = nub [ s | EAmbiguous s <- evs ]
    hard  = nub [ t | EType t      <- evs ]
    soft  = not (null [ () | EWeakNumber <- evs ])

-- | What ONE cell says about its column's type.
--
-- 'EWeakNumber' is the tier that makes anchoring possible at all.
-- 'DMN.ParseCell.parseNumberCell' is the right oracle for "is this a number",
-- but it is not a usable one on its own: its arithmetic arm accepts bare FEEL
-- names, because §9.2 rule 27 puts @. \/ - ’ + *@ inside legal names — so it
-- accepts @Non-Participating@ and @n\/a@, neither of which contains a digit.
-- Counting arithmetic as hard evidence therefore types
-- @policy\/md-quoted-literal-all-or-nothing@'s string column as Number;
-- discarding it entirely turns @policy\/num-dash-range-refused@ from a refusal
-- into silence, because @40 - 50@ is the only informative cell in that column.
-- So arithmetic is evidence of LAST RESORT: it decides a column only when
-- nothing harder spoke.
data TypeEvidence
  = ENoEvidence          -- ^ a wildcard, a blank, or a collection value.
  | EType DMNType        -- ^ unambiguous.
  | EWeakNumber          -- ^ arithmetic. See above.
  | EAmbiguous String    -- ^ carries the offending text, for the diagnostic.
  deriving (Eq, Show)

inferEvidence :: FEELexp -> TypeEvidence
-- A 'VL' never reaches inference: inference runs over CELLS, and no cell can
-- hold one. A collection column is therefore always explicitly declared —
-- there is no `[Number]` to infer.
inferEvidence (FNullary  (VL _)) = ENoEvidence
inferEvidence (FSection _ (VL _)) = ENoEvidence
inferEvidence (FFunction _) = EWeakNumber
inferEvidence (FSection _ (VN _)) = EType DMN_Number
inferEvidence (FSection _ (VB _)) = EType DMN_Boolean
inferEvidence (FSection _ (VS _)) = EType DMN_String
inferEvidence (FInRange _ _ _ _)  = EType DMN_Number
inferEvidence  FAnything          = ENoEvidence
-- A negation is exactly as strong evidence as the test it negates: @not(…)@
-- constrains the same column. Note this arm is currently unreachable from the
-- undeclared-column path, which sees only the @FNullary (VS raw)@ arm below —
-- there, @namedRefusal@ still answers for @not(…)@ and yields the same verdict
-- by a different route. Written correctly anyway rather than left to a
-- catch-all, because a declared column does reach here through 'reprocessRows'.
inferEvidence (FNot inner)        = inferEvidence inner
inferEvidence (FNullary (VN _)) = EType DMN_Number
inferEvidence (FNullary (VB _)) = EType DMN_Boolean
-- The only arm that ever fires for an undeclared column: pass 1 calls
-- @mkFEither Nothing@, whose one arm wraps the raw text. See D-2.
inferEvidence (FNullary (VS arg))
  | arg `elem` ["-","_",""] = ENoEvidence
  -- Before the number test, and live only on the XML path: the markdown reader
  -- strips quotes in 'mkFsEither' before a cell ever reaches here, but
  -- 'DMN.XML.XmlToDmnmd''s inference pre-pass deliberately keeps them, because
  -- the quotes are the only thing distinguishing FEEL's "2020" from 2020.
  | length arg >= 2 && head arg == '\"' && last arg == '\"' = EType DMN_String
  | (toLower <$> arg) `elem` boolWords = EType DMN_Boolean
  -- A construct only a Number column could hold, which ParseCell refuses BY
  -- NAME. It is a Left, but "not a number" is the wrong reading of it — see
  -- 'DMN.ParseCell.namedRefusal', which exists because leaving this out made
  -- symptom/num-negation-not-implemented silently pass at exit 0.
  | namedRefusal arg = EType DMN_Number
  | otherwise = case parseNumberCell arg of
      Left _                 -> EType DMN_String
      -- Arithmetic is weak evidence, and it is evidence of a NUMBER only if a
      -- digit appears in it. §9.2 rule 27 puts @. / - ’ + *@ inside legal FEEL
      -- names, so @parseNumberCell@ reads @Non-Participating@ and @n/a@ as
      -- subtraction and division over bare names — arithmetic containing no
      -- number at all. Without this test a column of hyphenated words types
      -- Number, and the two ways that goes wrong are both bad and only one is
      -- loud: an INPUT column refuses a table that was correct before, and an
      -- OUTPUT column emits @return {"Status":(Non - Participating)}@ at exit 0
      -- — TypeScript naming two variables that do not exist, where the string
      -- literal used to be. That is the silent-wrong-answer class D-2 exists to
      -- remove, reintroduced by D-2, and it is pinned from both sides by
      -- policy/infer-hyphenated-words-stay-string and
      -- policy/infer-hyphenated-output-stays-string.
      --
      -- The digit test is what makes the last-resort tier safe rather than
      -- merely narrow. @40 - 50@ still speaks (it is the only informative cell
      -- in policy/num-dash-range-refused, which is why the tier cannot simply be
      -- discarded); @Full-Time@ no longer does.
      Right (FFunction _)
        | any isDigit arg    -> EWeakNumber
        | otherwise          -> EType DMN_String
      Right (FNullary (VN _))
        | redundantLeadingZero arg -> EAmbiguous arg
      Right _                -> EType DMN_Number
  where
    -- Unchanged from the pre-D-2 list. It is one word longer on each side than
    -- 'mkVB' will accept, which is its own defect and not this one's business.
    boolWords = ["true","yes","positive","y","false","no","negative","n"]

-- | Does this text spell a number with a leading zero that means nothing?
--
-- @007@, @042@ and @000@ are legal FEEL numbers (rule 31 admits them) denoting
-- 7, 42 and 0 — and are also exactly how product codes, postcodes and flight
-- numbers are written. dmnmd will not choose; it refuses and asks. @0.5@ is not
-- caught: its single leading zero is ordinary decimal notation, not padding.
--
-- __Why not the general rule "the source text is not the canonical spelling of
-- the value".__ That would also catch @1.10@ — but it catches @10.50@ and @2.0@
-- with it, and refusing a money column for writing cents is a worse outcome
-- than the defect it fixes. The redundant TRAILING zero is ordinary decimal
-- notation; the redundant LEADING zero is not notation at all.
--
-- @1.10@ is nonetheless caught now, and NOT here: D-13 landed
-- 'uniquenessErrors', which refuses two rows of a @U@ table with identical
-- guards. That is the larger class and it needs no reference to types, so this
-- function did not have to widen. See
-- @policy\/hp-unique-duplicate-rows-refused@, which was the symptom this
-- paragraph used to say stays open.
redundantLeadingZero :: String -> Bool
redundantLeadingZero s = case span isDigit (dropWhile (`elem` "+-") (trim s)) of
  (d:_:_, _) -> d == '0'
  _          -> False

-- | Kept as the @Maybe@-shaped view of 'inferEvidence', because the test suite
-- and the haddock both talk in these terms. 'EAmbiguous' maps to 'Nothing':
-- "this cell does not tell me the type" is exactly what it means, and the
-- column-level machinery that turns it into a message is 'columnVerdict'.
inferType :: FEELexp -> Maybe DMNType
inferType fx = case inferEvidence fx of
  EType t      -> Just t
  EWeakNumber  -> Just DMN_Number
  EAmbiguous _ -> Nothing
  ENoEvidence  -> Nothing
