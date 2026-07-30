{-# LANGUAGE LambdaCase, RecordWildCards, NoOverloadedStrings #-}

module DMN.DecisionTable where

{-| Given an ASCII decision table, parse it and transpile it to operational languages like JS and Python. -}

import Control.Arrow ( (<<<), (>>>) )
import Prelude hiding (takeWhile)
import DMN.ParseFEEL ( parseFNumFunction )
import Data.List (intercalate, dropWhileEnd, transpose, nub, sortOn, sortBy, elemIndex, intersect, isPrefixOf, isSuffixOf, find)
import Data.List.Split ( splitOn )
import Data.Maybe ( catMaybes, fromJust, listToMaybe )
import Text.Regex.PCRE ( (=~) )
import Data.Char (toLower)
import Text.Read (readMaybe)
import Debug.Trace ( trace )
import qualified Data.Text as T
import qualified Data.Map as Map
import DMN.ParsingUtils ( parseOnly )
import DMN.Types

-- main = do
--     putStrLn $ show example1_dish
--     putStrLn $ show $ evalTable example1_dish [VS "Fall"]

evalTable :: DecisionTable -> [FEELexp] -> Either String [[[FEELexp]]]
evalTable table given_input =
  let symtab = Map.fromList $ zip (varname <$> filter ((DTCH_In==).label) (header table)) given_input
      matched = filter ((given_input `matches`) . row_inputs) (datarows table)
      -- evaluate any FFunctions
      outputs = (\row -> row { row_outputs = evalFunctions symtab <$> row_outputs row }) <$> matched
  in case hitpolicy table of
    HP_Unique -> case length outputs of
                   0 -> Left "no rows returned -- a unique table should have one result!"
                   1 -> Right (row_outputs <$> outputs)
                   _ -> Left $ "multiple rows returned -- this was supposed to be a unique table!\n" ++ show outputs
    HP_Any    -> case length outputs of
                   0 -> Left "no rows returned"
                   _ -> if not (null (nub (row_outputs <$> outputs)))
                        then Left ("multiple distinct rows returned -- an Any lookup may return multiple matches but they should all be the same!\n" ++ show outputs)
                        else Right (row_outputs <$> outputs)
    HP_Priority    -> Right [row_outputs $ head0 table (outputOrder (header table) outputs)]
    HP_First       -> Right [row_outputs $ head0 table outputs]
    HP_OutputOrder -> Right (row_outputs <$> outputOrder (header table) outputs) -- order according to enums in subheaders.
    HP_RuleOrder   -> Right (row_outputs <$> sortOn row_number outputs)
    HP_Collect Collect_All -> trace ("outputs has length " ++ show (length outputs)) $ Right (row_outputs <$> outputs)
    HP_Collect Collect_Cnt -> trace ("outputs has length " ++ show (length outputs)) $ Right [[[FNullary (VN (fromIntegral (length outputs) :: Float))]]]
    HP_Collect Collect_Min -> Right [[[FNullary (VN (minimum $ [ x | (FNullary (VN x)) <- concat $ concat (row_outputs <$> outputs) ]))]]]
    HP_Collect Collect_Max -> Right [[[FNullary (VN (maximum $ [ x | (FNullary (VN x)) <- concat $ concat (row_outputs <$> outputs) ]))]]]
    HP_Collect Collect_Sum -> Right [[[FNullary (VN (    sum $ [ x | (FNullary (VN x)) <- concat $ concat (row_outputs <$> outputs) ]))]]]
    _ -> Left ("don't know how to evaluate hit policy " ++ show (hitpolicy table))
  where
    evalFunctions :: SymbolTable -> [FEELexp] -> [FEELexp]
    evalFunctions symtab cells = do
      fexp <- cells
      case fexp of
        FFunction f -> return $ FNullary (fNEval symtab f)
        x           -> return x

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
fNEval :: SymbolTable -> FNumFunction -> DMNVal
fNEval symtab (FNF0 dmnval) = dmnval
fNEval symtab (FNF1 varname) = maybe (error $ "function unable to resolve variable " ++ varname) fe2dval $ Map.lookup varname symtab
fNEval symtab (FNF3 fnf1 fnop2 fnf3) = let lhs = fromVN (fNEval symtab fnf1)
                                           rhs = fromVN (fNEval symtab fnf3)
                                           result = case fnop2 of
                                             FNMul   -> lhs * rhs
                                             FNDiv   -> lhs / rhs
                                             FNPlus  -> lhs + rhs
                                             FNMinus -> lhs - rhs
                                             FNExp   -> lhs ** rhs
                                       in VN result

mkFs :: Maybe DMNType -> String -> [FEELexp]
mkFs dmntype args = either error id (mkFsEither dmntype args)

-- | 'mkFs' as a total function.
--
-- The markdown reader is happy to die on a malformed cell: it has already told
-- the user which file and table it was reading. The XML reader is not — it has
-- to name the table, column and rule, and refuse just that table. So the real
-- work lives in 'mkFEither' and 'mkF' is the @error@-ing wrapper, which keeps
-- the markdown path byte-for-byte as it was while giving the XML path something
-- it can report on. Do not reintroduce a second copy of these guards elsewhere:
-- a validator that drifts from the constructor is worse than no validator.
mkFsEither :: Maybe DMNType -> String -> Either String [FEELexp]
mkFsEither dmntype args = traverse (mkFEither dmntype) (unquoteCell (trim <$> splitOn "," args))


-- TODO: add a state monad to allow type inference to span all input rows;
-- if any row contains a string, that entire column becomes a string not a num;
-- if all the columns contain nums or bools, then they're that;
-- but we can only make that decision after viewing the entire table.
-- maybe we use a multi-pass strategy ... where we allow the cells to remain untyped ... and then we review the entire table
-- after it's been fully parsed once.
mkF :: Maybe DMNType -> String -> FEELexp
mkF dmntype arg = either error id (mkFEither dmntype arg)

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
-- 'reprocessRows' calls @mkF (vartype ch)@ with the FULL column type on two
-- live paths — a list-typed OUTPUT column's cells, and 'retypeEnums' rebuilding
-- a declared domain — and @mkFs = either error id@. Refusals belong in
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
    mkVB arg
      | (toLower <$> arg) `elem` ["true","yes","t","y","positive"] = Right (VB True)
      | (toLower <$> arg) `elem` ["false","no","t","y","negative"] = Right (VB False)
      | otherwise = Left $  "unable to parse an alleged boolean: " ++ arg
mkFEither (Just DMN_Number)  arg1
  | not (null ("+-*/" `intersect` arg2)) = either (\msg -> Left $ "error: parsing suspected function expression " ++ arg2 ++ ": " ++ msg) (Right . FFunction) (parseOnly parseFNumFunction (T.pack arg2))
  | "<=" `isPrefixOf` arg2 = FSection Flte <$> (mkVN $ trim $ drop 2 arg2)
  | "<"  `isPrefixOf` arg2 = FSection Flt  <$> (mkVN $ trim $ drop 1 arg2)
  | ">=" `isPrefixOf` arg2 = FSection Fgte <$> (mkVN $ trim $ drop 2 arg2)
  | ">"  `isPrefixOf` arg2 = FSection Fgt  <$> (mkVN $ trim $ drop 1 arg2)
  | "<=" `isSuffixOf` arg2 = FSection Fgt  <$> (mkVN $ trim $ Prelude.take (length arg2 - 2) arg2)
  | "<"  `isSuffixOf` arg2 = FSection Fgte <$> (mkVN $ trim $ Prelude.take (length arg2 - 1) arg2)
  | ">=" `isSuffixOf` arg2 = FSection Flt  <$> (mkVN $ trim $ Prelude.take (length arg2 - 2) arg2)
  | arg2 =~ "\\[\\s*(\\d+)\\s*\\.\\.\\s*(\\d+)\\s*\\]" :: Bool =
    let (_,_,_,bounds) = arg2 =~ "\\[\\s*(\\d+)\\s*\\.\\.\\s*(\\d+)\\s*\\]" :: (String,String,String,[String])
    in Right (FInRange BClosed ((read $ head bounds) :: Float) ((read $ bounds!!1) :: Float) BClosed)
  | "="  `isPrefixOf` arg2 = FSection Feq  <$> (mkVN $ trim $ dropWhile    (=='=') arg2)
  | "="  `isSuffixOf` arg2 = FSection Feq  <$> (mkVN $ trim $ dropWhileEnd (=='=') arg2)
  | otherwise              = FNullary      <$> (mkVN $ trim                        arg2)
  where arg2 = trim arg1 -- probably extraneous
        mkVN x = maybe (Left $ "expected a number, but this column is typed Number and the cell reads " ++ show arg2)
                       (Right . VN)
                       (readMaybe x :: Maybe Float)

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
    go (Just DMN_Number) s =
      maybe (Left $ "expected a number, got " ++ show s) (Right . VN)
            (readMaybe (trim s) :: Maybe Float)
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

fromVN :: DMNVal -> Float
fromVN (VN n) = n
fromVN (VB True) = 1.0
fromVN (VB False) = 0.0
fromVN _ = error "type error: tried to read a float out of a string"

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
--  is in the interval (e1..e2), also notated ]e1..e2[, if and only if o > e1 and o < e1
--  is in the interval (e1..e2], also notated ]e1..e2], if and only if o > e1 and o ≤ e2
--  is in the interval [e1..e2] if and only if o ≥ e1 and o ≤ e2
--  is in the interval [e1..e2), also notated [e1..e2[, if and only if o ≥ e1 and o < e2
-- An expression to be tested satisfies an instance of simple unary tests (grammar rule 12) if and only if, either the
-- expression is a list and the expression satisfies at least one simple unitary test in the list, or the simple unitary tests is “-”.
-- 


-- perform type inference to resolve colheader values based on a review of the rows
mkDTable :: String -> HitPolicy -> [ColHeader] -> [DTrow] -> DecisionTable
mkDTable origname orighp origchs origdtrows =
--  Debug.Trace.trace ("mkDTable: starting; origchs = " ++ show origchs) $
  let newchs   = zipWith inferTypes (getInputHeaders origchs ++ getOutputHeaders origchs)
                                     (transpose $ [ row_inputs r ++  row_outputs r | r@DTrow{} <- origdtrows])
      typedchs = retypeEnums <$> (if not (null newchs) then newchs ++ getCommentHeaders origchs else origchs)
      built = DTable origname orighp typedchs
              ((\case
                   (DTrow rn ri ro rc) -> (DTrow rn
                                  (reprocessRows (getInputHeaders typedchs)  ri)
                                  (reprocessRows (getOutputHeaders typedchs) ro)
                                  rc)) <$> origdtrows)
  in -- Debug.Trace.trace ("mkDTable: finishing...\n" ++
        --                 "origchs = " ++ show(origchs) ++ "\n" ++
           --             "newchs = " ++ show(newchs) ++ "\n" )
    -- A cell outside the domain its own sub-header row declares is a typo, not
    -- a new domain member, and a rule built from it can never match. Emitting it
    -- would be a silently-widened table that exits 0. See BUILD-SPEC-dmnmd-e4.md
    -- §8. Reported by @error@ because that is how this path already reports a
    -- bad cell ('mkFs'); the XML reader calls 'domainErrors' directly so it can
    -- locate the failure and refuse only the offending table.
    case tableErrors built of
      []   -> built
      errs -> error (intercalate "\n" ((("error: table " ++ show origname ++ ": ") ++) <$> errs))

-- | Every reason to refuse a table, in one place, for both readers.
--
-- 'structuralErrors' is about the table's SHAPE — a cell whose meaning dmnmd
-- will not guess at. 'domainErrors' is about a cell disagreeing with the domain
-- the table itself declares. Structural first, because a cell that has no
-- meaning cannot meaningfully be checked against a domain.
tableErrors :: DecisionTable -> [String]
tableErrors dt = structuralErrors dt ++ domainErrors dt

-- | Cell shapes dmnmd refuses rather than guessing at, mostly about collections.
--
-- __Why this lives here and not in 'mkFEither'.__ Refusing inside the cell
-- constructor is the obvious implementation and it is wrong three times over:
--
--  * 'mkFEither' cannot tell an input cell from an output cell from a
--    __sub-header domain member__ — 'DMN.ParseTable.parseTable' builds @enums@
--    through the same 'mkFs'. A range domain @[0..150]@ on a @[Number]@ column
--    works today; refusing tests in the constructor would make it unwritable.
--  * it knows no row number and no column name, so the message could not locate
--    the offending cell.
--  * @mkFs = either error id@, and 'reprocessRows' calls it with the full column
--    type on live paths, so a 'Left' there crashes ordinary tables.
--
-- Walking 'allrows' fixes all three: the sub-header row is excluded __by
-- construction__ rather than by a special case that could rot.
--
-- Returned rather than thrown, exactly as 'domainErrors' is, so the markdown
-- path can @error@ and the XML path can locate and refuse one table.
structuralErrors :: DecisionTable -> [String]
structuralErrors dt = concat
  [ nestedCols, listInputErrs, listOutputErrs, listArithErrs, hitPolicyErrs ]
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
      , " dmnmd does not implement FEEL function calls, negation or list"
      , " literals in a decision-table cell, and the comma split has already"
      , " broken this one into fragments."
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

    nested ch = case vartype ch of
      Just (DMN_List (DMN_List _)) -> True
      _                            -> False

    isPlainOrWild FAnything    = True
    isPlainOrWild (FNullary _) = True
    isPlainOrWild _            = False

    locate ch rn body = concat
      [ "column ", show (varname ch)
      , maybe "" (\n -> ": row " ++ show n) rn, ": ", body ]

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
domainErrors dt = malformedDomains ++ violations
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
    -- framings — rather than one rule and two implementations.
    msg ch rn cell = concat
      [ "column ", show (varname ch)
      , maybe "" (\n -> ": row " ++ show n) rn
      , ": value outside the column's declared domain {"
      , intercalate ", " (showDomainMember <$> fromJust (enums ch))
      , "} — the cell reads ", showDomainMember cell
      ]

-- | A domain member or cell, as it would have been written in the table.
-- Only used to build the diagnostic in 'domainErrors'.
showDomainMember :: FEELexp -> String
showDomainMember (FNullary (VS s)) = s
showDomainMember (FNullary (VN n)) = show n
showDomainMember (FNullary (VB b)) = toLower <$> show b
showDomainMember (FInRange lk lo hi rk) =
  openBracket lk ++ show lo ++ ".." ++ show hi ++ closeBracket rk
  where openBracket  BClosed = "["
        openBracket  BOpen   = "("
        closeBracket BClosed = "]"
        closeBracket BOpen   = ")"
showDomainMember  FAnything        = "-"
-- A refusal quotes the cell back at the author, so these have to read like the
-- table did — @"> 3"@, not @"FSection Fgt (VN 3.0)"@. Before 'structuralErrors'
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

showFNumFunction :: FNumFunction -> String
showFNumFunction (FNF0 v)       = showDomainMember (FNullary v)
showFNumFunction (FNF1 v)       = v
showFNumFunction (FNF3 l op r)  =
  showFNumFunction l ++ showFNOp2' op ++ showFNumFunction r
  where
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
retypeEnums :: ColHeader -> ColHeader
retypeEnums ch = case enums ch of
  Nothing -> ch
  Just es -> ch { enums = listToMaybe (reprocessRows [ch] [es]) }

reprocessRows :: [ColHeader] -> [[FEELexp]] -> [[FEELexp]]
reprocessRows = 
  -- bang through all columns where the header vartype is Just something, and if the body is FNullary VS, then re- mkF it using the new type info
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
               if vartype ch /= Nothing && (length [ x | FNullary (VS x) <- cells] == length cells)
               then -- Debug.Trace.trace ("reprocessing to " ++ show (vartype ch) ++ ": " ++ show cells) $
                    [ mkF (vartype ch) x | FNullary (VS x) <- cells ]
               else cells)
                         
  
getInputHeaders :: [ColHeader] -> [ColHeader]
getInputHeaders = getWantedHeaders DTCH_In

getOutputHeaders :: [ColHeader] -> [ColHeader]
getOutputHeaders = getWantedHeaders DTCH_Out

getCommentHeaders :: [ColHeader] -> [ColHeader]
getCommentHeaders = getWantedHeaders DTCH_Comment

getWantedHeaders :: DTCH_Label -> [ColHeader] -> [ColHeader]
getWantedHeaders wantedLabel = filter ((wantedLabel==).label)

inferTypes :: ColHeader   -- in or out header column
           -> [[FEELexp]] -- body column of expressions corresponding to that column
           -> ColHeader   -- revised header column with vartype set
inferTypes origch origrows = -- Debug.Trace.trace ("  infertypes: called with colheader = " ++ show origch ++ "\n           and rows = " ++ show origrows) $
  let coltypes = nub $ catMaybes $ do
        cells <- origrows
        inferType <$> cells

  in if length coltypes == 1
     then let coltype = head coltypes
          in if null (vartype origch)
             then origch { vartype = Just coltype }
             else if vartype origch /= Just coltype
                  then -- Debug.Trace.trace ("    vartype for " ++ (varname origch) ++ " is " ++ (show $ vartype origch) ++ "; but inferred type is " ++ (show coltype))
                       origch
                  else origch { vartype = Just coltype }
     else -- Debug.Trace.trace ("    vartype for " ++ (varname origch) ++ " is " ++ (show $ vartype origch) ++ "; but inferred types are " ++ (show coltypes))
          origch

-- initially, we let type inference work for everything except functions.
-- in the future we may need to change the return type from Maybe DMNType to FEELexp (FNumFunction | FNullary)
inferType :: FEELexp -> Maybe DMNType
-- A 'VL' never reaches inference: inference runs over CELLS, and no cell can
-- hold one. A collection column is therefore always explicitly declared —
-- there is no `[Number]` to infer — which is also why 'inferTypes' preserving a
-- declared list type is load-bearing rather than incidental.
inferType (FNullary  (VL _)) = Nothing
inferType (FSection _ (VL _)) = Nothing
inferType (FFunction _) = Just DMN_Number
inferType (FSection _ (VN _)) = Just DMN_Number
inferType (FSection _ (VB _)) = Just DMN_Boolean
inferType (FSection _ (VS _)) = Just DMN_String
inferType (FInRange _ _ _ _)  = Just DMN_Number
inferType  FAnything         = Nothing
inferType (FNullary (VN _)) = Just DMN_Number
inferType (FNullary (VB _)) = Just DMN_Boolean
inferType (FNullary (VS arg))
  | any (arg =~) ["^\\d+(\\.\\d+)?$", "\\.\\.", ">", "<", "="] = Just DMN_Number
  | arg `elem` ["-","_",""] = Nothing
  | (toLower <$> arg) `elem` ["true","yes","positive","y","false","no","negative","n"] = Just DMN_Boolean
  | head arg == '\"' && last arg == '\"' = Just DMN_String
  | any (arg =~) [" \\* ", " \\+ ", " - ", " / ", " \\*\\* "] = Just DMN_Number
  | otherwise = -- Debug.Trace.trace ("inferType " ++ show arg ++ " returning String!") $
      Just DMN_String
