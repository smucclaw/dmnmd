{-# LANGUAGE LambdaCase, RecordWildCards, NoOverloadedStrings #-}

module DMN.DecisionTable where

{-| Given an ASCII decision table, parse it and transpile it to operational languages like JS and Python. -}

import Control.Arrow ( (<<<), (>>>) )
import Prelude hiding (takeWhile)
import DMN.ParseFEEL ( parseFNumFunction )
import Data.List (dropWhileEnd, transpose, nub, sortOn, sortBy, elemIndex, intersect, isPrefixOf, isSuffixOf, find)
import Data.List.Split ( splitOn )
import Data.Maybe ( catMaybes, fromJust )
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
mkFEither t@(Just (DMN_List _)) x  = mkFEither (baseType t) x
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
    in Right (FInRange ((read $ head bounds) :: Float) ((read $ bounds!!1) :: Float))
  | "="  `isPrefixOf` arg2 = FSection Feq  <$> (mkVN $ trim $ dropWhile    (=='=') arg2)
  | "="  `isSuffixOf` arg2 = FSection Feq  <$> (mkVN $ trim $ dropWhileEnd (=='=') arg2)
  | otherwise              = FNullary      <$> (mkVN $ trim                        arg2)
  where arg2 = trim arg1 -- probably extraneous
        mkVN x = maybe (Left $ "expected a number, but this column is typed Number and the cell reads " ++ show arg2)
                       (Right . VN)
                       (readMaybe x :: Maybe Float)

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
  -- alternative phrasing without arrows: (snd . fromJust . (find ((== f) . fst))
fEval (FSection f    (VN rhs)) (FNullary (VN lhs)) = (find ((== f) <<< fst) >>> fromJust >>> snd)
                                                      [(Flt,(<)), (Flte,(<=)), (Fgt,(>)), (Fgte,(>=)), (Feq,(==))]
                                                     lhs rhs
fEval (FInRange lower upper)   (FNullary (VN lhs)) = lower <= lhs && lhs <= upper
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
      typedchs = if not (null newchs) then newchs ++ getCommentHeaders origchs else origchs
  in -- Debug.Trace.trace ("mkDTable: finishing...\n" ++
        --                 "origchs = " ++ show(origchs) ++ "\n" ++
           --             "newchs = " ++ show(newchs) ++ "\n" )
    DTable origname orighp typedchs
    ((\case
         (DTrow rn ri ro rc) -> (DTrow rn
                        (reprocessRows (getInputHeaders typedchs)  ri)
                        (reprocessRows (getOutputHeaders typedchs) ro)
                        rc)) <$> origdtrows)
                         
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
inferType (FFunction _) = Just DMN_Number
inferType (FSection _ (VN _)) = Just DMN_Number
inferType (FSection _ (VB _)) = Just DMN_Boolean
inferType (FSection _ (VS _)) = Just DMN_String
inferType (FInRange _ _)      = Just DMN_Number
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
