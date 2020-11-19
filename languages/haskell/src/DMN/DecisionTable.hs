{-# LANGUAGE LambdaCase, RecordWildCards, NoOverloadedStrings #-}

module DMN.DecisionTable where

import Control.Arrow
import Prelude hiding (takeWhile)
import DMN.ParseFEEL
import Data.List (transpose, nub, sortOn, sortBy, elemIndex, find)
import Data.Maybe
import Debug.Trace
import DMN.Types
-- import Data.Attoparsec.Text
-- import Text.Megaparsec hiding (label)
import Text.Megaparsec (runParser)
-- import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Char (toLower)

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
fNEval symtab (FNFf FNFmax args) = maximum $ fNEval symtab <$> args
fNEval symtab (FNFf FNFmin args) = minimum $ fNEval symtab <$> args

fromVN :: DMNVal -> Float
fromVN (VN n) = n
fromVN (VB True) = 1.0
fromVN (VB False) = 0.0
fromVN _ = error "type error: tried to read a float out of a string"

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
                                     (transpose $ [ row_inputs r ++  row_outputs r
                                                  | r@DTrow{} <- origdtrows])
      typedchs = if not (null newchs) then newchs ++ getCommentHeaders origchs else origchs
  in Debug.Trace.trace ("mkDTable: finishing...\n" ++
                        "origchs = " ++ show(origchs) ++ "\n" ++
                        "newchs = " ++ show(newchs) ++ (if null (getCommentHeaders origchs) then "" else " (yes the comment columns don't show here)\n" ))
    DTable origname orighp typedchs
    ((\case
         (DTrow rn ri ro rc) -> (DTrow rn
                        (reprocessInRows  (getInputHeaders  typedchs) ri)
                        (reprocessOutRows (                 typedchs) ro)
                        rc)) <$> origdtrows)

reprocessInRows :: [ColHeader] -> [[FEELexp]] -> [[FEELexp]]
reprocessInRows = 
  -- bang through all columns where the header vartype is Just something, and if the body is FNullary VS, then re-parse it using the new type info
  zipWith (\ch cells ->
             -- Debug.Trace.trace ("** reprocessRows: have the option to reprocess cells to " ++ show (vartype ch) ++ ": " ++ show cells) $
               if notElem (vartype ch) [Nothing]
                  && (length [ x | FNullary (VS x) <- cells] == length cells)
               then -- Debug.Trace.trace ("reprocessing to " ++ show (vartype ch) ++ ": " ++ show cells) $
                    concat [ either (error.show) id (runParser (parseDataCell (vartype ch)) "type-inferred reprocessing for input columns" (T.pack x))
                    | FNullary (VS x) <- cells ]
               else -- Debug.Trace.trace ("declining to reprocess because we probably got it right the first time") $
                 cells)

-- rewrite any `bareword strings` from FNullary VS to FFunction FNF1 if they're found in the symbol table.
reprocessOutRows :: [ColHeader] -> [[FEELexp]] -> [[FEELexp]]
reprocessOutRows allchs colcells =
  let inputchs  = getInputHeaders allchs
      symtab = (toLower <$>) <$> varname <$> inputchs
      outputchs = getOutputHeaders allchs
      pass1 = zipWith (\ch cells ->
               if notElem (vartype ch) [Nothing]
                  && (length [ x | FNullary (VS x) <- cells] == length cells)
               then let myout = [ either (error.show) (id)
                                  (runParser (parseFEELext (vartype ch))
                                   "type-inferred reprocessing for output columns" (T.pack x))
                                | FNullary (VS x) <- cells ]
                    in Debug.Trace.trace (unlines [ unwords [ "reprocessing outputs to ",
                                                              show (vartype ch), ": ", show cells ]
                                                  , unwords [ "result: ", show myout ] ] ) myout
               else Debug.Trace.trace ("declining to reprocess output cells") $
                 cells) outputchs colcells
  in (\cells -> do
         cell <- cells
         return $ case cell of
                    FNullary (VS x) -> if (toLower <$> x) `elem` symtab
                                       then FFunction (FNF1 x)
                                       else cell
                    _               -> cell
     ) <$> pass1
  
getInputHeaders :: [ColHeader] -> [ColHeader]
getInputHeaders = getWantedHeaders DTCH_In

getOutputHeaders :: [ColHeader] -> [ColHeader]
getOutputHeaders = getWantedHeaders DTCH_Out

getCommentHeaders :: [ColHeader] -> [ColHeader]
getCommentHeaders = getWantedHeaders DTCH_Comment

getWantedHeaders :: DTCH_Label -> [ColHeader] -> [ColHeader]
getWantedHeaders wantedLabel = filter ((wantedLabel==).label)

-- TODO: we should treat input and output columns slightly differently:
--  input columns can be comma-separated
--  output columns can be FNumFunctions
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
                  then Debug.Trace.trace ("    vartype for " ++ (varname origch) ++ " is " ++ (show $ vartype origch) ++ "; but inferred type is " ++ (show coltype))
                       origch
                  else origch { vartype = Just coltype }
     else Debug.Trace.trace ("    length coltypes > 1, no changes due to type inference")
          origch

-- initially, we let type inference work for everything except functions.
-- in the future we may need to change the return type from Maybe DMNType to FEELexp (FNumFunction | FNullary)

-- the FEELexp might have already been typed as something other than FNullary VS because the column header carried an explicit type annotation
-- so we'll just basically pass those through
-- but we will need to take a more critical look at anything that came in as an FNullary VS
inferType :: FEELexp -> Maybe DMNType
inferType (FFunction _) = Just DMN_Number
inferType (FSection _ (VN _)) = Just DMN_Number
inferType (FSection _ (VB _)) = Just DMN_Boolean
inferType (FSection _ (VS _)) = Just DMN_String
inferType (FInRange _ _)      = Just DMN_Number
inferType  FAnything         = Nothing
inferType (FNullary (VN _)) = Just DMN_Number
inferType (FNullary (VB _)) = Just DMN_Boolean

-- parse, don't validate!
inferType (FNullary (VS arg)) = either (error.show) id $ runParser parseDMNType "type inference" (T.pack arg)

