{-# LANGUAGE LambdaCase #-}
-- Ignore these for now
{-# OPTIONS_GHC -Wno-unused-matches #-}

module DMN.Translate.PY where

-- in a future iteration of this code, consider using http://hackage.haskell.org/package/js-good-parts

import DMN.DecisionTable
import Data.List
import Data.Maybe
import Data.Char
import DMN.Types
import DMN.Translate.FEELhelpers

data PYOpts = PYOpts { propstyle :: Bool
                     , typescript :: Bool }

toPY :: PYOpts -> DecisionTable -> String
-- https://github.com/faylang/fay/wiki
toPY jsopts dt =
  unlines $ ( --this section creates the function header
             [ unwords $ concat [ mkFunction (tableName dt)                                 
             , mkArguments jsopts (header dt)
                                , [ ":"]
                                ] ] 
             -- this section creates the relevant function body
             ++ zipWith (\if_ dtrow -> mkIf jsopts (hitpolicy dt) if_ (header dt) dtrow) elsif (datarows dt)             
            )   
  where
    elsif = "if" : repeat ( case hitpolicy dt of
                               HP_Unique    -> "elif"
                               HP_First     -> "elif"
                               HP_Priority  -> "elif"
                               HP_Collect _ -> "if"
                               _            -> "if")
  
mkFunction tablename = [ "def", underscore tablename ]  -- helper function for function boilerplate 

-- helper function for generating (arguments)
mkArguments :: PYOpts -> [ColHeader] -> [String]
mkArguments pyopts chs = ["(", intercalate ", " (mkArgument pyopts <$> input_headers chs), ")"]

-- helper function for generating individual arguments that go into (args)
mkArgument :: PYOpts -> ColHeader -> String
mkArgument pyopts ch = var_name ch ++ maybe "" (if typescript pyopts then (" : " ++) . type2py else const "") (vartype ch) 

type2py :: DMNType -> String
type2py DMN_String    = "str"
type2py DMN_Number    = "float"
type2py DMN_Boolean   = "bool"
type2py (DMN_List x)  = type2py x ++ "[]"

mkIf :: PYOpts -> HitPolicy -> String -> [ColHeader] -> DTrow -> String
mkIf jsopts hp ifword chs dtrow =
  let conditions = uncurry (fexp2js jsopts) <$> catMaybes ( zipWith nonBlankCols (input_headers chs) (row_inputs dtrow) )
  in
    "  " ++ ifword ++ " (" ++
    (if not (null conditions)
     then intercalate " && " conditions
     else "\"default\"") -- TODO: tweak ifword to allow just an "else" here, rather than exploiting the truthiness of JS
    ++ "): # " ++
    maybe "cont'd" show (row_number dtrow) ++ "\n" ++
    (let feelout = feel2pyOut hp chs dtrow
         standard = maybe "" (\infra -> "    " ++ infra ++ "\n") (fst feelout) ++ "    return {" ++ intercalate ", " (snd feelout) ++ "};"
     in
     case hp of
       HP_Unique    -> standard
       HP_Any       -> standard
       HP_Collect _ -> standard
       HP_First     -> standard
       HP_Priority  -> standard
       HP_OutputOrder -> standard
       HP_RuleOrder -> standard
       HP_Aggregate -> standard
    )
    ++ "\n"
    ++ annotationsAsComments chs dtrow

-- if the row has multiple annotation columns, show the varname of the column header.
-- if there is only one visible annotation column, hide the varname of the column header.
annotationsAsComments :: [ColHeader] -> DTrow -> String
annotationsAsComments chs dtrow =
  let prefixedComments = catMaybes $ zipWith (\cheader commentcol -> ((varname cheader ++ ": ") ++) <$> commentcol) (comment_headers chs) (row_comments dtrow)
      unprefixed = catMaybes $ row_comments dtrow
  in
  unlines $ ("    # "++) <$> (if length unprefixed > 1 then prefixedComments else unprefixed)

fexp2js :: PYOpts -> ColHeader -> [FEELexp] -> String
fexp2js jsopts ch fexps = wrapParen " || " (feel2pyIn ( showVarname jsopts ch) <$> fexps)

showVarname :: PYOpts -> ColHeader -> String
showVarname jsopts ch
  | propstyle jsopts = "props[\"" ++ varname ch ++ "\"]"
  | otherwise        = var_name ch
               
wrapParen :: String -> [String] -> String
wrapParen myop xs
  | length xs  > 1 = "(" ++ intercalate myop xs ++ ")"
  | length xs == 1 = head xs
  | otherwise      = "null"
wrapArray :: String -> [String] -> String
wrapArray myop xs = "[" ++ intercalate myop xs ++ "]"

nonBlankCols :: a -> [FEELexp] -> Maybe (a, [FEELexp])
nonBlankCols chs dtrows = if dtrows /= [FAnything] then Just (chs, dtrows) else Nothing

input_headers :: [ColHeader] -> [ColHeader]
input_headers   = filter ((DTCH_In==).label)

comment_headers :: [ColHeader] -> [ColHeader]
comment_headers = filter ((DTCH_Comment==).label)

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

feel2pyIn :: String -> FEELexp -> String
feel2pyIn lhs  FAnything = wrapParen "||" ["True",lhs]
feel2pyIn lhs (FSection Feq (VB rhs))  = lhs ++ "==" ++ capitalize (toLower <$> show rhs)
feel2pyIn lhs (FSection Feq (VN rhs))  = lhs ++ "==" ++ show rhs
feel2pyIn lhs (FSection Feq (VS rhs))  = lhs ++ "==" ++ show rhs
feel2pyIn lhs (FSection Flt  (VN rhs)) = lhs ++ " < "  ++ show rhs
feel2pyIn lhs (FSection Flte (VN rhs)) = lhs ++ " <="  ++ show rhs
feel2pyIn lhs (FSection Fgt  (VN rhs)) = lhs ++ " > "  ++ show rhs
feel2pyIn lhs (FSection Fgte (VN rhs)) = lhs ++ " >="  ++ show rhs
feel2pyIn lhs (FInRange lower upper)   = wrapParen " && " [show lower ++ "<=" ++ lhs, lhs ++ "<=" ++ show upper]
feel2pyIn lhs (FNullary rhs)           = feel2pyIn lhs (FSection Feq rhs)

-- TODO:
-- let's extend FEEL with support for PCRE lol

-- if there's a single output column then we just return that value
-- if there are multiple output columns then we construct an object with multiple properties and return the object.

-- we could treat each output column as a lambda with access to the input namespace.
-- if there's only one input column, then we honour sections by returning the boolean result of operating against the unnamed input
-- if there are multiple input columns then we require explicit use of the input column varname

feel2pyOut :: HitPolicy -> [ColHeader] -> DTrow -> (Maybe String, [String])
feel2pyOut hp chs dtrow
  -- ("// one input column, allowing binary operators in output column(s)"
  = (Nothing, -- "toreturn[thiscolumn] = ..."
     uncurry showFeels <$> zip
                             (filter ((DTCH_Out==).label) chs)
                             (row_outputs dtrow))
