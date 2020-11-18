{-# LANGUAGE LambdaCase #-}
-- Ignore these for now
{-# OPTIONS_GHC -Wno-unused-matches #-}

module DMN.Translate.PY where

-- in a future iteration of this code, consider using http://hackage.haskell.org/package/js-good-parts

import Data.List
import Data.Maybe
import Data.Char
import DMN.Types
import DMN.Translate.FEELhelpers

toPY :: Opts -> DecisionTable -> String
-- https://github.com/faylang/fay/wiki
toPY opts dt =
  unlines $ ( --this section creates the function header
             [ unwords $ concat [ mkFunction (tableName dt)                                 
             , mkArguments opts (header dt)
                                , [ ":"]
                                ] ] 
             -- this section creates the relevant function body
             ++ zipWith (\if_ dtrow -> mkIf opts (hitpolicy dt) if_ (header dt) dtrow) elsif (datarows dt)             
            )   
  where
    elsif = "if" : repeat ( case hitpolicy dt of
                               HP_Unique    -> "elif"
                               HP_First     -> "elif"
                               HP_Priority  -> "elif"
                               HP_Collect _ -> "if"
                               _            -> "if")

mkFunction :: String -> [String]  
mkFunction tablename = [ "def", underscore tablename ]  -- helper function for function boilerplate 

-- helper function for generating (arguments)
mkArguments :: Opts -> [ColHeader] -> [String]
mkArguments pyopts chs = ["(", intercalate ", " (mkArgument pyopts <$> input_headers chs), ")"]

-- helper function for generating individual arguments that go into (args)
mkArgument :: Opts -> ColHeader -> String
mkArgument pyopts ch = var_name ch ++ maybe "" (const "") (vartype ch) 

type2py :: DMNType -> String
type2py DMN_String    = "str"
type2py DMN_Number    = "float"
type2py DMN_Boolean   = "bool"
type2py (DMN_List x)  = type2py x ++ "[]"

mkIf :: Opts -> HitPolicy -> String -> [ColHeader] -> DTrow -> String
mkIf opts hp ifword chs dtrow =
  let conditions = uncurry (fexp2js opts) <$> catMaybes ( zipWith nonBlankCols (input_headers chs) (row_inputs dtrow) )
  in
    "  " ++ ifword ++ " (" ++
    (if not (null conditions)
     then intercalate " and " conditions
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

fexp2js :: Opts -> ColHeader -> [FEELexp] -> String
fexp2js opts ch fexps = wrapParen " or " (feel2pyIn ( showVarname opts ch) <$> fexps)

showVarname :: Opts -> ColHeader -> String
showVarname opts ch
  | propstyle opts = "props[\"" ++ varname ch ++ "\"]"
  | otherwise        = var_name ch
               

feel2pyIn :: String -> FEELexp -> String
feel2pyIn lhs  FAnything = wrapParen "or" ["True",lhs]
feel2pyIn lhs (FSection Feq (VB rhs))  = lhs ++ showFNComp "py" FNEq  ++ capitalize (toLower <$> show rhs)
feel2pyIn lhs (FSection Feq (VN rhs))  = lhs ++ showFNComp "py" FNEq  ++ show rhs
feel2pyIn lhs (FSection Feq (VS rhs))  = lhs ++ showFNComp "py" FNEq  ++ show rhs
feel2pyIn lhs (FSection Flt  (VN rhs)) = lhs ++ showFNComp "py" FNLt  ++ show rhs
feel2pyIn lhs (FSection Flte (VN rhs)) = lhs ++ showFNComp "py" FNLeq ++ show rhs
feel2pyIn lhs (FSection Fgt  (VN rhs)) = lhs ++ showFNComp "py" FNGt  ++ show rhs
feel2pyIn lhs (FSection Fgte (VN rhs)) = lhs ++ showFNComp "py" FNGeq ++ show rhs
feel2pyIn lhs (FInRange lower upper)   = wrapParen (showFNLog "py" FNAnd) [show lower ++ showFNComp "py" FNLeq ++ lhs, lhs ++ showFNComp "py" FNLeq ++ show upper]
feel2pyIn lhs (FNullary rhs)           = feel2pyIn lhs (FSection Feq rhs)
feel2pyIn lhs rhs                      = error $ "feel2pyIn: unhandled case " ++ lhs ++ " / " ++ show rhs

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
     uncurry (showFeels "py") <$> zip
                                  (filter ((DTCH_Out==).label) chs)
                                  (row_outputs dtrow))

lambdaHeader :: String
lambdaHeader = "lambda x: "
