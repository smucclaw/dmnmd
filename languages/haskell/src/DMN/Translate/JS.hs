{-# LANGUAGE LambdaCase #-}
-- Ignore these for now
{-# OPTIONS_GHC -Wno-unused-matches #-}

module DMN.Translate.JS where

-- in a future iteration of this code, consider using http://hackage.haskell.org/package/js-good-parts

import DMN.DecisionTable
import Data.List
import Data.Maybe
import Data.Char
import DMN.Types
import DMN.Translate.FEELhelpers

toJS :: Opts -> DecisionTable -> String
-- https://github.com/faylang/fay/wiki
toJS opts dt =
  unlines $ (if propstyle opts && typehints opts
              then mkArgSpec opts (tableName dt) (header dt)
                   <> mkReturnSpec opts (tableName dt) (header dt)
              else mempty) ++
             [ unwords $ concat [ mkFunction (tableName dt)
                                , if propstyle opts
                                  then mkProps opts (tableName dt) (getInputHeaders $ header dt)
                                       ++ ([": " ++ returnName (tableName dt) | typehints opts])
                                  else mkArguments opts (header dt)
                                , [ "{"]
                                ] ]
             ++ zipWith (\if_ dtrow -> mkIf opts (hitpolicy dt) if_ (header dt) dtrow) elsif (datarows dt)
             ++ [ "}" ]
  where
    elsif = "if" : repeat ( case hitpolicy dt of
                               HP_Unique    -> "else if"
                               HP_First     -> "else if"
                               HP_Priority  -> "else if"
                               HP_Collect _ -> "if"
                               _            -> "if")
  
mkArgSpec :: Opts -> String -> [ColHeader] -> [String]
mkArgSpec opts tablename chs = mkTypeSpec opts (propsName tablename) (getInputHeaders chs)

mkReturnSpec :: Opts -> String -> [ColHeader] -> [String]
mkReturnSpec opts tablename chs = mkTypeSpec opts (returnName tablename) (getOutputHeaders chs)

mkTypeSpec :: Opts -> String -> [ColHeader] -> [String]
mkTypeSpec opts specname chs =
  ["type " ++ specname ++ " = {"] ++
  ["    \"" ++ varname ch ++ "\" : " ++ maybe "any" type2js (vartype ch) ++ ";" | ch <- chs ] ++
  ["}"]

mkFunction :: String -> [String]
mkFunction tablename = [ "export", "function", underscore tablename ]

mkProps :: Opts -> String -> [ColHeader] -> [String]
mkProps opts tablename chs = ["(", "props" ++ if typehints opts then " : " ++ propsName tablename else "", ")"]

propsName :: String -> String
propsName tablename = "Props_" ++ underscore tablename

returnName :: String -> String
returnName tablename = "Return_" ++ underscore tablename

mkArguments :: Opts -> [ColHeader] -> [String]
mkArguments opts chs = ["(", intercalate ", " (mkArgument opts <$> input_headers chs), ")"]

mkArgument :: Opts -> ColHeader -> String
mkArgument opts ch = var_name ch ++ maybe "" (if typehints opts then (" : " ++) . type2js else const "") (vartype ch)

type2js :: DMNType -> String
type2js DMN_String    = "string"
type2js DMN_Number    = "number"
type2js DMN_Boolean   = "boolean"
type2js (DMN_List x)  = type2js x ++ "[]"

mkIf :: Opts -> HitPolicy -> String -> [ColHeader] -> DTrow -> String
mkIf opts hp ifword chs dtrow =
  let conditions = uncurry (fexp2js opts) <$> catMaybes ( zipWith nonBlankCols (input_headers chs) (row_inputs dtrow) )
  in
    "  " ++ ifword ++ " (" ++
    (if not (null conditions)
     then intercalate " && " conditions
     else "\"default\"") -- TODO: tweak ifword to allow just an "else" here, rather than exploiting the truthiness of JS
    ++ ") { // " ++
    maybe "cont'd" show (row_number dtrow) ++ "\n" ++
    (let feelout = feel2jsOut hp chs dtrow
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
    ++ "  }"

-- if the row has multiple annotation columns, show the varname of the column header.
-- if there is only one visible annotation column, hide the varname of the column header.
annotationsAsComments :: [ColHeader] -> DTrow -> String
annotationsAsComments chs dtrow =
  let prefixedComments = catMaybes $ zipWith (\cheader commentcol -> ((varname cheader ++ ": ") ++) <$> commentcol) (comment_headers chs) (row_comments dtrow)
      unprefixed = catMaybes $ row_comments dtrow
  in
  unlines $ ("    // "++) <$> (if length unprefixed > 1 then prefixedComments else unprefixed)

fexp2js :: Opts -> ColHeader -> [FEELexp] -> String
fexp2js opts ch fexps = wrapParen " || " (feel2jsIn ( showVarname opts ch) <$> fexps)

showVarname :: Opts -> ColHeader -> String
showVarname opts ch
  | propstyle opts = "props[\"" ++ varname ch ++ "\"]"
  | otherwise        = var_name ch
               

feel2jsIn :: String -> FEELexp -> String
feel2jsIn lhs  FAnything = wrapParen "||" ["true",lhs]
feel2jsIn lhs (FSection Feq (VB rhs))  = lhs ++ showFNComp "ts" FNEq  ++ (toLower <$> show rhs)
feel2jsIn lhs (FSection Feq (VN rhs))  = lhs ++ showFNComp "ts" FNEq  ++ show rhs
feel2jsIn lhs (FSection Feq (VS rhs))  = lhs ++ showFNComp "ts" FNEq  ++ show rhs
feel2jsIn lhs (FSection Flt  (VN rhs)) = lhs ++ showFNComp "ts" FNLt  ++ show rhs
feel2jsIn lhs (FSection Flte (VN rhs)) = lhs ++ showFNComp "ts" FNLeq ++ show rhs
feel2jsIn lhs (FSection Fgt  (VN rhs)) = lhs ++ showFNComp "ts" FNGt  ++ show rhs
feel2jsIn lhs (FSection Fgte (VN rhs)) = lhs ++ showFNComp "ts" FNGeq ++ show rhs
feel2jsIn lhs (FInRange lower upper)   = wrapParen (showFNLog "ts" FNAnd) [show lower ++ showFNComp "ts" FNLeq ++ lhs, lhs ++ showFNComp "ts" FNLeq ++ show upper]
feel2jsIn lhs (FNullary rhs)           = feel2jsIn lhs (FSection Feq rhs)
feel2jsIn lhs rhs                      = error $ "feel2jsIn: unhandled case " ++ lhs ++ " / " ++ show rhs

-- TODO:
-- let's extend FEEL with support for PCRE lol

-- if there's a single output column then we just return that value
-- if there are multiple output columns then we construct an object with multiple properties and return the object.

-- we could treat each output column as a lambda with access to the input namespace.
-- if there's only one input column, then we honour sections by returning the boolean result of operating against the unnamed input
-- if there are multiple input columns then we require explicit use of the input column varname

feel2jsOut :: HitPolicy -> [ColHeader] -> DTrow -> (Maybe String, [String])
feel2jsOut hp chs dtrow
  -- ("// one input column, allowing binary operators in output column(s)"
  = (Nothing, -- "toreturn[thiscolumn] = ..."
     uncurry (showFeels "js") <$> zip
                                    (filter ((DTCH_Out==).label) chs)
                                    (row_outputs dtrow))

lambdaHeader :: String
lambdaHeader = "(x)=> "
