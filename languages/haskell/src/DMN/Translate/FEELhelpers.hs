{-# LANGUAGE LambdaCase #-}

module DMN.Translate.FEELhelpers where

import Data.Char
import Data.List
import DMN.Types

capitalize :: String -> String -- This file is the "root" for generating the if-else condittions, and should be imported in fileformat-specific translation scripts, so I thought it'd be best to place this here
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

-- Helper functions are are intended for use in generating lambda 
-- functions in the respective outputs (ts, js, python)
lambdaHeader :: String -> String
lambdaHeader optform
  | optform == "py" = "lambda x: "
  | (optform == "ts") || (optform == "js") = "(x)=> "
  | otherwise = error "Currently only ts, js and py formats are supported"


wrapArray :: String -> [String] -> String
wrapArray myop xs = "[" ++ intercalate myop xs ++ "]"

showFeels optform ch fexps = "\"" ++ varname ch ++ "\":" ++ if squash
                                                    then showFeel optform  $ head fexps
                                                    else wrapArray "," (showFeel optform <$> fexps)
  where squash = maybe True (\case
                                DMN_List _ -> False
                                _          -> True) (vartype ch)
          
showFeel :: String -> FEELexp -> String
showFeel _ (FNullary (VS str))  = show str
showFeel _ (FNullary (VN num))  = show num
showFeel optform (FNullary (VB bool)) = if optform == "py" then capitalize (toLower <$> show bool) else toLower <$> show bool 
showFeel optform (FSection Feq  (VB rhs)) = lambdaHeader optform ++ "x" ++ showFNComp optform FNEq  ++ (toLower <$> show rhs)
showFeel optform (FSection Feq  (VS rhs)) = lambdaHeader optform ++ "x" ++ showFNComp optform FNEq  ++ show rhs
showFeel optform (FSection Feq  (VN rhs)) = lambdaHeader optform ++ "x" ++ showFNComp optform FNEq  ++ show rhs
showFeel optform (FSection Flt  (VN rhs)) = lambdaHeader optform ++ "x" ++ showFNComp optform FNLt  ++ show rhs
showFeel optform (FSection Flte (VN rhs)) = lambdaHeader optform ++ "x" ++ showFNComp optform FNLeq ++ show rhs
showFeel optform (FSection Fgt  (VN rhs)) = lambdaHeader optform ++ "x" ++ showFNComp optform FNGt  ++ show rhs
showFeel optform (FSection Fgte (VN rhs)) = lambdaHeader optform ++ "x" ++ showFNComp optform FNGeq ++show rhs
showFeel optform (FInRange lower upper)   = lambdaHeader optform ++ show lower ++ showFNComp optform FNLeq ++ "x" 
                                            ++ showFNLog optform FNAnd ++ "x" ++ showFNComp optform FNLeq  ++ show upper
showFeel _ (FFunction (FNF1 var))     = var
showFeel _ (FFunction (FNF0 (VS str))) = "\"" ++ str ++ "\""
showFeel _ (FFunction (FNF0 (VB bool))) = toLower <$> show bool
showFeel _ (FFunction (FNF0 (VN num)))  = show num
showFeel optform (FFunction (FNF3 lhs fnop2 rhs))  = "(" ++ showFeel optform (FFunction lhs) ++ showFNOp2 fnop2 ++ showFeel optform (FFunction rhs) ++ ")"
showFeel  _ FAnything               = "undefined"

showFNOp2 :: FNOp2 -> String
showFNOp2 FNMul   = " * "
showFNOp2 FNDiv   = " / "
showFNOp2 FNPlus  = " + "
showFNOp2 FNMinus = " - "
showFNOp2 FNExp   = " ** "

showFNLog :: String -> FNLog -> String
showFNLog "py" FNNot   = "not  "
showFNLog "py" FNAnd  = " and "
showFNLog "py" FNOr   = " or "
showFNLog "ts" FNNot   = "!"
showFNLog "ts" FNAnd  = " && "
showFNLog "ts" FNOr   = " || "
showFNLog "js" x      = showFNLog "ts" x

showFNComp :: String -> FNComp -> String
showFNComp "py" FNEq  = " == "
showFNComp "py" FNNeq = " != "
showFNComp "py" FNLt  = " < "
showFNComp "py" FNLeq = " <= "
showFNComp "py" FNGt  = " > "
showFNComp "py" FNGeq = " >= "
showFNComp "ts" FNEq  = " === "
showFNComp "ts" FNNeq = " !== "
showFNComp "ts" x     = showFNComp "py" x
showFNComp "js" x     = showFNComp "ts" x
