{-# LANGUAGE LambdaCase #-}

module DMN.Translate.FEELhelpers where

import Data.Char
import Data.List
import DMN.Types

wrapArray :: String -> [String] -> String
wrapArray myop xs = "[" ++ intercalate myop xs ++ "]"

showFeels ch fexps = "\"" ++ varname ch ++ "\":" ++ if squash
                                                    then showFeel $ head fexps
                                                    else wrapArray "," (showFeel <$> fexps)
  where squash = maybe True (\case
                                DMN_List _ -> False
                                _          -> True) (vartype ch)
          
showFeel :: FEELexp -> String
showFeel (FNullary (VS str))  = show str
showFeel (FNullary (VN num))  = show num
showFeel (FNullary (VB bool)) = toLower <$> show bool
showFeel (FSection Feq  (VB rhs)) = "(x)=>x === "++ (toLower <$> show rhs)
showFeel (FSection Feq  (VS rhs)) = "(x)=>x === "++ show rhs
showFeel (FSection Feq  (VN rhs)) = "(x)=>x === "++ show rhs
showFeel (FSection Flt  (VN rhs)) = "(x)=>x < "++show rhs
showFeel (FSection Flte (VN rhs)) = "(x)=>x <="++show rhs
showFeel (FSection Fgt  (VN rhs)) = "(x)=>x > "++show rhs
showFeel (FSection Fgte (VN rhs)) = "(x)=>x >="++show rhs
showFeel (FInRange lower upper)   = "(x)=>" ++ show lower ++ "<= x && x <= " ++ show upper
showFeel (FFunction (FNF1 var))     = var
showFeel (FFunction (FNF0 (VS str))) = "\"" ++ str ++ "\""
showFeel (FFunction (FNF0 (VB bool))) = toLower <$> show bool
showFeel (FFunction (FNF0 (VN num)))  = show num
showFeel (FFunction (FNF3 lhs fnop2 rhs))  = "(" ++ showFeel (FFunction lhs) ++ showFNOp2 fnop2 ++ showFeel (FFunction rhs) ++ ")"
showFeel  FAnything               = "undefined"

showFNOp2 :: FNOp2 -> String
showFNOp2 FNMul   = " * "
showFNOp2 FNDiv   = " / "
showFNOp2 FNPlus  = " + "
showFNOp2 FNMinus = " - "
showFNOp2 FNExp   = " ** "
