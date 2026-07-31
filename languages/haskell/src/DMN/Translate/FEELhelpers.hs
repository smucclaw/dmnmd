{-# LANGUAGE LambdaCase #-}

module DMN.Translate.FEELhelpers where

import Data.Char
import Data.List
import DMN.Number (showNumFloatish)
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
                                                    then squashed
                                                    else wrapArray "," (showFeel optform <$> members)
  where squash = not (isListCol ch)
        -- A wildcard OUTPUT cell in a collection column is the EMPTY list, not
        -- a one-element list holding a wildcard. Before this, `-` rendered as
        -- `{"sides":[undefined]}`: length 1 in JS where it should be 0, and a
        -- NameError in Python, since `undefined` is not a Python name.
        members = filter (/= FAnything) fexps
        squashed = case fexps of
          (f:_) -> showFeel optform f
          []    -> error $ "showFeels: empty cell for column " ++ show (varname ch)
          
showFeel :: String -> FEELexp -> String
showFeel _ (FNullary (VS str))  = show str
showFeel _ (FNullary (VN num))  = showNumFloatish num
showFeel optform (FNullary (VB bool)) = if optform == "py" then capitalize (toLower <$> show bool) else toLower <$> show bool 
showFeel optform (FSection Feq  (VB rhs)) = lambdaHeader optform ++ "x" ++ showFNComp optform FNEq  ++ (toLower <$> show rhs)
showFeel optform (FSection Feq  (VS rhs)) = lambdaHeader optform ++ "x" ++ showFNComp optform FNEq  ++ show rhs
showFeel optform (FSection Feq  (VN rhs)) = lambdaHeader optform ++ "x" ++ showFNComp optform FNEq  ++ showNumFloatish rhs
showFeel optform (FSection Flt  (VN rhs)) = lambdaHeader optform ++ "x" ++ showFNComp optform FNLt  ++ showNumFloatish rhs
showFeel optform (FSection Flte (VN rhs)) = lambdaHeader optform ++ "x" ++ showFNComp optform FNLeq ++ showNumFloatish rhs
showFeel optform (FSection Fgt  (VN rhs)) = lambdaHeader optform ++ "x" ++ showFNComp optform FNGt  ++ showNumFloatish rhs
showFeel optform (FSection Fgte (VN rhs)) = lambdaHeader optform ++ "x" ++ showFNComp optform FNGeq ++showNumFloatish rhs
showFeel optform (FInRange lk lower upper rk) =
  lambdaHeader optform ++ showNumFloatish lower ++ showFNComp optform (leq lk) ++ "x"
  ++ showFNLog optform FNAnd ++ "x" ++ showFNComp optform (leq rk) ++ showNumFloatish upper
  where leq BClosed = FNLeq
        leq BOpen   = FNLt
showFeel _ (FFunction (FNF1 var))     = var
showFeel _ (FFunction (FNF0 (VS str))) = "\"" ++ str ++ "\""
showFeel _ (FFunction (FNF0 (VB bool))) = toLower <$> show bool
showFeel _ (FFunction (FNF0 (VN num)))  = showNumFloatish num
showFeel optform (FFunction (FNF3 lhs fnop2 rhs))  = "(" ++ showFeel optform (FFunction lhs) ++ showFNOp2 fnop2 ++ showFeel optform (FFunction rhs) ++ ")"
showFeel  _ FAnything               = "undefined"
-- The remaining shapes are ordering comparisons over a non-numeric value
-- (@FSection Flt (VS …)@ and friends): @< "Fall"@ has no meaning, and nothing
-- upstream can build one, because 'DMN.DecisionTable.mkFEither' only produces an
-- ordering 'FSection' in the 'DMN_Number' arm. Named rather than left to a bare
-- @Non-exhaustive patterns@, so that if a future cell shape does reach here the
-- message says which one.
showFeel optform fexp = error $ unwords
  [ "showFeel: no", optform, "rendering for", show fexp
  , "-- an ordering comparison against a non-numeric value is not a thing dmnmd can emit"
  ]

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
-- 'lambdaHeader' already rejects any optform outside {py,ts,js}; this arm says so
-- at the same volume instead of dying with a bare @Non-exhaustive patterns@.
showFNLog optform x   = error $ unwords
  [ "showFNLog: unsupported output format", show optform, "for", show x
  , "-- only py, ts and js are supported" ]

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
showFNComp optform x  = error $ unwords
  [ "showFNComp: unsupported output format", show optform, "for", show x
  , "-- only py, ts and js are supported" ]
