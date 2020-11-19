{-# LANGUAGE OverloadedStrings #-}

module DMN.Types where

-- definitions common to DecisionTable and DMNParseTable

import Prelude hiding (takeWhile)
import qualified Data.Map as Map
import Data.List.Utils (replace)

data HitPolicy = HP_Unique
               | HP_Any
               | HP_Priority
               | HP_First
               | HP_OutputOrder
               | HP_RuleOrder
               | HP_Collect CollectOperator
               | HP_Aggregate
               deriving (Show, Eq)

data CollectOperator = Collect_Sum -- +
                     | Collect_Min -- <
                     | Collect_Max -- >
                     | Collect_Cnt -- #
                     | Collect_All --
               deriving (Show, Eq)

-- INSIGHT
-- input columns always evaluate to a boolean somehow
--   usually this is something like ">= 21"
--   but it could be be something like "age.isAdult" which has a boolean value
-- output columns can evaluate to any value type, typically "lentil soup" but sometimes a function [2..10]

-- proposal1 add a new type DMN_Enum
--         2 add a new type DMN_FEEL, which can be one of
                                      -- a feel expression like       age * 2
                                      -- a double quoted string like  "potato"
                                      -- a feel expression like       2
-- by default if the user doesn't explicitly type the column in the header,
     -- then the type is string
     -- but if they type it as   :FEEL    then it becomes a DMN_FEEL
     -- and then literal strings need to be double-quoted
     -- and any unquoted strings are interpreted as feel expressions
     -- and substrings like   Number Of Guests   get interpreted as variables.
     -- so you can do      Number Of Guests * Cost Per Head + 100
-- DMN's types are slightly different from ours
data DMNType = DMN_String         -- no need to double quote; we use this for enums too.
             | DMN_Number         -- a numeric type or a numeric comparison section
             | DMN_Boolean
             | DMN_List DMNType
             deriving (Show, Eq)

baseType :: Maybe DMNType -> Maybe DMNType
baseType Nothing = Just DMN_String
baseType (Just (DMN_List x)) = baseType (Just x)
baseType (Just x) = Just x

type DTvar = String

-- binary operators returning bool
data FBinOp = Flt | Flte               -- binary operators < <=
            | Fgt | Fgte               -- binary operators > >=
            | Feq                      -- binary operator  ==
             deriving (Show, Eq)

data FEELexp = FSection FBinOp DMNVal  --    > 2               FSection Fgt (VN Float)
             | FInRange Float Float    --    [2..4]            FInRange 2.0 4.0 -- though it should be Int Int!
             | FAnything               --    -                 FAnything
             | FNullary DMNVal         --    plain string      FNullary (VS "plain string")
             | FFunction FNumFunction  --    FEEL expression   age * 2 (meant for output)
             deriving (Show, Eq)
type SymbolTable = Map.Map String FEELexp

-- once we go higher-order we can do fun things like define ordered semilattices or whatever. for example:
-- http://matt.might.net/articles/partial-orders/
-- fEval (>50)   (100)    = True
-- fEval (>50) (< (>100)) = True

data DTCH_Label = DTCH_Comment
                | DTCH_In
                | DTCH_Out
                 deriving (Show, Eq)

data ColHeader = DTCH { label   :: DTCH_Label
                      , varname :: String
                      , vartype :: Maybe DMNType
                      , enums   :: Maybe [FEELexp] -- ordered list of domain elements seen in the column below; used by HP_OutputOrder
                      }
                 deriving (Show, Eq)

var_name :: ColHeader -> String
var_name = underscore . varname

underscore :: String -> String
underscore = replace " " "_"

data DecisionTable = DTable { tableName :: String
                             , hitpolicy :: HitPolicy
                             , header    :: [ColHeader]
                             , allrows   :: [DTrow]
                             }
               deriving (Show, Eq)

datarows :: DecisionTable -> [DTrow]
datarows = allrows

data DTrow = DTrow { row_number   :: Maybe Int
                   , row_inputs   :: [[FEELexp]] -- two-layer input and output to handle : | foo, bar | baz |
                   , row_outputs  :: [[FEELexp]] --                                        [[   ,    ]      ]
                   , row_comments :: [Maybe String] }
           deriving (Show, Eq)


data HeaderRow = DTHR { hrhp :: HitPolicy
                      , cols :: [ColHeader]
                      }
                 deriving (Show, Eq)

type CommentString = String

data ColBody = DTCBFeels [FEELexp] -- inputs and outputs are both FEELexps. lists, in fact, in hxt arrowlist tradition. so multivalues can propagate.
             | DTComment (Maybe CommentString)
                 deriving (Show, Eq)


-- in a decision table, an output cell might contain something like
-- max(0, age)  which is  FFunction ( FNFf FNFmax [ ... ] )
-- age      which becomes FFunction (       FNF1 "age"                            )
-- "age"    a string literal remains FNullary (FS "age")
-- age * 2  which becomes FFunction ( FNF3 (FNF1 "age")    FNMul (FNF0 (FN 2.0))  )
-- 2 * 4    which becomes FFunction ( FNF3 (FNF0 (FN 2.0)) FNMul (FNF0 (FN 4.0))  )
-- < 2      which becomes FSection FBinOp DMNVal
-- 2        which becomes FNullary (FN 2.0)

data FNumFunction = FNF0 DMNVal  -- terminal value
                  | FNF1 String  -- variable name
                  | FNF3 FNumFunction FNOp2 FNumFunction -- binary operator function
                  | FNFf FNFF [ FNumFunction ] -- function name, followed by argument list
             deriving (Show, Eq)

data FNFF = FNFmax | FNFmin
  deriving (Eq)

instance Show FNFF where
  show FNFmax     = "max"
  show FNFmin     = "min"

-- binary operators returning num
data FNOp2 = FNMul
           | FNDiv
           | FNPlus
           | FNMinus
           | FNExp
           deriving (Show, Eq)

-- Logical operators (ALF: I'm not sure how i feel about this, when it comes to generating the lambda syntax in FEELhelpers.hs) 
data FNLog = FNNot
           | FNAnd
           | FNOr
           deriving (Show, Eq)

-- Comparison Operators (ALF: I'm not sure how i feel about this, when it comes to generating the lambda syntax in FEELhelpers.hs) 
data FNComp = FNEq
            | FNNeq
            | FNLt
            | FNLeq
            | FNGt
            | FNGeq
            deriving (Show, Eq)

-- will need some pickle/unpickle infrastructure later
data DMNVal = VS String
            | VN Float
            | VB Bool
            deriving (Show, Eq)

-- for the max/min functionality
instance Ord DMNVal where
  compare (VS x) (VS y) = compare x y
  compare (VN x) (VN y) = compare x y
  compare (VB x) (VB y) = compare x y
  compare _ _           = error "can't compare different types"
