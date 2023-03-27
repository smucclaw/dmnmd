{-# LANGUAGE OverloadedStrings #-}

{-| Types for DMNMD; the parser transforms input markdown tables into these types, which implement some subset of the DMN and FEEL standards -}

module DMN.Types where

-- definitions common to DecisionTable and DMNParseTable

import Prelude hiding (takeWhile)
import qualified Data.Map as Map
import Data.List.Utils (replace)

-- | We implement DMN Hit Policies.
data HitPolicy = HP_Unique
               | HP_Any
               | HP_Priority
               | HP_First
               | HP_OutputOrder
               | HP_RuleOrder
               | HP_Collect CollectOperator
               | HP_Aggregate
               deriving (Show, Eq)

-- | The Collect hit policy uses these.
data CollectOperator = Collect_Sum -- +
                     | Collect_Min -- <
                     | Collect_Max -- >
                     | Collect_Cnt -- #
                     | Collect_All --
               deriving (Show, Eq)

-- * INSIGHT
-- input columns always evaluate to a boolean somehow
--   usually this is something like ">= 21"
--   but it could be be something like "age.isAdult" which has a boolean value
-- output columns can evaluate to any value type, typically "lentil soup" but sometimes a function [2..10]
--
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

-- | DMN's types are slightly different from ours; these are the types that are given in input DMN headers
data DMNType = DMN_String         -- no need to double quote; we use this for enums too.
             | DMN_Number         -- a numeric type or a numeric comparison section
             | DMN_Boolean
             | DMN_List DMNType
             deriving (Show, Eq)
-- | what is the underlying base type? e.g. a list of @something@ has the base type @something@
baseType :: Maybe DMNType -> Maybe DMNType
baseType Nothing = Just DMN_String
baseType (Just (DMN_List x)) = baseType (Just x)
baseType (Just x) = Just x

type DTvar = String

-- | binary operators returning bool
data FBinOp = Flt | Flte               -- binary operators < <=
            | Fgt | Fgte               -- binary operators > >=
            | Feq                      -- binary operator  ==
             deriving (Show, Eq)

data FEELexp = FSection FBinOp DMNVal  --    > 2               FSection Fgt (VN Float)
             | FInRange Float Float    --    [2..4]            FInRange 2.0 4.0
             | FAnything               --    -                 FAnything
             | FNullary DMNVal         --    plain string      FNullary (VS "plain string")
             | FFunction FNumFunction  --    FEEL expression   age * 2
             deriving (Show, Eq)
type SymbolTable = Map.Map String FEELexp

-- once we go higher-order we can do fun things like define ordered semilattices or whatever. for example:
-- http://matt.might.net/articles/partial-orders/
-- fEval (>50)   (100)    = True
-- fEval (>50) (< (>100)) = True

-- | A column header is labeled as either an input column, and output column, or a comment.
data DTCH_Label = DTCH_Comment
                | DTCH_In
                | DTCH_Out
                 deriving (Show, Eq)

-- | A column header contains a label, a name of the column, an optional type, and sometimes an enum list.
data ColHeader = DTCH { label   :: DTCH_Label
                      , varname :: String
                      , vartype :: Maybe DMNType
                      , enums   :: Maybe [FEELexp] -- ordered list of domain elements seen in the column below; used by HP_OutputOrder
                      }
                 deriving (Show, Eq)

-- | utility function converts variable names to snake_case.
var_name :: ColHeader -> String
var_name = underscore . varname

-- | utility function replaces spaces with underscores.
underscore :: String -> String
underscore = replace " " "_"

-- | a decision table has a name, a hit policy, a set of column headers, and data rows beneath.
data DecisionTable = DTable { tableName :: String
                            , hitpolicy :: HitPolicy
                            , header    :: [ColHeader]
                            , allrows   :: [DTrow]
                            }
               deriving (Show, Eq)

-- | get the data rows out of a decision table
datarows :: DecisionTable -> [DTrow]
datarows = allrows

-- | a data row is numbered, and has input and output columns, also comment columns.
data DTrow = DTrow { row_number   :: Maybe Int
                   , row_inputs   :: [[FEELexp]] -- two-layer input and output to handle : | foo, bar | baz |
                   , row_outputs  :: [[FEELexp]] --                                        [[   ,    ]      ]
                   , row_comments :: [Maybe String] }
           deriving (Show, Eq)

-- | a header row contains the hit policy at the top left, then a list of column headers
data HeaderRow = DTHR { hrhp :: HitPolicy
                      , cols :: [ColHeader]
                      }
                 deriving (Show, Eq)

-- | comments are strings
type CommentString = String

-- | a cell contains either a list of FEEL expressions or a comment
data ColBody = DTCBFeels [FEELexp] -- inputs and outputs are both FEELexps. lists, in fact, in hxt arrowlist tradition. so multivalues can propagate.
             | DTComment (Maybe CommentString)
                 deriving (Show, Eq)

-- * Our FEEL Model
-- in a decision table, a cell might contain something like
--
-- age      which becomes FFunction (       FNF1 "age"                            )
--
-- age * 2  which becomes FFunction ( FNF3 (FNF1 "age")    FNMul (FNF0 (FN 2.0))  )
--
-- 2 * 4    which becomes FFunction ( FNF3 (FNF0 (FN 2.0)) FNMul (FNF0 (FN 4.0))  )
--
-- < 2      which becomes FSection FBinOp DMNVal
--
-- 2        which becomes FNullary (FN 2.0)

-- | a FEEL expression is either a terminal value, a variable name, or a function with a binary operator
data FNumFunction = FNF0 DMNVal  -- terminal value
                  | FNF1 String  -- variable name
                  | FNF3 FNumFunction FNOp2 FNumFunction -- binary operator function
             deriving (Show, Eq)

-- | binary operators returning num
data FNOp2 = FNMul
           | FNDiv
           | FNPlus
           | FNMinus
           | FNExp
           deriving (Show, Eq)

-- | Logical operators (ALF: I'm not sure how i feel about this, when it comes to generating the lambda syntax in FEELhelpers.hs) 
data FNLog = FNNot
           | FNAnd
           | FNOr
           deriving (Show, Eq)

-- | Comparison Operators (ALF: I'm not sure how i feel about this, when it comes to generating the lambda syntax in FEELhelpers.hs) 
data FNComp = FNEq
            | FNNeq
            | FNLt
            | FNLeq
            | FNGt
            | FNGeq
            deriving (Show, Eq)

-- | a DMN value is either a string, a float, or a bool.
-- For interop with other formats, we will need some pickle/unpickle infrastructure later.
data DMNVal = VS String
            | VN Float
            | VB Bool
            deriving (Show, Eq)

