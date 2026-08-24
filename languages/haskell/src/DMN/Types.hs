{-# LANGUAGE OverloadedStrings #-}

{-| Types for DMNMD; the parser transforms input markdown tables into these types, which implement some subset of the DMN and FEEL standards -}

module DMN.Types where

-- definitions common to DecisionTable and DMNParseTable

import Prelude hiding (takeWhile)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Scientific (Scientific)

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
-- | The __element__ type of a column type: 'Just' for a collection, 'Nothing'
-- for a scalar. Ask @isListType@ if all you want is the yes/no.
--
-- This replaced @baseType@, which recursed to the innermost scalar and coerced
-- @Nothing@ to @Just DMN_String@ on the way. Neither behaviour is wanted here:
--
--  * a nested @[[T]]@ is __refused__, so flattening it would paper over the
--    refusal and hand a @[[Number]]@ column the cell layer of a @Number@ one;
--  * the @Nothing -> Just DMN_String@ coercion had no dependant. Its only
--    caller was @mkFEither@'s list arm, and @mkFEither Nothing@ already returns
--    @FNullary (VS (trim arg))@ by itself.
elemType :: Maybe DMNType -> Maybe DMNType
elemType (Just (DMN_List t)) = Just t
elemType _                   = Nothing

-- | Is this column type a collection?
isListType :: Maybe DMNType -> Bool
isListType = isJust . elemType

-- | Is this column a collection? The form almost every caller wants.
isListCol :: ColHeader -> Bool
isListCol = isListType . vartype

type DTvar = String

-- | binary operators returning bool
data FBinOp = Flt | Flte               -- binary operators < <=
            | Fgt | Fgte               -- binary operators > >=
            | Feq                      -- binary operator  ==
             deriving (Show, Eq)

-- | Which side of an interval endpoint is included.
data Bound = BClosed | BOpen
             deriving (Show, Eq)

data FEELexp = FSection FBinOp DMNVal  --    > 2               FSection Fgt (VN 2)
             | FInRange Bound Scientific Scientific Bound -- [2..4)  FInRange BClosed 2 4 BOpen
             | FAnything               --    -                 FAnything
             | FNullary DMNVal         --    plain string      FNullary (VS "plain string")
             | FFunction FNumFunction  --    FEEL expression   age * 2
             | FNot FEELexp            --    not([1..5])       FNot (FInRange BClosed 1 5 BClosed)
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
--
-- Was @Data.List.Utils.replace " " "_"@, and was the package's only use of
-- MissingH — which brought nine packages, three of them C-compiling, into the
-- library's closure for this one line. For a single-character needle @replace@
-- /is/ a pointwise map: it is @intercalate new . split old@, and splitting on
-- one character and rejoining reproduces every other character unchanged.
-- Checked exhaustively against MissingH-1.6.0.3's own source rather than
-- argued: all 3280 strings of length <= 7 over @{' ', '_', 'a'}@ agree, and
-- that alphabet is complete because no other character is distinguishable to
-- this function.
underscore :: String -> String
underscore = map (\c -> if c == ' ' then '_' else c)

-- | a decision table has a name, a hit policy, a set of column headers, and data rows beneath.
data DecisionTable = DTable { tableName :: String
                            , hitpolicy :: HitPolicy
                            , header    :: [ColHeader]
                            , allrows   :: [DTrow]
                            , dtDefaultOutput :: Maybe [[FEELexp]]
                              -- ^ DMN §8.2.11's default output value: what the table answers
                              -- when __no rule matches__. One cell per output column, aligned
                              -- with 'getOutputHeaders'; a column with no declared default
                              -- holds @[FAnything]@, the same spelling an output wildcard has.
                              -- 'Nothing' means the table says nothing about unmatched inputs.
                              --
                              -- Markdown has no syntax for this — its spelling of the same
                              -- intent is a trailing all-wildcard row, which the XML emitter
                              -- promotes into this slot (D-16 phase 2). It is populated by the
                              -- XML reader's @\<defaultOutputEntry\>@ and by that promotion,
                              -- and by nothing else.
                            }
               deriving (Show, Eq)

-- | get the data rows out of a decision table
datarows :: DecisionTable -> [DTrow]
datarows = allrows

-- | The table's rules plus, when it carries one, the §8.2.11 default output
-- value materialised as a trailing catch-all row — the markdown spelling of the
-- same statement.
--
-- For the first-match renderings the js\/ts\/py backends emit, "a row that
-- matches whatever nothing above matched" and "the value taken when no rule
-- matches" are the same function, so those backends consume this and need no
-- other knowledge of the field. 'DMN.DecisionTable.evalTable' deliberately does
-- __not__: under Any every matching row must agree and under Collect every
-- matching row contributes, so a materialised always-matching row would let the
-- default collide with (or pollute) real matches, which a default must never
-- do. The evaluator consults 'dtDefaultOutput' only after finding no match.
--
-- The synthetic row is numbered one past the row count, which is the number the
-- equivalent authored catch-all row carries in a 1..n table.
rowsPlusDefault :: DecisionTable -> [DTrow]
rowsPlusDefault dt = allrows dt ++ case dtDefaultOutput dt of
  Nothing -> []
  Just d  ->
    [ DTrow { row_number   = Just (length (allrows dt) + 1)
            , row_inputs   = [ [FAnything] | ch <- header dt, label ch == DTCH_In ]
            , row_outputs  = d
            , row_comments = [ Nothing | ch <- header dt, label ch == DTCH_Comment ]
            }
    ]

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
-- age * 2  which becomes FFunction ( FNF3 (FNF1 "age")    FNMul (FNF0 (VN 2))  )
--
-- 2 * 4    which becomes FFunction ( FNF3 (FNF0 (VN 2))   FNMul (FNF0 (VN 4))  )
--
-- < 2      which becomes FSection FBinOp DMNVal
--
-- 2        which becomes FNullary (VN 2)

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

-- | a DMN value is either a string, a number, or a bool.
-- For interop with other formats, we will need some pickle/unpickle infrastructure later.
data DMNVal = VS String
            -- | A number, held as an arbitrary-precision decimal.
            --
            -- __Not__ a 'Float'. FEEL's number is decimal128 (DMN 1.3
            -- §10.3.2.3.1), and binary32 could hold neither of the two things
            -- this tool is aimed at: @16777217@ became @16777216@ and
            -- @1234567.89@ became @1234567.9@, both at exit 0 with nothing on
            -- stderr. See @DECISIONS.md@ D-1, and 'DMN.Number' for everything
            -- that follows from the choice.
            --
            -- 'Scientific' records the scale the author wrote — @9@ is
            -- coefficient 9 exponent 0, @9.0@ is coefficient 90 exponent -1 —
            -- but its 'Eq' and 'Ord' compare on value, so the two are one
            -- number here and 'DMN.Number.showNumPlain' spells both @9@.
            | VN Scientific
            | VB Bool
            -- | A collection, and __only ever a runtime argument__.
            --
            -- A 'VL' cannot appear in a decision table cell:
            -- 'DMN.DecisionTable.mkFEither' has no way to build one, because a
            -- cell in a collection column is parsed at the ELEMENT type. It is
            -- produced solely by 'Main.mkInputValue', which parses a VALUE
            -- supplied at the prompt, as opposed to a TEST written in a table.
            --
            -- That distinction is the point. A cell is a test, a runtime
            -- argument is a value, and conflating them is why the @-q@ REPL
            -- used to accept @>= 5@ as an "input".
            | VL [DMNVal]
            deriving (Show, Eq)

