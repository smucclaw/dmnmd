
{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}

module ParserSpecHelpers (
    module ParserSpecHelpers
    , shouldParse

) where

import Test.Hspec
import Data.Text (Text)
import Text.Megaparsec hiding (label)
import qualified Test.Hspec.Megaparsec as HM
import Test.Hspec.Megaparsec (shouldParse)
import DMN.ParsingUtils
import Data.Void (Void)
import qualified Data.Text as T
import Text.RawString.QQ

-- * Helper functions

-- shouldParse :: Parser a -> a -> Expectation
-- shouldParse = undefined -- HM.shouldParse . parse ""

-- shouldParse = HM.shouldParse

(~>) :: Text -> Parser a -> Either (ParseErrorBundle Text Void) a
t ~> p = parse (p <* eof) "" t

shouldSucceedOn :: Show a => Parser a -> Text -> Expectation
shouldSucceedOn p = HM.shouldSucceedOn $ parse p ""

shouldFailOn :: Show a => Parser a -> Text -> Expectation
shouldFailOn p = HM.shouldFailOn $ parse p ""

throwOnLeft :: HasCallStack => Either String a -> a
throwOnLeft = either error id

dmn1 :: Text
dmn1 = T.pack $ dropWhile (=='\n') [r|
| U | Season | Dish                         | # Annotation  |
|---+--------+------------------------------+---------------|
| 1 | Fall   | Spareribs                    |               |
| 2 | Winter | Roastbeef                    |               |
| 3 | Spring | Steak                        |               |
| 4 | Summer | Light Salad and a nice Steak | Hey, why not? |
|]




-- with multivalues in the input
dmn1b :: Text
dmn1b = T.pack $ dropWhile (=='\n') [r|
| U | Season               | Dish                         | # Annotation  |
|---+----------------------+------------------------------+---------------|
| 1 | Fall                 | Spareribs                    |               |
| 2 | Winter               | Roastbeef                    |               |
| 3 | Spring, Summer       | Stew                         | Multivalue    |
|]

-- with type annotations multivalues in the input
dmn1c :: Text
dmn1c = T.pack $ dropWhile (=='\n') [r|
| U | Season : String      | Dish : String                | # Annotation  |
|---+----------------------+------------------------------+---------------|
| 1 | Fall                 | Spareribs                    |               |
| 2 | Winter               | Roastbeef, Strawberries      |               |
| 3 | Spring, Summer       | Stew                         | Multivalue    |
|]

dmn2 :: Text
dmn2 = T.pack $ dropWhile (=='\n') [r|
| U | Season : String      | guestCount : Number  | Dish : String                | # Annotation  |
|---+----------------------+----------------------+------------------------------+---------------|
| 1 | Fall                 | <= 8                 | Spareribs                    |               |
| 2 | Winter               | <= 8                 | Roastbeef                    |               |
| 3 | Spring               | <= 4                 | Dry Aged Gourmet Steak       |               |
| 4 | Spring               | [5..8]               | Steak                        |               |
| 5 | Fall, Winter, Spring | > 8                  | Stew                         |               |
| 6 | Summer               | -                    | Light Salad and a nice Steak | Hey, why not? |
|]

dmn3a :: Text
dmn3a = T.pack $ dropWhile (=='\n') [r|
| C | Age : Number | RiskCategory      | DebtReview : Boolean | Routing (out)          | Review_level (out)     | Reason (out)                |
|   |              | LOW, MEDIUM, HIGH |                      | DECLINE, REFER, ACCEPT | LEVEL 2, LEVEL 1, NONE |                             |
|---|--------------|-------------------|----------------------|------------------------|------------------------|-----------------------------|
| 1 |              |                   |                      | ACCEPT                 | NONE                   | Acceptable                  |
| 2 | <18          |                   |                      | DECLINE                | NONE                   | Applicant too young         |
| 3 |              | HIGH              |                      | REFER                  | LEVEL 1                | High risk application       |
| 4 |              |                   | True                 | REFER                  | LEVEL 2                | Applicant under debt review |
|]

dmn3b :: Text
dmn3b = T.pack $ dropWhile (=='\n') [r|
| O | Age : Number | RiskCategory      | DebtReview : Boolean | Routing (out)          | Review_level (out)     | Reason (out)        |
|   |              | LOW, MEDIUM, HIGH |                      | DECLINE, REFER, ACCEPT | LEVEL 2, LEVEL 1, NONE |                     |
|---+--------------+-------------------+----------------------+------------------------+------------------------+---------------------|
| 1 |              |                   |                      | ACCEPT                 | NONE                   | Acceptable          |
| 2 |          <18 |                   |                      | DECLINE                | NONE                   | Applicant too young |
| 3 |              | HIGH              |                      | REFER                  | LEVEL 1                | High risk           |
|   |              |                   |                      |                        |                        | application         |
| 4 |              |                   | True                 | REFER                  | LEVEL                  | Applicant under     |
|   |              |                   |                      |                        | 2                      | debt review         |
|]

dmn3count :: Text
dmn3count = T.pack $ dropWhile (=='\n') [r|
| C# | Age : Number | RiskCategory      | DebtReview : Boolean | Routing (out)          | Review_level (out)     | Reason (out)                |
|   |              | LOW, MEDIUM, HIGH |                      | DECLINE, REFER, ACCEPT | LEVEL 2, LEVEL 1, NONE |                             |
|---|--------------|-------------------|----------------------|------------------------|------------------------|-----------------------------|
| 1 |              |                   |                      | ACCEPT                 | NONE                   | Acceptable                  |
| 2 | <18          |                   |                      | DECLINE                | NONE                   | Applicant too young         |
| 3 |              | HIGH              |                      | REFER                  | LEVEL 1                | High risk application       |
| 4 |              |                   | True                 | REFER                  | LEVEL 2                | Applicant under debt review |
|]


dmn4sum :: Text
dmn4sum = T.pack $ dropWhile (=='\n') [r|
| C+ | Age : Number | SpiritOrbs : Number (out) | KorokSeeds : Number (out) |
|----+--------------+---------------------+---------------------|
|  1 | <18          |                   1 |                   2 |
|  2 | [18..21]     |                   3 |                   4 |
|  3 | >=18         |                   5 |                   6 |
|  4 | >=65         |                   7 |                   8 |
|]

dmn4count :: Text
dmn4count = T.pack $ dropWhile (=='\n') [r|
| C# | Age : Number | SpiritOrbs : Number (out) | KorokSeeds : Number (out) |
|----+--------------+---------------------+---------------------|
|  1 | <18          |                   1 |                   2 |
|  2 | [18..21]     |                   3 |                   4 |
|  3 | >=18         |                   5 |                   6 |
|  4 | >=65         |                   7 |                   8 |
|]

dmn4min :: Text
dmn4min = T.pack $ dropWhile (=='\n') [r|
| C< | Age : Number | SpiritOrbs : Number (out) | KorokSeeds : Number (out) |
|----+--------------+---------------------+---------------------|
|  1 | <18          |                   1 |                   2 |
|  2 | [18..21]     |                   3 |                   4 |
|  3 | >=18         |                   5 |                   6 |
|  4 | >=65         |                   7 |                   8 |
|]

dmn4max :: Text
dmn4max = T.pack $ dropWhile (=='\n') [r|
| C> | Age : Number | SpiritOrbs : Number (out) | KorokSeeds : Number (out) |
|----+--------------+---------------------+---------------------|
|  1 | <18          |                   1 |                   2 |
|  2 | [18..21]     |                   3 |                   4 |
|  3 | >=18         |                   5 |                   6 |
|  4 | >=65         |                   7 |                   8 |
|]

dmn5a :: Text
dmn5a = T.pack $ dropWhile (=='\n') [r|
| C> | Age  | SpiritOrbs (out) | KorokSeeds (out) |
|----+--------------+---------------------+---------------------|
|  1 | <18          |                   1 |                   true |
|  2 | [18..21]     |                   3 |                   false |
|  3 | >=18         |                   5 |                   yes |
|  4 | >=65         |                   7 |                   no |
|]

dmn5b :: Text
dmn5b = T.pack $ dropWhile (=='\n') [r|
| C> | Age      | SpiritOrbs (out) | KorokSeeds (out) |
|----+----------+------------------+------------------|
|  1 | <18      | one              | true             |
|  2 | [18..21] | three            | false            |
|  3 | >=18     | five             | yes              |
|  4 | >=65     | seven            | no               |
|]

dmn6a :: Text
dmn6a = T.pack $ dropWhile (=='\n') [r|
| F | age : Number | mayBuy : Boolean (out) | limit : Number (out) |
|---+--------------+------------------------+----------------------|
| 1 | <18          | False                  |                    0 |
| 2 | [18..21]     | True                   |                  750 |
| 3 | [21..25]     | True                   |                 1500 |
| 4 | >25          | True                   |            age * 100 |
|]
