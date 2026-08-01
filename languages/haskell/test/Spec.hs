{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, QuasiQuotes #-}

module Main where

import Control.Monad
import Text.RawString.QQ
import DMN.DecisionTable
import DMN.ParseCell (parseNumberCell)
import Data.Either (isLeft, isRight)
import DMN.Types
import DMN.ParseTable
import DMN.ParseFEEL
import Test.Hspec
-- import Test.Hspec.Attoparsec
-- import Data.Either (fromRight)
-- import Control.Applicative hiding (many, some)
-- import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec hiding (label)
import Text.Megaparsec.Char
import DMN.ParsingUtils

import DmnXmlSpec (xmlSpec)
import ParseFEELSpec (feelSpec)
import ParserSpecHelpers
import SFeelGrammar
import TranslateL4Spec (l4Spec)
import TranslateXMLSpec (xmlEmitSpec)

-- * Main content

main :: IO ()
main = do
  forM_ [spec1, spec2, spec3, xmlSpec, feelSpec, sfeelSpec, l4Spec, xmlEmitSpec, listSpec] $ hspec
  return ()

parseHelloWorld :: Parser ()
parseHelloWorld = do
  _ <- string "Hello World!"
  return ()

spec1 :: Spec
spec1 = do
  describe "parseHelloWorld" $ do
    it "should parse the phrase 'Hello World!'" $
      parseHelloWorld `shouldSucceedOn` ("Hello World!" :: Text)

parseHelloWorld2 :: Parser Text
parseHelloWorld2 = do
  result <- string "Hello World!"
  return result

spec2 :: Spec
spec2 = do
  describe "parseHelloWorld2" $ do 
    it "should parse the phrase 'Hello World!' and return 'Hello World!'" $
      ("Hello World!" :: Text) ~> parseHelloWorld2 `shouldParse` ("Hello World!" :: Text)

spec3 :: Spec
spec3 = do
  describe "parseVarname" $ do
    it "should parse a typical variable name"             $ ("varname" :: Text) ~> parseVarname `shouldParse` ("varname" :: Text)
    it "should parse a variable name with spaces"         $ ("var name" :: Text) ~> parseVarname `shouldParse` ("var name" :: Text)
    it "should fail on a non-variable name (digit first)" $ parseVarname `shouldFailOn` ("123varname" :: Text)
    it "should fail on a non-variable name (dash first)"  $ parseVarname `shouldFailOn` ("- Foovar_" :: Text)
    it "should fail on a blank string"                    $ parseVarname `shouldFailOn` ("" :: Text)
  describe "parseColHeader" $ do
    it "should parse just a column header"                $ ("varname" :: Text) ~> parseColHeader `shouldParse` (DTCH DTCH_In"varname" Nothing Nothing)
  describe "pipeSeparator" $ do
    it "should parse just a single pipe"                  $ (getpipeSeparator >> endOfInput) `shouldSucceedOn` ("|" :: Text)
    it "should not parse more than one pipe"              $ (getpipeSeparator >> endOfInput) `shouldFailOn`    ("||" :: Text)
  describe "getpipeSeparator" $ do
    it "should parse just a single pipe"                   $ ("|" :: Text) ~> getpipeSeparator `shouldParse` ("|" :: Text)
    it "should parse just a single pipe with r whitespace" $ ("|   " :: Text) ~> getpipeSeparator `shouldParse` ("|" :: Text)
    it "should parse just a single pipe with l whitespace" $ ("   |" :: Text) ~> getpipeSeparator `shouldParse` ("|" :: Text)
    it "should parse just a single pipe with 2 whitespace" $ ("  | " :: Text) ~> getpipeSeparator `shouldParse` ("|" :: Text)
    it "should not parse multiple pipes to end"            $ (getpipeSeparator *> endOfInput) `shouldFailOn` ("||" :: Text)
    it "should parse multiple pipes presumably leaving some unconsumed" $ (getpipeSeparator) `shouldSucceedOn` ("||" :: Text)
    -- it "should leave the text unconsumed"                  $ ("|abc" :: Text) ~?> getpipeSeparator `leavesUnconsumed` ("abc" :: Text)
  describe "parseHeaderRow" $ do
    it "should parse a pipe"                                 $ pipeSeparator `shouldSucceedOn` ("|"::Text)
    it "should parse many pipes"                             $ (many pipeSeparator <* endOfInput) `shouldSucceedOn` ("| |||   |  |  "::Text)
    it "should parse many pipes but not something with text" $ (many pipeSeparator <* endOfInput) `shouldFailOn`    ("| | text |"::Text)
    it "should parse a no-column header row"  $ ("| U |"                       :: Text) ~> parseHeaderRow `shouldParse` (DTHR HP_Unique [])
    it "should parse a one-column header row" $ ("| U | varname1 |"            :: Text) ~> parseHeaderRow `shouldParse` (DTHR HP_Unique [DTCH DTCH_In "varname1" Nothing Nothing])
    it "should parse a two-column header row" $ ("| U | varname1 | varname2 |" :: Text) ~> parseHeaderRow `shouldParse` (DTHR HP_Unique [DTCH DTCH_In "varname1" Nothing Nothing
                                                                                                                                        ,DTCH DTCH_In "varname2" Nothing Nothing])

  describe "parseSubHeadRow" $ do
    it "should parse a continuation row" $
      ("|  | LOW, MEDIUM, HIGH |                      | DECLINE, REFER, ACCEPT | LEVEL 2, LEVEL 1, NONE |                             |\n" :: Text) ~>
      parseContinuationRow `shouldParse` (["LOW, MEDIUM, HIGH", "", "DECLINE, REFER, ACCEPT", "LEVEL 2, LEVEL 1, NONE", ""])

  describe "parseSubHeadRows" $ do
    it "should parse multiple continuation rows, unwrapping along the way" $
      (("|  | LOW, MEDIUM,      |                      | DECLINE,      | LEVEL 2, LEVEL |                             |\n" :: Text) <>
       ("|  |              HIGH |                      | REFER, ACCEPT | 1, NONE        |                             |\n" :: Text)) ~>
      parseContinuationRows `shouldParse` (["LOW, MEDIUM, HIGH", "", "DECLINE, REFER, ACCEPT", "LEVEL 2, LEVEL 1, NONE", ""])

  describe "parse header and subhead together" $ do
    it "should parse an entire header ignoring dashed lines" $
      (("| O | Age | RiskCategory (out) | DebtReview : Boolean (out)|\n" :: Text) <>
       ("|----------------------------------------------------------|\n" :: Text) <>
       ("|   |     | LOW, MEDIUM,       | true                      |\n" :: Text) <>
       ("|----------------------------------------------------------|\n" :: Text) <>
       ("|   |     | HIGH               |                           |\n" :: Text))
      ~> (do
        hr <- parseHeaderRow
        sh <- parseContinuationRows
        return (hr, sh)
      ) `shouldParse` ( (DTHR HP_OutputOrder [ DTCH DTCH_In "Age" Nothing Nothing
                                             , DTCH DTCH_Out "RiskCategory" Nothing Nothing
                                             , DTCH DTCH_Out "DebtReview" (Just DMN_Boolean) Nothing])
                      , [ "", "LOW, MEDIUM, HIGH", "true" ] )

  describe "parse header and subhead and a data row together" $ do
    it "should parse an entire header, the ---- line, and a logical data row below split over two physical rows, then a data row" $
      (("| O | Age : Number | RiskCategory (out) | DebtReview : Boolean (out)|\n" :: Text) <>
       ("|   |              | LOW, MEDIUM,       | true                      |\n" :: Text) <>
       ("|   |              | HIGH               |                           |\n" :: Text) <>
       ("|-------------------------------------------------------------------|\n" :: Text) <>
       ("| 1 | <            | HIGH               |                           |\n" :: Text) <>
       ("|   |   18         |                    | false                     |\n" :: Text) <>
       ("| 2 | >= 21        | LOW                | yes                       |\n" :: Text))
      ~> (do
        hr <- parseHeaderRow
        sh <- parseContinuationRows <?> "parseContinuationRows"
        let columnSignatures = columnSigs hr
        dr <- parseDataRows "mytable1" columnSignatures <?> "parseDataRows"
        return (hr, sh, dr)
      ) `shouldParse` ( (DTHR HP_OutputOrder [ DTCH DTCH_In "Age"           (Just DMN_Number) Nothing
                                             , DTCH DTCH_Out "RiskCategory"  Nothing Nothing
                                             , DTCH DTCH_Out "DebtReview"   (Just DMN_Boolean) Nothing])
                      , [ "", "LOW, MEDIUM, HIGH", "true" ]
                      , [ DTrow (Just 1) [ mkFs (Just DMN_Number) "<18"  ] [ [ FNullary $ VS "HIGH" ] , [ FNullary $ VB False ] ] []
                        , DTrow (Just 2) [ mkFs (Just DMN_Number) ">=21" ] [ [ FNullary $ VS "LOW" ]  , [ FNullary $ VB True  ] ] [] 
                        ])

  describe "parseHitPolicy" $ do
    it "should parse a Unique hit policy"        $ ("U"  :: Text) ~> parseHitPolicy `shouldParse` HP_Unique
    it "should parse an Any hit policy"          $ ("A"  :: Text) ~> parseHitPolicy `shouldParse` HP_Any
    it "should parse a Collect All hit policy"   $ ("C"  :: Text) ~> parseHitPolicy `shouldParse` HP_Collect Collect_All
    it "should parse a Collect All hit policy"   $ ("CA" :: Text) ~> parseHitPolicy `shouldParse` HP_Collect Collect_All
    it "should parse a Collect Sum hit policy"   $ ("C+" :: Text) ~> parseHitPolicy `shouldParse` HP_Collect Collect_Sum
    it "should parse a Collect Count hit policy" $ ("C#" :: Text) ~> parseHitPolicy `shouldParse` HP_Collect Collect_Cnt

  describe "parseTypeDecl" $ do
    it "should parse a String type annotation"  $ (": String"  :: Text) ~> parseTypeDecl `shouldParse` (Just DMN_String)
    it "should parse a Number type annotation"  $ (": Number"  :: Text) ~> parseTypeDecl `shouldParse` (Just DMN_Number)
    it "should parse a Boolean type annotation" $ (": Boolean" :: Text) ~> parseTypeDecl `shouldParse` (Just DMN_Boolean)
    it "should parse a List of Strings"         $ (": [String]" :: Text) ~> parseTypeDecl `shouldParse` (Just (DMN_List DMN_String))

  describe "wtf" $ do
    it "testing many1 digit"
      $ ("| 123 |" :: Text) ~> (
      do
        pipeSeparator
        mymap <- mapM (\x -> do
                          myjust <- Just . (\d -> x * (read d) :: Int) <$> many1 digit
                          pipeSeparator
                          return myjust
                      ) [2]
        return mymap
      ) `shouldParse` ([Just 246])

    it "testing manual recreation of parseDataRow"
      $ ("| 1 |\n" :: Text) ~> (
      do
        pipeSeparator
        myrownumber <-  many1 digit
        pipeSeparator
        endOfLine
        return (DTrow (Just . (\n -> (read n) :: Int) $ myrownumber) [] [] [])
      ) `shouldParse` (DTrow (Just 1) [] [] [])
      
  -- These four call parseDataRow with a hand-built signature list and no header
  -- row, so the table name and the ColSig column names are invented. They are
  -- read only by DMN.DecisionTable.CellSite when a cell is REFUSED, which none
  -- of these rows does; nothing here asserts on them.
  describe "parseDataRow" $ do
    it "should parse a zero-column row"
      $ ("| 1 |\n" :: Text ) ~> (parseDataRow "mytable1" []) `shouldParse` (DTrow (Just 1) [] [] [])
    it "should parse a comment-only row"
      $ ("| 1 | rem |\n" :: Text ) ~> (parseDataRow "mytable1" [ColSig DTCH_Comment "note" Nothing]) `shouldParse` (DTrow (Just 1) [] [] [Just "rem"])
    it "should parse an input-only row"
      $ ("| 1 | potato |\n" :: Text ) ~> (parseDataRow "mytable1" [ColSig DTCH_In "veg" Nothing]) `shouldParse` (DTrow (Just 1) [[FNullary $ VS "potato"]] [] [])
    it "should parse an output-only row"
      $ ("| 1 | potato |\n" :: Text ) ~> (parseDataRow "mytable1" [ColSig DTCH_Out "veg" Nothing]) `shouldParse` (DTrow (Just 1) [] [[FNullary $ VS "potato"]] [])

  describe "parseTable" $ do
    it "should parse a null table with no header columns and no body rows"
      $ ("| U |\n" :: Text )
      ~> (parseTable "mytable1") `shouldParse` (DTable "mytable1" HP_Unique [] [])
    it "should parse a boring table with one header column and no body rows"
      $ ("| U | varname1 |\n" :: Text )
      ~> (parseTable "mytable1") `shouldParse` (DTable "mytable1" HP_Unique [DTCH DTCH_In "varname1" Nothing Nothing] [])
    it "should parse a boring table with one in header, one explicit out, and no body rows"
      $ ("| U | varname1 | varname2 (out) |\n" :: Text )
      ~> (parseTable "mytable1") `shouldParse` (DTable "mytable1" HP_Unique [DTCH DTCH_In "varname1" Nothing Nothing, DTCH DTCH_Out "varname2" Nothing Nothing] [])
    it "should parse a boring table with two in headers, which should autoswitch to out"
      $ ("| U | varname1 | varname2 |\n" :: Text )
      ~> (parseTable "mytable1") `shouldParse` (DTable "mytable1" HP_Unique [DTCH DTCH_In "varname1" Nothing Nothing, DTCH DTCH_Out "varname2" Nothing Nothing] [])
    it "should parse a boring table with one explicit in headers, and the other which should autoswitch to out"
      $ ("| U | varname1 (in) | varname2 |\n" :: Text )
      ~> (parseTable "mytable1") `shouldParse` (DTable "mytable1" HP_Unique [DTCH DTCH_In "varname1" Nothing Nothing, DTCH DTCH_Out "varname2" Nothing Nothing] [])
    it "should parse a boring table with one header column, one comment column, and no body rows"
      $ ("| U | varname1 | # rem |\n" :: Text )
      ~> (parseTable "mytable1") `shouldParse` (DTable "mytable1" HP_Unique [DTCH DTCH_In "varname1" Nothing Nothing, DTCH DTCH_Comment "rem" Nothing Nothing] [])
    it "should parse a boring table with one header column, one comment column, and no body rows"
      $ ("| U | varname1 | rem (comment) |\n" :: Text )
      ~> (parseTable "mytable1") `shouldParse` (DTable "mytable1" HP_Unique [DTCH DTCH_In "varname1" Nothing Nothing, DTCH DTCH_Comment "rem" Nothing Nothing] [])
    it "should parse a boring table with one header column, one comment column, and one body row"
      $ ("| U | varname1 | rem (comment) |\n| 1 | foo | mycomment |\n" :: Text )
      ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" HP_Unique
        [DTCH DTCH_In "varname1" (Just DMN_String) Nothing, DTCH DTCH_Comment "rem" Nothing Nothing]
        [DTrow (Just 1) [mkFs (Just DMN_String) "foo"] [] [Just "mycomment"]])
    it "should parse the standard dmn example 1"
      $ dmn1
      ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" HP_Unique
        [DTCH DTCH_In "Season" (Just DMN_String) Nothing, DTCH DTCH_Out "Dish" (Just DMN_String) Nothing, DTCH DTCH_Comment "Annotation" Nothing Nothing]
        [DTrow (Just 1) [mkFs Nothing "Fall"]   [mkFs Nothing "Spareribs"] [Nothing]
        ,DTrow (Just 2) [mkFs Nothing "Winter"] [mkFs Nothing "Roastbeef"] [Nothing]
        ,DTrow (Just 3) [mkFs Nothing "Spring"] [mkFs Nothing "Steak"    ] [Nothing]
        ,DTrow (Just 4) [mkFs Nothing "Summer"] [mkFs Nothing "Light Salad and a nice Steak"] [Just "Hey, why not?"]
        ])

    it "should parse the standard dmn example 1b with comma expressions"
      $ dmn1b
      ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" HP_Unique
        [DTCH DTCH_In "Season" (Just DMN_String) Nothing, DTCH DTCH_Out "Dish" (Just DMN_String) Nothing, DTCH DTCH_Comment "Annotation" Nothing Nothing]
        [DTrow (Just 1) [mkFs Nothing "Fall"]   [mkFs Nothing "Spareribs"] [Nothing]
        ,DTrow (Just 2) [mkFs Nothing "Winter"] [mkFs Nothing "Roastbeef"] [Nothing]
        ,DTrow (Just 3) [[FNullary $ VS "Spring", FNullary $ VS "Summer"]] [mkFs (Just DMN_String) "Stew"    ] [Just "Multivalue"]
        ])

    it "should parse the standard dmn example 1c with type annotations and multivalues"
      $ dmn1c
      ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" HP_Unique
        [DTCH DTCH_In "Season" (Just DMN_String) Nothing, DTCH DTCH_Out "Dish" (Just DMN_String) Nothing, DTCH DTCH_Comment "Annotation" Nothing Nothing]
        [DTrow (Just 1) [mkFs (Just DMN_String) "Fall"]   [mkFs (Just DMN_String) "Spareribs"] [Nothing]
        ,DTrow (Just 2) [mkFs (Just DMN_String) "Winter"] [[FNullary $ VS "Roastbeef", FNullary $ VS "Strawberries"]] [Nothing]
        ,DTrow (Just 3) [[FNullary $ VS "Spring", FNullary $ VS "Summer"]] [[FNullary $ VS "Stew"    ]] [Just "Multivalue"]
        ])

    it "should parse the standard dmn example 2 with multiple columns and numeric comparisons"
      $ dmn2
      ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" HP_Unique
        [DTCH DTCH_In "Season" (Just DMN_String) Nothing, DTCH DTCH_In "guestCount" (Just DMN_Number) Nothing, DTCH DTCH_Out "Dish" (Just DMN_String) Nothing, DTCH DTCH_Comment "Annotation" Nothing Nothing]
        [DTrow (Just 1) [mkFs (Just DMN_String) "Fall",   mkFs (Just DMN_Number) "<= 8"]    [mkFs (Just DMN_String) "Spareribs"] [Nothing]
        ,DTrow (Just 2) [mkFs (Just DMN_String) "Winter", mkFs (Just DMN_Number) "<= 8"]   [[FNullary $ VS "Roastbeef"]] [Nothing]
        ,DTrow (Just 3) [mkFs (Just DMN_String) "Spring", mkFs (Just DMN_Number) "<= 4"]   [[FNullary $ VS "Dry Aged Gourmet Steak"]] [Nothing]
        ,DTrow (Just 4) [mkFs (Just DMN_String) "Spring", mkFs (Just DMN_Number) "[5..8]"] [[FNullary $ VS "Steak"]] [Nothing]
        ,DTrow (Just 5) [mkFs (Just DMN_String) "Fall, Winter, Spring", mkFs (Just DMN_Number) "> 8"] [[FNullary $ VS "Stew"]] [Nothing]
        ,DTrow (Just 6) [[FNullary $ VS "Summer"], [FAnything]] [mkFs (Just DMN_String) "Light Salad and a nice Steak"] [Just "Hey, why not?"]
        ])

    it "should parse the Collect table with a subheader row"
      $ dmn3a
      ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" (HP_Collect Collect_All)
        [ DTCH DTCH_In "Age"            (Just DMN_Number) Nothing
        , DTCH DTCH_In "RiskCategory"  (Just DMN_String) (Just $ FNullary . VS <$> words "LOW MEDIUM HIGH")
        , DTCH DTCH_In "DebtReview"    (Just DMN_Boolean) Nothing
        , DTCH DTCH_Out "Routing"       (Just DMN_String) (Just $ FNullary . VS <$> words "DECLINE REFER ACCEPT")
        , DTCH DTCH_Out "Review_level"  (Just DMN_String) (Just $ FNullary . VS <$> ["LEVEL 2", "LEVEL 1", "NONE"])
        , DTCH DTCH_Out "Reason"        (Just DMN_String) Nothing
        ]
        [DTrow (Just 1) [[FAnything], [FAnything], [FAnything]]                   [mkFs (Just DMN_String) "ACCEPT",  mkFs (Just DMN_String) "NONE", mkFs (Just DMN_String) "Acceptable"]              []
        ,DTrow (Just 2) [mkFs (Just DMN_Number) "<18", [FAnything], [FAnything]]  [mkFs (Just DMN_String) "DECLINE",  mkFs (Just DMN_String) "NONE", mkFs (Just DMN_String) "Applicant too young"]    []
        ,DTrow (Just 3) [[FAnything], mkFs (Just DMN_String) "HIGH", [FAnything]] [mkFs (Just DMN_String) "REFER",  mkFs (Just DMN_String) "LEVEL 1", mkFs (Just DMN_String) "High risk application"] []
        ,DTrow (Just 4) [[FAnything], [FAnything], [FNullary (VB True)]]          [mkFs (Just DMN_String) "REFER",  mkFs (Just DMN_String) "LEVEL 2", mkFs (Just DMN_String) "Applicant under debt review"]   []
        ])

    it "should parse the Output Order table with a subheader and continuation body rows"
      $ dmn3b
      ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" HP_OutputOrder
        [ DTCH DTCH_In "Age"            (Just DMN_Number) Nothing
        , DTCH DTCH_In "RiskCategory"  (Just DMN_String) (Just $ FNullary . VS <$> words "LOW MEDIUM HIGH")
        , DTCH DTCH_In "DebtReview"    (Just DMN_Boolean) Nothing
        , DTCH DTCH_Out "Routing"       (Just DMN_String) (Just $ FNullary . VS <$> words "DECLINE REFER ACCEPT")
        , DTCH DTCH_Out "Review_level"  (Just DMN_String) (Just $ FNullary . VS <$> ["LEVEL 2", "LEVEL 1", "NONE"])
        , DTCH DTCH_Out "Reason"        (Just DMN_String) Nothing
        ]
        [DTrow (Just 1) [[FAnything], [FAnything], [FAnything]]                   [mkFs (Just DMN_String) "ACCEPT",  mkFs (Just DMN_String) "NONE", mkFs (Just DMN_String) "Acceptable"]              []
        ,DTrow (Just 2) [mkFs (Just DMN_Number) "<18", [FAnything], [FAnything]]  [mkFs (Just DMN_String) "DECLINE",  mkFs (Just DMN_String) "NONE", mkFs (Just DMN_String) "Applicant too young"]    []
        ,DTrow (Just 3) [[FAnything], mkFs (Just DMN_String) "HIGH", [FAnything]] [mkFs (Just DMN_String) "REFER",  mkFs (Just DMN_String) "LEVEL 1", mkFs (Just DMN_String) "High risk application"] []
        ,DTrow (Just 4) [[FAnything], [FAnything], [FNullary (VB True)]]          [mkFs (Just DMN_String) "REFER",  mkFs (Just DMN_String) "LEVEL 2", mkFs (Just DMN_String) "Applicant under debt review"]   []
        ])

  describe "evalTable dmn1" $ do
    it "should run standard dmn example 1: Fall -> Spareribs"
      $ (evalTable (throwOnLeft (parseOnly (parseTable "mytable1") dmn1)) (mkFs (Just DMN_String) "Fall")) `shouldBe` Right [[[FNullary $ VS "Spareribs"]]]
    it "should run standard dmn example 1: Winter -> Roastbeef"
      $ (evalTable (throwOnLeft (parseOnly (parseTable "mytable1") dmn1)) (mkFs (Just DMN_String) "Fall")) `shouldBe` Right [[[FNullary $ VS "Spareribs"]]]
  
  describe "evalTable dmn1c" $ do
    it "should run standard dmn example 1c: Fall -> Spareribs"
      $ (evalTable (throwOnLeft (parseOnly (parseTable "mytable1") dmn1c)) (mkFs (Just DMN_String) "Fall")) `shouldBe` Right [[[FNullary $ VS "Spareribs"]]]
    it "should run standard dmn example 1c: Winter -> [Roastbeef, Strawberries]"
      $ (evalTable (throwOnLeft (parseOnly (parseTable "mytable1") dmn1c)) (mkFs (Just DMN_String) "Winter")) `shouldBe` Right [[[FNullary $ VS "Roastbeef", FNullary $ VS "Strawberries"]]]
    it "should run standard dmn example 1c: Spring -> Stew"
      $ (evalTable (throwOnLeft (parseOnly (parseTable "mytable1") dmn1c)) ([FNullary $ VS "Spring"])) `shouldBe` Right [[[FNullary $ VS "Stew"]]]
    it "should run standard dmn example 1c: Summer -> Stew"
      $ (evalTable (throwOnLeft (parseOnly (parseTable "mytable1") dmn1c)) ([FNullary $ VS "Summer"])) `shouldBe` Right [[[FNullary $ VS "Stew"]]]
    it "should run standard dmn example 1c: Never -> Left \"no match\""
      $ (evalTable (throwOnLeft (parseOnly (parseTable "mytable1") dmn1c)) ([FNullary $ VS "Never"])) `shouldBe` Left "no rows returned -- a unique table should have one result!"

  -- Hit policy A (D-5). Before this block the suite's ENTIRE coverage of HP_Any
  -- was one `parseHitPolicy` assertion, so `cabal test` stayed green with the arm
  -- returning Left for every input, and stayed green again with the guard fixed
  -- but the success branch still list-valued. Both halves are asserted here.
  describe "evalTable hit policy A" $ do
    let anyAgree    = throwOnLeft (parseOnly (parseTable "AnyAgree")    dmnAnyAgree)
        anyDisagree = throwOnLeft (parseOnly (parseTable "AnyDisagree") dmnAnyDisagree)
    it "two rows match and agree: returns the shared output ONCE, not once per matching row"
      $ evalTable anyAgree [FNullary (VN 5)]  `shouldBe` Right [[[FNullary (VS "ok")]]]
    it "one row matches: returns its output"
      $ evalTable anyAgree [FNullary (VN 25)] `shouldBe` Right [[[FNullary (VS "ok")]]]
    it "no row matches: no rows returned"
      $ evalTable anyAgree [FNullary (VN 99)] `shouldBe` Left "no rows returned"
    it "two rows match and disagree: refused, because ANY permits multiple matches but not multiple answers"
      -- only the first line: the rest is a `show` of the matched rows, which is a
      -- debugging dump rather than a promise.
      $ (case evalTable anyDisagree [FNullary (VN 5)] of
           Left e  -> head (lines e)
           Right r -> "unexpectedly returned " ++ show r)
        `shouldBe` "multiple distinct rows returned -- an Any lookup may return multiple matches but they should all be the same!"
    it "one row matches in a disagreeing table: still answers, because only the matched rows must agree"
      $ evalTable anyDisagree [FNullary (VN 25)] `shouldBe` Right [[[FNullary (VS "deny")]]]

  describe "evalTable dmn2" $ do
    let evaled2 = (throwOnLeft (parseOnly (parseTable "mytable1") dmn2))
    it "should handle multiple inputs: Fall, 5"   $ evalTable evaled2 [FNullary (VS "Fall"),   FNullary (VN 5)] `shouldBe` Right [[[FNullary $ VS "Spareribs"]]]
    it "should handle multiple inputs: Fall, 9"   $ evalTable evaled2 [FNullary (VS "Fall"),   FNullary (VN 9)] `shouldBe` Right [[[FNullary $ VS "Stew"]]]
    it "should handle multiple inputs: Winter, 5" $ evalTable evaled2 [FNullary (VS "Winter"), FNullary (VN 5)] `shouldBe` Right [[[FNullary $ VS "Roastbeef"]]]
    it "should handle multiple inputs: Winter, 9" $ evalTable evaled2 [FNullary (VS "Winter"), FNullary (VN 9)] `shouldBe` Right [[[FNullary $ VS "Stew"]]]
    it "should handle multiple inputs: Spring, 3" $ evalTable evaled2 [FNullary (VS "Spring"), FNullary (VN 3)] `shouldBe` Right [[[FNullary $ VS "Dry Aged Gourmet Steak"]]]
    it "should handle multiple inputs: Spring, 9" $ evalTable evaled2 [FNullary (VS "Spring"), FNullary (VN 9)] `shouldBe` Right [[[FNullary $ VS "Stew"]]]
    it "should handle multiple inputs: Summer, 3" $ evalTable evaled2 [FNullary (VS "Summer"), FNullary (VN 3)] `shouldBe` Right [[[FNullary $ VS "Light Salad and a nice Steak"]]]
    it "should handle multiple inputs: Summer, 9" $ evalTable evaled2 [FNullary (VS "Summer"), FNullary (VN 9)] `shouldBe` Right [[[FNullary $ VS "Light Salad and a nice Steak"]]]

  describe "evalTable dmn3a - collect" $ do
    let evaled3 = (throwOnLeft (parseOnly (parseTable "mytable1") dmn3a))
    it "should return all rows: 17, HIGH, True"   $ evalTable evaled3 [FNullary (VN 17.0),   FNullary (VS "HIGH"), FNullary (VB True)] `shouldBe`
      Right [ [[FNullary $ VS "ACCEPT"],  [FNullary $ VS "NONE"],    [FNullary $ VS "Acceptable"]]
            , [[FNullary $ VS "DECLINE"], [FNullary $ VS "NONE"],    [FNullary $ VS "Applicant too young"]]
            , [[FNullary $ VS "REFER"],   [FNullary $ VS "LEVEL 1"], [FNullary $ VS "High risk application"]]
            , [[FNullary $ VS "REFER"],   [FNullary $ VS "LEVEL 2"], [FNullary $ VS "Applicant under debt review"]]
            ]

  describe "evalTable dmn3count - collect count" $ do
    it "should parse table 3c as a Collect Count"   $ dmn3count ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" (HP_Collect Collect_Cnt)
        [ DTCH DTCH_In "Age"            (Just DMN_Number) Nothing
        , DTCH DTCH_In "RiskCategory"  (Just DMN_String) (Just $ FNullary . VS <$> words "LOW MEDIUM HIGH")
        , DTCH DTCH_In "DebtReview"    (Just DMN_Boolean) Nothing
        , DTCH DTCH_Out "Routing"       (Just DMN_String) (Just $ FNullary . VS <$> words "DECLINE REFER ACCEPT")
        , DTCH DTCH_Out "Review_level"  (Just DMN_String) (Just $ FNullary . VS <$> ["LEVEL 2", "LEVEL 1", "NONE"])
        , DTCH DTCH_Out "Reason"        (Just DMN_String) Nothing
        ]
        [DTrow (Just 1) [[FAnything], [FAnything], [FAnything]]                   [mkFs (Just DMN_String) "ACCEPT",  mkFs (Just DMN_String) "NONE", mkFs (Just DMN_String) "Acceptable"]              []
        ,DTrow (Just 2) [mkFs (Just DMN_Number) "<18", [FAnything], [FAnything]]  [mkFs (Just DMN_String) "DECLINE",  mkFs (Just DMN_String) "NONE", mkFs (Just DMN_String) "Applicant too young"]    []
        ,DTrow (Just 3) [[FAnything], mkFs (Just DMN_String) "HIGH", [FAnything]] [mkFs (Just DMN_String) "REFER",  mkFs (Just DMN_String) "LEVEL 1", mkFs (Just DMN_String) "High risk application"] []
        ,DTrow (Just 4) [[FAnything], [FAnything], [FNullary (VB True)]]          [mkFs (Just DMN_String) "REFER",  mkFs (Just DMN_String) "LEVEL 2", mkFs (Just DMN_String) "Applicant under debt review"]   []
        ])
    let evaled3 = (throwOnLeft (parseOnly (parseTable "mytable1") dmn3count))
    it "17 should return a count of 4"   $ evalTable evaled3 [FNullary (VN 17.0),   FNullary (VS "HIGH"), FNullary (VB True)] `shouldBe` Right [ [[FNullary $ VN 4.0 ]] ]
    it "18 should return a count of 3"   $ evalTable evaled3 [FNullary (VN 18.0),   FNullary (VS "HIGH"), FNullary (VB True)] `shouldBe` Right [ [[FNullary $ VN 3.0 ]] ]




-- So, for example, if called with Age = 17, Risk category = “HIGH” and Debt review = true, the Routing rules table in Figure 8.19 would return the outputs of all four rules, in the order 2, 4, 3, 1.
-- p 96
  describe "evalTable dmn3 - ordered output" $ do
    let evaled3 = (throwOnLeft (parseOnly (parseTable "mytable1") dmn3b))
    it "should return all rows in order of specification in the subheader"   $ evalTable evaled3 [FNullary (VN 17.0),   FNullary (VS "HIGH"), FNullary (VB True)] `shouldBe`
      Right [ [[FNullary $ VS "DECLINE"], [FNullary $ VS "NONE"],    [FNullary $ VS "Applicant too young"]]
            , [[FNullary $ VS "REFER"],   [FNullary $ VS "LEVEL 2"], [FNullary $ VS "Applicant under debt review"]]
            , [[FNullary $ VS "REFER"],   [FNullary $ VS "LEVEL 1"], [FNullary $ VS "High risk application"]]
            , [[FNullary $ VS "ACCEPT"],  [FNullary $ VS "NONE"],    [FNullary $ VS "Acceptable"]]
            ]

  describe "evalTable dmn4sum - collect sum" $ do
    it "should parse the Zelda Collect Sum table"
      $ dmn4sum
      ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" (HP_Collect Collect_Sum)
        [ DTCH DTCH_In "Age"            (Just DMN_Number) Nothing
        , DTCH DTCH_Out "SpiritOrbs"     (Just DMN_Number) Nothing
        , DTCH DTCH_Out "KorokSeeds"     (Just DMN_Number) Nothing
        ]
        [DTrow (Just 1) [mkFs (Just DMN_Number) "<18"]       [mkFs (Just DMN_Number) "1",  mkFs (Just DMN_Number) "2"]    []
        ,DTrow (Just 2) [mkFs (Just DMN_Number) "[18..21]"]  [mkFs (Just DMN_Number) "3",  mkFs (Just DMN_Number) "4"]    []
        ,DTrow (Just 3) [mkFs (Just DMN_Number) ">=18"]      [mkFs (Just DMN_Number) "5",  mkFs (Just DMN_Number) "6"]    []
        ,DTrow (Just 4) [mkFs (Just DMN_Number) ">=65"]      [mkFs (Just DMN_Number) "7",  mkFs (Just DMN_Number) "8"]    []
        ])
    let evaled3 = (throwOnLeft (parseOnly (parseTable "mytable1") dmn4sum))
    it "17 should return 3 magical objects"    $ evalTable evaled3 [FNullary (VN 17.0)] `shouldBe` Right [ [[FNullary $ VN  3.0 ]] ]
    it "18 should return 18 magical objects"   $ evalTable evaled3 [FNullary (VN 18.0)] `shouldBe` Right [ [[FNullary $ VN 18.0 ]] ]

  describe "evalTable dmn4d - collect count" $ do
    it "should parse the Zelda Collect Count table"
      $ dmn4count
      ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" (HP_Collect Collect_Cnt)
        [ DTCH DTCH_In "Age"            (Just DMN_Number) Nothing
        , DTCH DTCH_Out "SpiritOrbs"     (Just DMN_Number) Nothing
        , DTCH DTCH_Out "KorokSeeds"     (Just DMN_Number) Nothing
        ]
        [DTrow (Just 1) [mkFs (Just DMN_Number) "<18"]       [mkFs (Just DMN_Number) "1",  mkFs (Just DMN_Number) "2"]    []
        ,DTrow (Just 2) [mkFs (Just DMN_Number) "[18..21]"]  [mkFs (Just DMN_Number) "3",  mkFs (Just DMN_Number) "4"]    []
        ,DTrow (Just 3) [mkFs (Just DMN_Number) ">=18"]      [mkFs (Just DMN_Number) "5",  mkFs (Just DMN_Number) "6"]    []
        ,DTrow (Just 4) [mkFs (Just DMN_Number) ">=65"]      [mkFs (Just DMN_Number) "7",  mkFs (Just DMN_Number) "8"]    []
        ])
    let evaled3 = (throwOnLeft (parseOnly (parseTable "mytable1") dmn4count))
    it "17 should match 1 row"    $ evalTable evaled3 [FNullary (VN 17.0)] `shouldBe` Right [ [[FNullary $ VN  1.0 ]] ]
    it "18 should match 2 rows"   $ evalTable evaled3 [FNullary (VN 18.0)] `shouldBe` Right [ [[FNullary $ VN  2.0 ]] ]


  describe "evalTable dmn4d - collect min" $ do
    it "should parse the Zelda Collect Min table"
      $ dmn4min
      ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" (HP_Collect Collect_Min)
        [ DTCH DTCH_In "Age"            (Just DMN_Number) Nothing
        , DTCH DTCH_Out "SpiritOrbs"     (Just DMN_Number) Nothing
        , DTCH DTCH_Out "KorokSeeds"     (Just DMN_Number) Nothing
        ]
        [DTrow (Just 1) [mkFs (Just DMN_Number) "<18"]       [mkFs (Just DMN_Number) "1",  mkFs (Just DMN_Number) "2"]    []
        ,DTrow (Just 2) [mkFs (Just DMN_Number) "[18..21]"]  [mkFs (Just DMN_Number) "3",  mkFs (Just DMN_Number) "4"]    []
        ,DTrow (Just 3) [mkFs (Just DMN_Number) ">=18"]      [mkFs (Just DMN_Number) "5",  mkFs (Just DMN_Number) "6"]    []
        ,DTrow (Just 4) [mkFs (Just DMN_Number) ">=65"]      [mkFs (Just DMN_Number) "7",  mkFs (Just DMN_Number) "8"]    []
        ])
    let evaled3 = (throwOnLeft (parseOnly (parseTable "mytable1") dmn4min))
    it "17 should have min result of 1"   $ evalTable evaled3 [FNullary (VN 17.0)] `shouldBe` Right [ [[FNullary $ VN  1.0 ]] ]
    it "18 should have min result of 3"   $ evalTable evaled3 [FNullary (VN 18.0)] `shouldBe` Right [ [[FNullary $ VN  3.0 ]] ]


  describe "parseFNumFunction" $ do
    it "should handle a simple string literal"
      $ ("myvar" :: Text) ~> (parseFNumFunction) `shouldParse` (FNF1 "myvar")
    it "should handle a quoted string"
      $ ("\"myvar\"" :: Text) ~> (parseFNumFunction) `shouldParse` (FNF0 $ VS "myvar")
    it "should handle a literal number"
      $ ("100.0" :: Text) ~> (parseFNumFunction) `shouldParse` (FNF0 $ VN 100.0)
    it "should handle a literal bool"
      $ ("False" :: Text) ~> (parseFNumFunction) `shouldParse` (FNF0 $ VB False)
    it "should handle two numbers multiplied"
      $ ("50.0 * 2.0" :: Text) ~> (parseFNumFunction) `shouldParse` (FNF3 (FNF0 $ VN 50) (FNMul) (FNF0 $ VN 2))
    it "should handle a variable times a number"
      $ ("age * 2.0" :: Text) ~> (parseFNumFunction) `shouldParse` (FNF3 (FNF1 "age") (FNMul) (FNF0 $ VN 2))

  -- ======================== D-2: ANCHORED INFERENCE ========================
  -- This block used to carry a KNOWN DEFECT banner saying that a green run of it
  -- was no evidence inference was right, because the expectations encoded the
  -- unanchored substring test. That is discharged: 'inferEvidence' now asks
  -- 'DMN.ParseCell.parseNumberCell', the same oracle the declared path uses.
  --
  -- Two things survived the rewrite and are worth knowing before editing here:
  --
  --   * ">23" and "<23" pass under BOTH rules, so on their own they cannot tell
  --     an anchored inference from an unanchored one. The discriminating cases
  --     are the block below them: "L1 > L2" and "Coming soon..." contain the same
  --     characters and must now be String, and "-5" must now be Number.
  --
  --   * "should infer \"no\" as a Boolean" passed "yes", not "no" — a copy-paste
  --     in the test itself, left in place deliberately until the rewrite it
  --     belonged with. It now passes "no", which is what it always claimed to.
  --
  -- See test/corpus/README.md for the symptom/policy distinction.
  -- ==========================================================================
  -- D-9. Negation, DMN 1.3 §9.2 rule 12.b. The corpus records the emitted text
  -- for all four backends; these assert the two things the corpus cannot see —
  -- the IR the cell parses to, and that 'fEval' actually inverts, which is what
  -- keeps a negated cell from parsing and then never matching anything.
  describe "negation (FNot)" $ do
    let num = mkF (Just DMN_Number)
    it "parses not([1..5]) to a negated interval"
      $ num "not([1..5])" `shouldBe` FNot (FInRange BClosed 1 5 BClosed)
    it "parses not(> 3) to a negated comparison"
      $ num "not(> 3)"    `shouldBe` FNot (FSection Fgt (VN 3))
    it "fEval inverts: 9 satisfies not([1..5])"
      $ fEval (FNot (FInRange BClosed 1 5 BClosed)) (FNullary (VN 9)) `shouldBe` True
    it "fEval inverts: 3 does not satisfy not([1..5])"
      $ fEval (FNot (FInRange BClosed 1 5 BClosed)) (FNullary (VN 3)) `shouldBe` False
    it "a negation is Number evidence for an undeclared column"
      $ inferType (mkF (Just DMN_String) "not([1..5])") `shouldBe` Just DMN_Number
    -- Rule 12.b admits simple POSITIVE unary tests, so neither of these is one.
    -- Spelled as a refusal check rather than `shouldThrow`, because mkF is
    -- `either error id` and the message is what carries the diagnosis.
    it "refuses a nested negation"
      $ parseNumberCell "not(not(> 3))" `shouldSatisfy` isLeft
    it "refuses a negated function call"
      $ parseNumberCell "not(floor(3))" `shouldSatisfy` isLeft
    it "refuses a negated arithmetic expression"
      $ parseNumberCell "not(40 - 50)"  `shouldSatisfy` isLeft
    it "accepts a negated interval"
      $ parseNumberCell "not([1..5])"   `shouldSatisfy` isRight

  describe "type inference" $ do
    it "should infer [1..2] as a Number"    $ inferType (mkF (Just DMN_String) "[1..2]") `shouldBe` Just DMN_Number
    it "should infer 123 as a Number"       $ inferType (mkF (Just DMN_String) "123")    `shouldBe` Just DMN_Number
    it "should infer >23 as a Number"       $ inferType (mkF (Just DMN_String) ">23")    `shouldBe` Just DMN_Number
    it "should infer <23 as a Number"       $ inferType (mkF (Just DMN_String) "<23")    `shouldBe` Just DMN_Number
    it "should infer \"yes\" as a Boolean"  $ inferType (mkF (Just DMN_String) "yes")    `shouldBe` Just DMN_Boolean
    it "should infer \"no\" as a Boolean"   $ inferType (mkF (Just DMN_String) "no")     `shouldBe` Just DMN_Boolean
    it "double-quoted string is a String"   $ inferType (mkF (Just DMN_String) "\"quoted\"")    `shouldBe` Just DMN_String
    it "should infer a stringified age * 2 as a Number" $ inferType (mkF (Just DMN_String) "age * 2") `shouldBe` Just DMN_Number
    it "should infer an explicit Function age * 2 as a Number" $ inferType (FFunction (FNF3 (FNF1 "age") FNMul (FNF0 $ VN 2))) `shouldBe` Just DMN_Number

  -- The cases that DISTINGUISH anchored inference from the substring test it
  -- replaced. Each of the first four was typed Number by the old rule; each of
  -- the next two was typed String. All six are also corpus recordings, which is
  -- the machine-checked copy — these are here so the unit for the decision has
  -- one too, and so a future edit to inferEvidence fails fast rather than at the
  -- corpus step.
  describe "type inference — anchored (D-2)" $ do
    it "prose containing > is a String, not a Number"
      $ inferEvidence (mkF Nothing "L1 > L2")        `shouldBe` EType DMN_String
    it "prose containing .. is a String, not a Number"
      $ inferEvidence (mkF Nothing "Coming soon...") `shouldBe` EType DMN_String
    it "a negative number is a Number"
      $ inferEvidence (mkF Nothing "-5")             `shouldBe` EType DMN_Number
    it "a leading-dot number is a Number"
      $ inferEvidence (mkF Nothing ".5")             `shouldBe` EType DMN_Number
    it "a padded 007 is ambiguous, and names itself"
      $ inferEvidence (mkF Nothing "007")            `shouldBe` EAmbiguous "007"
    it "0.5 is NOT padded — one leading zero is decimal notation"
      $ inferEvidence (mkF Nothing "0.5")            `shouldBe` EType DMN_Number
    it "arithmetic is evidence of last resort, not hard evidence"
      $ inferEvidence (mkF Nothing "age * 2")        `shouldBe` EWeakNumber
    it "a NAMED ParseCell refusal is numeric evidence, not \"not a number\""
      $ inferEvidence (mkF Nothing "not([1..5])")    `shouldBe` EType DMN_Number
    it "a wildcard says nothing"
      $ inferEvidence (mkF Nothing "-")              `shouldBe` ENoEvidence

  describe "type inference — the column verdict (D-2)" $ do
    let col = columnVerdict . map (pure . mkF Nothing)
    it "resolves a column its cells agree on"
      $ col ["<= 8", "> 8"]        `shouldBe` VType DMN_Number
    it "refuses a column its cells disagree on"
      $ col ["<= 8", "unknown"]    `shouldBe` VConflict [DMN_Number, DMN_String]
    it "an all-wildcard column is VNone, NOT a conflict — it must stay silent"
      $ col ["-", "-"]             `shouldBe` VNone
    it "arithmetic alone still resolves the column"
      $ col ["age * 2", "-"]       `shouldBe` VType DMN_Number
    it "arithmetic does not outvote a String cell"
      $ col ["Non-Participating", "5' 10\""] `shouldBe` VType DMN_String
    it "should infer dmn5a as number, number, bool" $
      dmn5a ~> (parseTable "dmn5a") `shouldParse` 
      (DTable "dmn5a" (HP_Collect Collect_Max)
        [ DTCH DTCH_In "Age"            (Just DMN_Number) Nothing
        , DTCH DTCH_Out "SpiritOrbs"     (Just DMN_Number) Nothing
        , DTCH DTCH_Out "KorokSeeds"     (Just DMN_Boolean) Nothing
        ]
        [DTrow (Just 1) [mkFs (Just DMN_Number) "<18"]       [mkFs (Just DMN_Number) "1",  [(FNullary $ VB True )]]    []
        ,DTrow (Just 2) [mkFs (Just DMN_Number) "[18..21]"]  [mkFs (Just DMN_Number) "3",  [(FNullary $ VB False)]]    []
        ,DTrow (Just 3) [mkFs (Just DMN_Number) ">=18"]      [mkFs (Just DMN_Number) "5",  [(FNullary $ VB True )]]    []
        ,DTrow (Just 4) [mkFs (Just DMN_Number) ">=65"]      [mkFs (Just DMN_Number) "7",  [(FNullary $ VB False)]]    []
        ])
    it "should infer dmn5b as number, number, bool" $
      dmn5b ~> (parseTable "dmn5b") `shouldParse` 
      (DTable "dmn5b" (HP_Collect Collect_Max)
        [ DTCH DTCH_In "Age"            (Just DMN_Number) Nothing
        , DTCH DTCH_Out "SpiritOrbs"     (Just DMN_String) Nothing
        , DTCH DTCH_Out "KorokSeeds"     (Just DMN_Boolean) Nothing
        ]
        [DTrow (Just 1) [mkFs (Just DMN_Number) "<18"]       [mkFs (Just DMN_String) "one",  [(FNullary $ VB True )]]    []
        ,DTrow (Just 2) [mkFs (Just DMN_Number) "[18..21]"]  [mkFs (Just DMN_String) "three",  [(FNullary $ VB False)]]    []
        ,DTrow (Just 3) [mkFs (Just DMN_Number) ">=18"]      [mkFs (Just DMN_String) "five",  [(FNullary $ VB True )]]    []
        ,DTrow (Just 4) [mkFs (Just DMN_Number) ">=65"]      [mkFs (Just DMN_String) "seven",  [(FNullary $ VB False)]]    []
        ])

  -- Example 6 was troublesome when switching to megaparsec
  describe "example 6" $ do
    it "should parse a simple expression" $
      "age * 100" ~> parseFNumFunction `shouldParse` FNF3 (FNF1 "age") FNMul (FNF0 (VN 100.0))
    let evaled = columnSigs . reviseInOut . throwOnLeft $ parseOnly parseHeaderRow $ head $ T.lines dmn6a
    it "should handle the last line" $
      (last (T.lines dmn6a) <> "\n") ~> (parseDataRow "mytable1" evaled) `shouldParse`
        (DTrow (Just 4) [[FSection Fgt (VN 25.0)]] [[FNullary $ VB True], [FFunction (FNF3 (FNF1 "age") FNMul (FNF0 (VN 100.0)))]] [])
    -- parseOnly (parseDataRow "mytable1" [ColSig DTCH_In "age" (Just DMN_Number), ColSig DTCH_Out "mayBuy" (Just DMN_Boolean), ColSig DTCH_Out "limit" (Just DMN_Number)]) "| 4 | >25          | True                   | age * 100            |\n"
    it "should parse correctly" $
      dmn6a ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" HP_First
        [ DTCH DTCH_In "age" (Just DMN_Number) Nothing
        , DTCH DTCH_Out "mayBuy" (Just DMN_Boolean) Nothing
        , DTCH DTCH_Out "limit" (Just DMN_Number) Nothing
        ]
        [ DTrow (Just 1) [[FSection Flt (VN 18.0)]] [[FNullary $ VB False], [FNullary (VN 0.0)]] []
        , DTrow (Just 2) [[FInRange BClosed 18.0 21.0 BClosed]]     [[FNullary $ VB True], [FNullary (VN 750.0)]] []
        , DTrow (Just 3) [[FInRange BClosed 21.0 25.0 BClosed]]     [[FNullary $ VB True], [FNullary (VN 1500.0)]] []
        , DTrow (Just 4) [[FSection Fgt (VN 25.0)]] [[FNullary $ VB True], [FFunction (FNF3 (FNF1 "age") FNMul (FNF0 (VN 100.0)))]] []
        ]
      )

  describe "function evaluation " $ do
    let evaled = (throwOnLeft (parseOnly (parseTable "mytable1") dmn6a))
    it "17 should have result of 0"      $ evalTable evaled [FNullary (VN 17.0)] `shouldBe` Right [ [[FNullary (VB False)], [FNullary $ VN    0.0 ]] ]
    it "18 should have result of 750"    $ evalTable evaled [FNullary (VN 18.0)] `shouldBe` Right [ [[FNullary (VB True)],  [FNullary $ VN  750.0 ]] ]
    it "30 should have result of 3000"   $ evalTable evaled [FNullary (VN 30.0)] `shouldBe` Right [ [[FNullary (VB True)],  [FNullary $ VN 3000.0 ]] ]



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

-- | Hit policy A. Rows 1 and 2 overlap deliberately: Age 5 matches both and they
-- agree, which is the situation ANY exists to permit. Age 25 matches row 2 alone.
-- Nothing matches Age 99.
dmnAnyAgree :: Text
dmnAnyAgree = T.pack $ dropWhile (=='\n') [r|
| A | Age : Number         | Verdict : String             |
|---+----------------------+------------------------------|
| 1 | < 10                 | ok                           |
| 2 | < 30                 | ok                           |
|]

-- | The same table with row 2 disagreeing, which makes it ill-defined under DMN.
-- Spelled @deny@ rather than @no@: a @no@ cell infers Boolean and D-2 anchored
-- inference then refuses the column before 'evalTable' is ever reached.
dmnAnyDisagree :: Text
dmnAnyDisagree = T.pack $ dropWhile (=='\n') [r|
| A | Age : Number         | Verdict : String             |
|---+----------------------+------------------------------|
| 1 | < 10                 | ok                           |
| 2 | < 30                 | deny                         |
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

-- | Collection semantics at the level they are decided: 'fEval' for matching,
-- 'mkInputValue' for the runtime argument, 'splitArgs' for the argument split.
-- Cheaper and sharper here than through the CLI.
listSpec :: Spec
listSpec = do
  describe "DMN.DecisionTable.fEval — collection arguments are membership" $ do
    it "matches when the collection contains the cell's value" $
      fEval (FNullary (VS "admin")) (FNullary (VL [VS "admin", VS "x"])) `shouldBe` True
    it "does not match when it does not" $
      fEval (FNullary (VS "admin")) (FNullary (VL [VS "clerk"])) `shouldBe` False
    it "is False over the empty collection — there is no vacuous-truth case" $
      fEval (FNullary (VS "admin")) (FNullary (VL [])) `shouldBe` False
    it "lets the wildcard match any collection, including the empty one" $ do
      fEval FAnything (FNullary (VL [VS "a"])) `shouldBe` True
      fEval FAnything (FNullary (VL []))       `shouldBe` True
    it "matches numerically, so 5 is found in [1,5,9]" $
      fEval (FNullary (VN 5)) (FNullary (VL [VN 1, VN 5, VN 9])) `shouldBe` True

  describe "DMN.DecisionTable.mkInputValue — an argument is a VALUE, not a test" $ do
    it "reads a collection written the way FEEL writes one" $
      mkInputValue (Just (DMN_List DMN_String)) "[a, b]"
        `shouldBe` Right (FNullary (VL [VS "a", VS "b"]))
    it "reads the empty collection" $
      mkInputValue (Just (DMN_List DMN_String)) "[]"
        `shouldBe` Right (FNullary (VL []))
    it "types the elements, not just the list" $
      mkInputValue (Just (DMN_List DMN_Number)) "[1, 5]"
        `shouldBe` Right (FNullary (VL [VN 1, VN 5]))
    it "refuses a scalar where a collection belongs, naming the repair" $
      case mkInputValue (Just (DMN_List DMN_String)) "admin" of
        Left msg -> msg `shouldContain` "[a, b, c]"
        Right v  -> expectationFailure ("expected a refusal, got " ++ show v)
    it "refuses an element that is not of the declared element type" $
      case mkInputValue (Just (DMN_List DMN_Number)) "[1, wat]" of
        Left msg -> msg `shouldContain` "expected a number"
        Right v  -> expectationFailure ("expected a refusal, got " ++ show v)

  describe "DMN.DecisionTable.splitArgs — bracket-aware argument split" $ do
    it "keeps a collection literal as ONE argument" $
      splitArgs "[1,2,3], x" `shouldBe` ["[1,2,3]", " x"]
    it "still splits ordinary scalar arguments" $
      splitArgs "a, b" `shouldBe` ["a", " b"]
