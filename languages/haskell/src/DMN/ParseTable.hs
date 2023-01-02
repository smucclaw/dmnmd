{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module DMN.ParseTable where

import Prelude hiding (takeWhile)
import DMN.DecisionTable ( mkFs, trim, mkDTable )
import DMN.ParseFEEL ( parseVarname, skipHorizontalSpace )
import DMN.SFeelGrammar
import Data.Maybe (catMaybes)
import Data.List (transpose)
import Data.Either (isLeft)
import Control.Applicative ( Alternative((<|>)) )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec as Mega
import Text.Megaparsec
    ( (<?>),
      runParser,
      satisfy,
      option,
      some,
      many,
      manyTill,
      lookAhead,
      MonadParsec(try) )
import Text.Megaparsec.Char ( char )
import DMN.ParsingUtils
    ( Parser, inClass, skipWhile, digit, endOfLine, endOfInput, many1 )
import DMN.Types
    ( ColBody(..),
      HeaderRow(..),
      DTrow(DTrow),
      DecisionTable,
      ColHeader(DTCH, label, enums, vartype),
      DTCH_Label(..),
      FEELexp(FAnything),
      DMNType(..),
      CollectOperator(Collect_Sum, Collect_All, Collect_Cnt, Collect_Min,
                      Collect_Max),
      HitPolicy(HP_Collect, HP_Unique, HP_Any, HP_Priority, HP_First,
                HP_OutputOrder, HP_RuleOrder) )
import Debug.Trace
import Control.Monad (when)

pipeSeparator :: Parser ()
pipeSeparator = try $ Mega.label "pipeSeparator" $ skipHorizontalSpace >> "|" >> skipHorizontalSpace
-- pipeSeparator = Mega.label "pipeSeparator" $ try skipHorizontalSpace >> skip (=='|') >> skipHorizontalSpace

getpipeSeparator :: Parser Text
getpipeSeparator = skipHorizontalSpace *> "|" <* skipHorizontalSpace

parseColHeader :: Parser ColHeader
parseColHeader = do
  mylabel_pre <- parseLabelPre <* skipHorizontalSpace <?> "mylabel_pre"
  myvarname <- parseVarname <?> "parseVarname"
  -- doTrace $ "parseColHeader: done with parseVarname, got: \"" ++ T.unpack myvarname ++ "\""
  mytype <- parseTypeDecl <?> "parseTypeDecl"
  mylabel_post <- skipHorizontalSpace *> parseLabelPost <?> "parseLabelPost"
  return $ DTCH (mkHeaderLabel mylabel_pre mylabel_post) (T.unpack myvarname) mytype Nothing

parseTypeDecl :: Parser (Maybe DMNType) -- Nothing means it's up to some later code to infer the type. Usually it gets treated just like a String.
parseTypeDecl = Mega.optional $ do
  skipHorizontalSpace >> ":" >> skipHorizontalSpace
  parseType

parseType :: Parser DMNType
parseType =
  DMN_List <$> ("[" *> parseType <* "]" <?> "inside list")
   <|>
   ((("String"  >> return (DMN_String)) <?> "string type") <|>
    (("Number"  >> return (DMN_Number)) <?> "number type") <|>
    (("Boolean" >> return (DMN_Boolean)) <?> "boolean type") ) -- need to check what the official DMN names are for these

mkHeaderLabel (Just "//") _        = DTCH_Comment
mkHeaderLabel (Just "#" ) _        = DTCH_Comment
mkHeaderLabel _ (Just "(comment)") = DTCH_Comment
mkHeaderLabel _ (Just "(out)")     = DTCH_Out
mkHeaderLabel _ (Just "(in)")      = DTCH_In
mkHeaderLabel _  Nothing           = DTCH_In

parseLabelPre :: Parser (Maybe Text)
parseLabelPre  = Mega.optional ("//"   <|> "#")

parseLabelPost :: Parser (Maybe Text)
parseLabelPost = Mega.optional ("(in)" <|> "(out)" <|> "(comment)")

parseHitPolicy :: Parser HitPolicy
parseHitPolicy = 
  mkHitPolicy_  <$> satisfy (inClass "UAPFOR")
  <|>
  (char 'C' >> skipHorizontalSpace >> (mkHitPolicy_C <$> option 'A' (satisfy (inClass "#<>+A"))))

parseHeaderRow :: Parser HeaderRow
parseHeaderRow = do
  pipeSeparator <?> "pipeSeparator"
  myhitpolicy <- parseHitPolicy  <?> "hitPolicy"
  pipeSeparator <?> "pipeSeparator"
  mychs <- many (parseColHeader <* pipeSeparator <?> "parseColHeader" ) <?> "mychs"
  endOfLine <|> endOfInput
  return $ DTHR myhitpolicy mychs

mkHitPolicy_ :: Char -> HitPolicy
mkHitPolicy_ 'U' = HP_Unique
mkHitPolicy_ 'A' = HP_Any
mkHitPolicy_ 'P' = HP_Priority
mkHitPolicy_ 'F' = HP_First
mkHitPolicy_ 'O' = HP_OutputOrder
mkHitPolicy_ 'R' = HP_RuleOrder

mkHitPolicy_C :: Char -> HitPolicy
mkHitPolicy_C 'A' = HP_Collect Collect_All
mkHitPolicy_C '#' = HP_Collect Collect_Cnt
mkHitPolicy_C '<' = HP_Collect Collect_Min
mkHitPolicy_C '>' = HP_Collect Collect_Max
mkHitPolicy_C '+' = HP_Collect Collect_Sum

-- TODO: consider allowing spaces in variable names

-- TODO: use sepBy in parsing columns

-- TODO: think about refactoring this into a multi-step parser:
-- 1. read the table into a 2d array of strings
-- 2. intuit whether it's a vertical, horizontal, or crosstab table
-- 3. perform type inference on the table values
-- 4. consider whether the values are consistent with the type declarations in the header
-- 5. vertical and horizontal get canonicalized into a common logical representation
-- 6. crosstab tables get reformatted into that representation
-- 7. then we have a validated decision table.

-- also, see page 112 for boxed expressions -- the contexts are pretty clearly an oop / record paradigm
-- so we probably need to bite the bullet and just support JSON input.

parseTable :: String -> Parser DecisionTable
parseTable tableName = do
  input <- lookAhead Mega.takeRest
  -- doTrace ("parseTable: starting. input =\n" ++ T.unpack input)
  -- doTrace ("parseTable: end of input")
  headerRow_1 <- reviseInOut <$> parseHeaderRow <?> "parseHeaderRow"
  -- doTrace ("parseTable: parseHeaderRow gave: " ++ show headerRow_1)
  let columnSignatures = columnSigs headerRow_1
  subHeadRow <- parseContinuationRows <?> "parseSubHeadRows"
  -- merge headerRow with subHeadRows
  let headerRow = if not (null subHeadRow)
                  then  headerRow_1 { cols = zipWith (\orig subhead -> orig { enums = if subhead == [FAnything] then Nothing else Just subhead } ) -- in the data section a blank cell means anything, but in the subhead it means nothing.
                                             (cols headerRow_1)
                                             (zipWith (\cs cell -> mkFs (snd cs) cell) columnSignatures subHeadRow) }
                  else headerRow_1
  dataRows <- parseDataRows columnSignatures <?> "parseDataRows"
  -- when our type inference is stronger, let's make the cells all just strings, and let the inference engine validate all the cells first, then infer, then construct.
  return ( mkDTable tableName (hrhp headerRow)
           (cols headerRow)
           dataRows )

grep_out_dashes :: String -> String
grep_out_dashes x = unlines ( filter ( \str -> isLeft $ runParser parseDThr "internal" $ T.pack str ) ( lines x ) )

parseDThr :: Parser ()
parseDThr = do
    ("|---" <|> "| -") >> skipWhile "character" (/='\n') >> endOfLine
    return ()

-- continuation rows are used to match both subheads and datarow continuations    
parseContinuationRows :: Parser [String]
parseContinuationRows = do
  plainrows <- many (try (many parseDThr >> parseContinuationRow <?> "parseContinuationRow")) <?> "ParseContinuationRows"
  return $ trim . unwords <$> transpose plainrows

parseContinuationRow :: Parser [String]
parseContinuationRow = try $ do -- We need try since it starts with a pipe
      pipeSeparator
      skipHorizontalSpace
      pipeSeparator
      parseTail <?> "parseTail"

parseTail :: Parser [String]
parseTail = do
  rowtail <- manyTill (manyTill (satisfy (/='|') <?> "Non-pipe") pipeSeparator <?> "headr") (skipHorizontalSpace >> endOfLine)
  return $ trim <$> rowtail
  
-- the pattern is this:
-- |---|------ DThr --- horizontal rule
-- | N | data row start   A1  | B1 | -- a single logical data row spans multiple physical rows
-- |   | continuation row A2  | B2 | 
-- |   | continuation row A3  | B3 |
-- subsequently, the lines are jammed together and converted to FEEL columns. "A1 A2 A3", "B1 B2 B3" happens thanks to "map unwords $ transpose"


parseDataRows :: [ColumnSignature] -> Parser [DTrow]
parseDataRows csigs = do
  -- the input could be a regular data row | foo | bar | baz |
  -- or it could be a horizontal rule, which used to be called DThr
  -- nowadays we discard all horizontal rules whenever we encounter them
  -- so … now i want to match something and then throw it away.
  drows <- many (try ((many parseDThr <?> "parseDThr") >> parseDataRow csigs <?> "parseDataRow"))
  endOfInput
  return drows

doTrace t = when True $ traceM t

-- | given some hints as to the types (from the column headers, aka the column signatures), parse a row of data.
-- Because each row has multiple columns, we use `zipWith` to, basically, fmap over both the column signatures and the data cells.
-- In the same breath we distinguish between the input columns and the output columns.
-- mkFEELCol does the heavy lifting here.

parseDataRow :: [ColumnSignature] -> Parser DTrow
parseDataRow csigs =
  do
      pipeSeparator
      myrownumber <- many1 digit <?> "row number"
      pipeSeparator
      firstrowtail <- parseTail
      doTrace $ unlines [ "** ParseDataRows: calling parseDThr and parseContinuationRow" ]
      morerows <- many (try ((many parseDThr <?> "parseDThr") >> parseContinuationRow))
      let transposed = map (trim . unwords) $ transpose (firstrowtail : morerows)
          datacols = zipWith mkFEELCol csigs transposed
          toreturn =
            DTrow (if not (null myrownumber) then Just $ (\n -> read n :: Int) myrownumber else Nothing)
            (catMaybes (zipWith getInputs  csigs datacols))
            (catMaybes (zipWith getOutputs csigs datacols))
            (catMaybes (zipWith getComments csigs datacols))
          sfeelPd = zip transposed (runParser simpleExpression "" . T.pack <$> transposed)
      doTrace $ unlines $ [ "parseDataRows: mkFEELCol running on"
                          , "    csigs = " <> show csigs
                          , "    transposed = " <> show transposed
                          , "    toreturn = " <> show toreturn
                          ] ++ [ "*** sfeelPd: \"" <> i <> "\"\n" <>
                                 either (\err -> "original:\n" <> i <> "\n" <> Mega.errorBundlePretty err) show o
                               | (i,o) <- sfeelPd ]
      return toreturn
  
  -- TODO: if myrownumber is blank, append the current row to the previous row as though connected by a ,
  where
    getInputs (DTCH_In, _) (DTCBFeels fexps) = Just fexps
    getInputs _ _ = Nothing
    getOutputs (DTCH_Out, _) (DTCBFeels fexps) = Just fexps
    getOutputs _ _ = Nothing
    getComments (DTCH_Comment, _) (DTComment mcs) = Just mcs
    getComments _ _ = Nothing

mkFEELCol :: ColumnSignature -> String -> ColBody
mkFEELCol (DTCH_Comment, _)      = mkDataColComment
mkFEELCol (DTCH_In, maybe_type)  = mkDataCol maybe_type
mkFEELCol (DTCH_Out, maybe_type) = mkDataCol maybe_type

type ColumnSignature = (DTCH_Label, Maybe DMNType)

columnSigs :: HeaderRow -> [ColumnSignature]
columnSigs = fmap (\ch -> (label ch, vartype ch)) . cols

-- hack: convert (in, in, in) to (in, in, out)
-- Control.Arrow would probably let us phrase this more cleverly, a la hxt, with "guard" and "when" but this is probably more readable for a beginner
reviseInOut :: HeaderRow -> HeaderRow
reviseInOut hr = let noncomments = filter ((DTCH_Comment /= ) . label) $ cols hr
                 in if length noncomments > 1 && all ((DTCH_In ==) . label) noncomments
                    then let rightmost = last noncomments
                         in hr { cols = map (\ch -> if ch == rightmost then ch { label = DTCH_Out } else ch) (cols hr) }
                    else hr

mkDataCol :: Maybe DMNType -> String -> ColBody
mkDataCol dmntype = DTCBFeels . mkFs dmntype 
mkDataColComment :: String -> ColBody
mkDataColComment mcs = DTComment (if mcs == "" then Nothing else Just mcs)
