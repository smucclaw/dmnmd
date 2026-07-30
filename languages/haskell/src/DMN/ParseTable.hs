{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module DMN.ParseTable where

import Prelude hiding (takeWhile)
import DMN.DecisionTable ( CellSite(..), mkFsAt, trim, mkDTable )
import DMN.ParseFEEL ( parseVarname )
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
      MonadParsec(try) )
import Text.Megaparsec.Char ( char )
import DMN.ParsingUtils
    ( Parser, inClass, skipWhile, digit, endOfLine, endOfInput, many1, lexeme, skipHorizontalSpace )
import DMN.Types
    ( ColBody(..),
      HeaderRow(..),
      DTrow(DTrow),
      DecisionTable,
      ColHeader(DTCH, label, enums, varname, vartype),
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

-- | parse column header.
-- (//|#|>|<) *([a-zA-Z0-9_ ]+?( *: *[a-z]+) *
parseColHeader :: Parser ColHeader
parseColHeader = do
  mylabel_pre   <- parseLabelPre <?> "pre-label"
  myvarname     <- parseVarname <?> "variable name"
  doTrace $ "parseColHeader: done with parseVarname, got: \"" ++ T.unpack myvarname ++ "\""
  -- Accept the (in)/(out)/(comment) post-label in EITHER order relative to the
  -- ": Type" declaration: dmnmd's own fixtures write "name : Type (out)", while
  -- the homelab golden writes "name (out) : Type". Try the label both before and
  -- after the type so both spellings parse to the same ColHeader.
  mylabel_postA <- skipHorizontalSpace *> parseLabelPost <?> "post-label (in/out/comment)"
  mytype        <- parseTypeDecl <?> "type declaration"
  mylabel_postB <- skipHorizontalSpace *> parseLabelPost <?> "post-label (in/out/comment)"
  return ( DTCH
           (mkHeaderLabel mylabel_pre (mylabel_postA <|> mylabel_postB))
           (T.unpack myvarname)
           mytype Nothing )

-- | Nothing means it's up to some later code to infer the type. Usually it gets treated just like a String.
parseTypeDecl :: Parser (Maybe DMNType)
parseTypeDecl = Mega.optional $ lexeme ":" *> parseType

parseType :: Parser DMNType
parseType
  =   (DMN_List    <$>  (lexeme "[" *> parseType <* lexeme "]") <?> "inside list")
  <|> (DMN_String  <$    lexeme "String"                        <?> "string type")
  <|> (DMN_Number  <$    lexeme "Number"                        <?> "number type")
  <|> (DMN_Boolean <$    lexeme "Boolean"                       <?> "boolean type")
  -- need to check what the official DMN names are for these
    
mkHeaderLabel :: Maybe Text -> Maybe Text -> DTCH_Label
mkHeaderLabel (Just "//") _        = DTCH_Comment
mkHeaderLabel (Just "#" ) _        = DTCH_Comment
mkHeaderLabel (Just "<" ) _        = DTCH_In
mkHeaderLabel (Just ">" ) _        = DTCH_Out
mkHeaderLabel _ (Just "(comment)") = DTCH_Comment
mkHeaderLabel _ (Just "(out)")     = DTCH_Out
mkHeaderLabel _ (Just "(in)")      = DTCH_In
mkHeaderLabel _  Nothing           = DTCH_In
-- 'parseLabelPre' and 'parseLabelPost' are the only producers, and each is an
-- alternation over exactly the literals matched above, so this is unreachable
-- unless one of those alternations grows a case this function was not told
-- about. Naming the pair beats a bare @Non-exhaustive patterns@.
mkHeaderLabel pre post = error $ unwords
  [ "mkHeaderLabel: unrecognised column label", show pre, show post
  , "-- parseLabelPre/parseLabelPost gained a literal this function does not handle" ]

parseLabelPre :: Parser (Maybe Text)
parseLabelPre  = Mega.optional $ lexeme ("//" <|> "#" <|> "<" <|> ">")

parseLabelPost :: Parser (Maybe Text)
parseLabelPost = Mega.optional $ lexeme ("(in)" <|> "(out)" <|> "(comment)")

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
-- Guarded by @satisfy (inClass "UAPFOR")@ in 'parseHitPolicy'; unreachable
-- unless that character class and this function drift apart.
mkHitPolicy_ c   = error $ unwords
  [ "mkHitPolicy_: not a hit policy:", show c
  , "-- parseHitPolicy's character class and this function disagree" ]

mkHitPolicy_C :: Char -> HitPolicy
mkHitPolicy_C 'A' = HP_Collect Collect_All
mkHitPolicy_C '#' = HP_Collect Collect_Cnt
mkHitPolicy_C '<' = HP_Collect Collect_Min
mkHitPolicy_C '>' = HP_Collect Collect_Max
mkHitPolicy_C '+' = HP_Collect Collect_Sum
-- Guarded by @satisfy (inClass "#<>+A")@ in 'parseHitPolicy'; same invariant.
mkHitPolicy_C c   = error $ unwords
  [ "mkHitPolicy_C: not a collect operator:", show c
  , "-- parseHitPolicy's character class and this function disagree" ]

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
  input <- Mega.lookAhead Mega.takeRest
  doTrace ("parseTable: starting. input =\n" ++ T.unpack input)
  doTrace ("parseTable: end of input")
  headerRow_1 <- reviseInOut <$> parseHeaderRow <?> "parseHeaderRow"
  doTrace ("parseTable: parseHeaderRow gave: " ++ show headerRow_1)
  let columnSignatures = columnSigs headerRow_1
  subHeadRow <- parseContinuationRows <?> "parseSubHeadRows"
  -- merge headerRow with subHeadRows
  let headerRow = if not (null subHeadRow)
                  then  headerRow_1 { cols = zipWith (\orig subhead -> orig { enums = if subhead == [FAnything] then Nothing else Just subhead } ) -- in the data section a blank cell means anything, but in the subhead it means nothing.
                                             (cols headerRow_1)
                                             -- siteRow = Nothing: the sub-header row has no rule number.
                                             (zipWith (\cs cell -> mkFsAt (CellSite tableName (csName cs) Nothing) (csType cs) cell) columnSignatures subHeadRow) }
                  else headerRow_1
  dataRows <- parseDataRows tableName columnSignatures <?> "parseDataRows"
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


-- | The table name is carried purely so a refused cell can say which table it
-- was in; see 'DMN.DecisionTable.CellSite'.
parseDataRows :: String -> [ColumnSignature] -> Parser [DTrow]
parseDataRows tableName csigs = do
  -- the input could be a regular data row | foo | bar | baz |
  -- or it could be a horizontal rule, which used to be called DThr
  -- nowadays we discard all horizontal rules whenever we encounter them
  -- so … now i want to match something and then throw it away.
  drows <- many (try ((many parseDThr <?> "parseDThr") >> parseDataRow tableName csigs <?> "parseDataRow"))
  endOfInput
  return drows

doTrace t = when False $ traceM t

parseDataRow :: String -> [ColumnSignature] -> Parser DTrow
parseDataRow tableName csigs =
  do
      pipeSeparator
      myrownumber <- many1 digit <?> "row number"
      pipeSeparator
      firstrowtail <- parseTail
      doTrace $ unlines [ "ParseDataRows: calling parseDThr and parseContinuationRow" ]
      morerows <- many (try ((many parseDThr <?> "parseDThr") >> parseContinuationRow))
      let transposed = map (trim . unwords) $ transpose (firstrowtail : morerows)
          -- Bound once and used both for the DTrow and for the CellSite of every
          -- cell in it, so a diagnostic can never name a different row from the
          -- one the row records. `many1 digit` cannot return "", so the Nothing
          -- branch is dead; it is left exactly as it was rather than tidied,
          -- because the row that would want it is eaten earlier by
          -- parseContinuationRow (symptom/struct-blank-rownum-swallowed).
          myrow = if not (null myrownumber) then Just $ (\n -> read n :: Int) myrownumber else Nothing
          datacols = zipWith (mkFEELCol tableName myrow) csigs transposed
      doTrace $ unlines [ "parseDataRows: mkFEELCol running on"
                        , "    csigs = " <> show csigs
                        , "    transposed = " <> show transposed
                        ]

      return ( DTrow myrow
               (catMaybes (zipWith getInputs  csigs datacols))
               (catMaybes (zipWith getOutputs csigs datacols))
               (catMaybes (zipWith getComments csigs datacols)) )

  -- TODO: if myrownumber is blank, append the current row to the previous row as though connected by a ,
  where
    getInputs ColSig{csLabel = DTCH_In} (DTCBFeels fexps) = Just fexps
    getInputs _ _ = Nothing
    getOutputs ColSig{csLabel = DTCH_Out} (DTCBFeels fexps) = Just fexps
    getOutputs _ _ = Nothing
    getComments ColSig{csLabel = DTCH_Comment} (DTComment mcs) = Just mcs
    getComments _ _ = Nothing

mkFEELCol :: String -> Maybe Int -> ColumnSignature -> String -> ColBody
mkFEELCol _         _     (ColSig DTCH_Comment _  _        ) = mkDataColComment
mkFEELCol tableName myrow (ColSig DTCH_In      nm maybe_type) = mkDataCol (CellSite tableName nm myrow) maybe_type
mkFEELCol tableName myrow (ColSig DTCH_Out     nm maybe_type) = mkDataCol (CellSite tableName nm myrow) maybe_type

-- | What a data row needs to know about a column: which kind it is, what it is
-- called, and what type its cells are read at.
--
-- Not 'ColHeader', though 'ColHeader' carries all three, because 'columnSigs'
-- runs on @headerRow_1@ — before 'parseTable' attaches the sub-header row's
-- @enums@. A 'ColHeader' handed to 'parseDataRow' would therefore always have
-- @enums = Nothing@ and invite somebody to read it as "this column declares no
-- domain". Three fields make that unrepresentable.
--
-- 'csName' is the column name only so a refused cell can be located; nothing
-- else reads it.
data ColumnSignature = ColSig
  { csLabel :: DTCH_Label
  , csName  :: String
  , csType  :: Maybe DMNType
  }
  deriving (Show, Eq)

columnSigs :: HeaderRow -> [ColumnSignature]
columnSigs = fmap (\ch -> ColSig (label ch) (varname ch) (vartype ch)) . cols

-- hack: convert (in, in, in) to (in, in, out)
-- Control.Arrow would probably let us phrase this more cleverly, a la hxt, with "guard" and "when" but this is probably more readable for a beginner
reviseInOut :: HeaderRow -> HeaderRow
reviseInOut hr = let noncomments = filter ((DTCH_Comment /= ) . label) $ cols hr
                 in if length noncomments > 1 && all ((DTCH_In ==) . label) noncomments
                    then let rightmost = last noncomments
                         in hr { cols = map (\ch -> if ch == rightmost then ch { label = DTCH_Out } else ch) (cols hr) }
                    else hr

mkDataCol :: CellSite -> Maybe DMNType -> String -> ColBody
mkDataCol site dmntype = DTCBFeels . mkFsAt site dmntype
mkDataColComment :: String -> ColBody
mkDataColComment mcs = DTComment (if mcs == "" then Nothing else Just mcs)
