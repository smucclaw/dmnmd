{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module DMN.ParseTable where

import Prelude hiding (takeWhile)
import DMN.BuildTable ( mkDTable )
import DMN.DecisionTable ( CellSite(..), mkFsAt, mkInputFsAt, showSite, trim )
import DMN.Diagnostic ( Diagnostic, anyErrors, errorAt, renderDiagnostic )
import DMN.ParseFEEL ( parseVarname )
import Data.Maybe (catMaybes)
import Data.List (intercalate, nub, transpose)
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
--
-- Fails, with a message, on a header whose labels disagree ('labelClash');
-- the reader goes through 'parseColHeaderL' instead, so that the disagreement
-- becomes a located 'Diagnostic' and not a parse error.
parseColHeader :: Parser ColHeader
parseColHeader = do
  (ch, written) <- parseColHeaderL
  maybe (pure ch) (fail . (("column " ++ show (varname ch) ++ ": ") ++)) (labelClash written)

-- | A column header, and every label that was written on it, in the order
-- they were written.
--
-- The header's own 'label' is only meaningful when 'labelClash' says nothing:
-- 'mkHeaderLabel' resolves a pre-label and a post-label by letting the first
-- one that is present win, which is a rule for choosing among labels that agree
-- and the very thing that used to hide the ones that did not.
parseColHeaderL :: Parser (ColHeader, [Text])
parseColHeaderL = do
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
           mytype Nothing
         , catMaybes [mylabel_pre, mylabel_postA, mylabel_postB] )

-- | What a label literal says a column is. The one table 'mkHeaderLabel' and
-- 'labelClash' share, so the two cannot disagree about a literal.
labelKind :: Text -> DTCH_Label
labelKind "//"        = DTCH_Comment
labelKind "#"         = DTCH_Comment
labelKind "<"         = DTCH_In
labelKind ">"         = DTCH_Out
labelKind "(comment)" = DTCH_Comment
labelKind "(out)"     = DTCH_Out
labelKind "(in)"      = DTCH_In
-- 'parseLabelPre' and 'parseLabelPost' are the only producers, and each is an
-- alternation over exactly the literals above.
labelKind other = error $ unwords
  [ "labelKind: unrecognised column label", show other
  , "-- parseLabelPre/parseLabelPost gained a literal this function does not handle" ]

-- | Why a column's labels cannot be reconciled, or 'Nothing' when they can.
--
-- A column is an input, an output or a comment. A header that says two of those
-- about one column, @Dish (in) (out)@ or @> Dish (comment)@, used to resolve
-- silently to whichever label was written first, and the other was dropped: the
-- column became an input or an output with no word to the author. Two labels
-- that agree, @> Dish (out)@ or @Dish (out) : Number (out)@, say the same thing
-- twice and are fine.
labelClash :: [Text] -> Maybe String
labelClash written
  | length (nub (labelKind <$> written)) < 2 = Nothing
  | otherwise = Just $ concat
      [ "the header labels this column ", enumerate (describe <$> nub written), "."
      , " A column is an input, an output or a comment, never two of them,"
      , " and dmnmd does not choose between the labels. Keep one." ]
  where
    describe w = show (T.unpack w) ++ " (" ++ kindWord (labelKind w) ++ ")"
    kindWord DTCH_In      = "an input"
    kindWord DTCH_Out     = "an output"
    kindWord DTCH_Comment = "a comment"
    enumerate [a, b] = a ++ " and " ++ b
    enumerate xs     = intercalate ", " (init xs) ++ " and " ++ last xs

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
    
-- | The label a column gets from the labels written on it, assuming they do not
-- disagree ('labelClash'): a pre-label if there is one, else a post-label, else
-- an input.
mkHeaderLabel :: Maybe Text -> Maybe Text -> DTCH_Label
mkHeaderLabel pre post = maybe DTCH_In labelKind (pre <|> post)

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
  (hr, clashes) <- parseHeaderRowL
  case clashes of
    []           -> pure hr
    (col, msg) : _ -> fail ("column " ++ show col ++ ": " ++ msg)

-- | A header row, and the columns whose labels disagree with the reason, in
-- column order. 'parseTableD' turns those into located diagnostics.
parseHeaderRowL :: Parser (HeaderRow, [(String, String)])
parseHeaderRowL = do
  pipeSeparator <?> "pipeSeparator"
  myhitpolicy <- parseHitPolicy  <?> "hitPolicy"
  pipeSeparator <?> "pipeSeparator"
  mychs <- many (parseColHeaderL <* pipeSeparator <?> "parseColHeader" ) <?> "mychs"
  endOfLine <|> endOfInput
  return ( DTHR myhitpolicy (fst <$> mychs)
         , [ (varname ch, msg) | (ch, written) <- mychs, Just msg <- [labelClash written] ] )

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

-- | 'parseTableD' for the test suite, which parses tables it expects to be
-- well-formed and wants one back.
--
-- __Not reachable from the CLI__ — @app\/ParseMarkdown.hs@ calls 'parseTableD'
-- — which is the only reason an @error@ is tolerable here. Exactly the shape
-- and the justification 'DMN.DecisionTable.mkFs' already has beside
-- 'DMN.DecisionTable.mkFsEither'. If you find yourself wanting this in @app/@,
-- you want 'parseTableD' and a diagnostics check.
parseTable :: String -> Parser DecisionTable
parseTable tableName = do
  (diags, tables) <- parseTableD tableName
  case tables of
    [t] -> pure t
    _   -> error (unlines (renderDiagnostic <$> diags))

-- | One markdown chunk to @([Diagnostic], 0-or-1 tables)@ — D-7's shape, the
-- same one 'DMN.XML.XmlToDmnmd.convTable' has always had.
--
-- Refusals from the sub-header row and from the data rows are collected here
-- rather than raised, and an 'DMN.Diagnostic.Error' among them means
-- 'mkDTable' is not called at all: pass 2 re-types cells against a column type
-- inferred from cells that pass 1 could not read, so its complaints would be
-- about dmnmd's placeholders rather than about the author's table.
parseTableD :: String -> Parser ([Diagnostic], [DecisionTable])
parseTableD tableName = do
  input <- Mega.lookAhead Mega.takeRest
  doTrace ("parseTable: starting. input =\n" ++ T.unpack input)
  doTrace ("parseTable: end of input")
  (headerRow_0, labelClashes) <- parseHeaderRowL <?> "parseHeaderRow"
  if not (null labelClashes)
    -- A column the header labels two ways has no meaning to read the rest of
    -- the table against, so the rest is skipped rather than read as whichever
    -- label happened to win. The Error means no table, as for every other
    -- pass-1 refusal; the input is consumed because 'parseOnly' wants it all.
    then do
      _ <- Mega.takeRest
      pure ( [ errorAt (showSite (CellSite tableName col Nothing) ++ msg) | (col, msg) <- labelClashes ]
           , [] )
    else parseTableBody tableName (reviseInOut headerRow_0)

-- | Everything in 'parseTableD' after the header row, which is where the
-- columns' meanings are fixed.
parseTableBody :: String -> HeaderRow -> Parser ([Diagnostic], [DecisionTable])
parseTableBody tableName headerRow_1 = do
  doTrace ("parseTable: parseHeaderRow gave: " ++ show headerRow_1)
  let columnSignatures = columnSigs headerRow_1
  subHeadRow <- parseContinuationRows <?> "parseSubHeadRows"
  let subHeadShape = subHeaderArityDiags tableName columnSignatures subHeadRow
  -- merge headerRow with subHeadRows
  -- siteRow = Nothing: the sub-header row has no rule number.
  let subHeadCells = zipWith (\cs cell -> mkFsAt (CellSite tableName (csName cs) Nothing) (csType cs) cell)
                             columnSignatures subHeadRow
      subHeadDiags = [ d | Left d <- subHeadCells ]
      -- A refused sub-header cell keeps FAnything in its place. It is never
      -- read: an Error below means no table is returned.
      subHeadOk    = either (const [FAnything]) id <$> subHeadCells
      headerRow = if not (null subHeadRow)
                  then  headerRow_1 { cols = zipWith (\orig subhead -> orig { enums = if subhead == [FAnything] then Nothing else Just subhead } ) -- in the data section a blank cell means anything, but in the subhead it means nothing.
                                             (cols headerRow_1)
                                             subHeadOk }
                  else headerRow_1
  (rowDiags, dataRows) <- parseDataRows tableName columnSignatures <?> "parseDataRows"
  -- when our type inference is stronger, let's make the cells all just strings, and let the inference engine validate all the cells first, then infer, then construct.
  let pass1 = subHeadShape ++ subHeadDiags ++ rowDiags
  pure $ if anyErrors pass1
         then (pass1, [])
         else let (ds, ts) = mkDTable tableName (hrhp headerRow) (cols headerRow) dataRows
              in (pass1 ++ ds, ts)

-- | A sub-header row has one cell per column, and a blank cell for a column
-- that declares no domain.
--
-- Cells are matched to columns by position, and 'zipWith' stops at the shorter list.
-- 'parseTableD' merges the sub-header into the header that way.
-- So a sub-header shorter than the header did not leave its last columns without a domain: it deleted them from the table.
-- A table with its output column gone emitted rules that return nothing, at exit 0 (the corpus case @md-short-subheader-refused@ pins the refusal).
-- A sub-header longer than the header lost its surplus cells the same way.
-- Neither is guessed at.
--
-- No sub-header row (@null cells@) is the ordinary case and says nothing.
-- Neither does a surplus of blank cells, which holds nothing to lose.
-- Located at the first column the row does not reach, or, for a surplus, at the table.
subHeaderArityDiags :: String -> [ColumnSignature] -> [String] -> [Diagnostic]
subHeaderArityDiags tableName csigs cells
  | null cells || n == m = []
  | n < m =
      [ errorAt $ concat
          [ case drop n csigs of
              (cs : _) -> showSite (CellSite tableName (csName cs) Nothing)
              []       -> "table " ++ show tableName ++ ": "
          , "the sub-header row has ", countOf n "cell", " but the header declares "
          , countOf m "column", ", so this column has no sub-header cell."
          , " A sub-header cell declares the domain of the column at its position,"
          , " and dmnmd does not guess which columns a short row was meant to cover."
          , " Write one cell per column, leaving the cell empty for a column that"
          , " declares no domain." ] ]
  | all null (drop m cells) = []
  | otherwise =
      [ errorAt $ concat
          [ "table ", show tableName, ": the sub-header row has ", countOf n "cell"
          , " but the header declares only ", countOf m "column"
          , ", so the cells after the last column belong to no column."
          , " Delete them, or add the columns they were meant for." ] ]
  where
    n = length cells
    m = length csigs

-- | A data row has one cell per column.
--
-- Cells are matched to columns by position, and 'zipWith' stops at the shorter list at three places downstream ('parseDataRow' itself, then @getInputs@\/@getOutputs@, then 'DMN.DecisionTable.matches').
-- So a missing input cell was not a wildcard written down: it was a guard that was never emitted, and the rule fired for any value of that column.
-- A missing output cell became an empty answer.
-- When no row reached the last columns, 'DMN.BuildTable.mkDTable' dropped their headers as well, and the table lost its output column (the corpus case @md-all-short-rows-drop-output-header@).
-- Padding with @-@ would WIDEN the rule in silence, which is why @--to=xml@ and the XML reader already refuse a short row; this refuses it at the source, for every backend.
--
-- A row is as wide as its widest physical line, because a continuation row may add cells to the logical row.
-- Located at the first column the row does not reach.
rowArityDiags :: String -> Maybe Int -> [ColumnSignature] -> [String] -> [Diagnostic]
rowArityDiags tableName myrow csigs cells = case drop n csigs of
  [] -> []
  (cs : _) ->
    [ errorAt $ concat
        [ showSite (CellSite tableName (csName cs) myrow)
        , "the row has ", countOf n "cell", " but the header declares ", countOf m "column"
        , ", so the row ends before this column."
        , " dmnmd does not pad a short row: an input cell left out would match every value of its column,"
        , " and an output cell left out would give an empty answer."
        , " Fill in the missing cells, writing - for an input that should match anything." ] ]
  where
    n = length cells
    m = length csigs

-- | @countOf 1 "cell"@ is @"1 cell"@, @countOf 2 "cell"@ is @"2 cells"@.
countOf :: Int -> String -> String
countOf k noun = show k ++ " " ++ noun ++ (if k == 1 then "" else "s")

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
parseDataRows :: String -> [ColumnSignature] -> Parser ([Diagnostic], [DTrow])
parseDataRows tableName csigs = do
  -- the input could be a regular data row | foo | bar | baz |
  -- or it could be a horizontal rule, which used to be called DThr
  -- nowadays we discard all horizontal rules whenever we encounter them
  -- so … now i want to match something and then throw it away.
  drows <- many (try ((many parseDThr <?> "parseDThr") >> parseDataRow tableName csigs <?> "parseDataRow"))
  endOfInput
  return (concatMap fst drows, snd <$> drows)

doTrace t = when False $ traceM t

parseDataRow :: String -> [ColumnSignature] -> Parser ([Diagnostic], DTrow)
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
          colResults = zipWith (mkFEELCol tableName myrow) csigs transposed
          cellDiags = rowArityDiags tableName myrow csigs transposed ++ concatMap fst colResults
          datacols = snd <$> colResults
      doTrace $ unlines [ "parseDataRows: mkFEELCol running on"
                        , "    csigs = " <> show csigs
                        , "    transposed = " <> show transposed
                        ]

      return ( cellDiags
             , DTrow myrow
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

-- | An input cell and an output cell are read by different pass-1 wrappers,
-- because they are different things: an input entry is a unary TEST, and a
-- test-shaped cell dmnmd cannot read is refused ('mkInputFsAt'); an output
-- entry is a VALUE, where @\<none\>@ or @>5 years@ is plausible text and the
-- same check would refuse working tables. This is the one place in pass 1 that
-- knows which is which — 'reviseInOut' has already run — and it is why that
-- refusal is not in 'DMN.DecisionTable.mkFEither', which cannot tell.
mkFEELCol :: String -> Maybe Int -> ColumnSignature -> String -> ([Diagnostic], ColBody)
mkFEELCol _         _     (ColSig DTCH_Comment _  _        ) = (,) [] . mkDataColComment
mkFEELCol tableName myrow (ColSig DTCH_In      nm maybe_type) = mkDataCol (mkInputFsAt (CellSite tableName nm myrow) maybe_type)
mkFEELCol tableName myrow (ColSig DTCH_Out     nm maybe_type) = mkDataCol (mkFsAt      (CellSite tableName nm myrow) maybe_type)

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

-- | A refused data cell keeps an empty cell body in its place; nothing reads it,
-- because an Error anywhere in the returned list means 'parseTableD' emits no
-- table. See 'DMN.DecisionTable.reprocessRows' for the same choice in pass 2.
mkDataCol :: (String -> Either Diagnostic [FEELexp]) -> String -> ([Diagnostic], ColBody)
mkDataCol readCell cell = case readCell cell of
  Left d   -> ([d], DTCBFeels [FAnything])
  Right fs -> ([],  DTCBFeels fs)
mkDataColComment :: String -> ColBody
mkDataColComment mcs = DTComment (if mcs == "" then Nothing else Just mcs)
