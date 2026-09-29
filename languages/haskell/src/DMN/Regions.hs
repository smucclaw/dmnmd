{-# LANGUAGE LambdaCase #-}

-- | The regions of a decision table's input space.
--
-- A __block__ is a set of values of one input column on which every cell of
-- that column gives the same answer. A __region__ is one block per input
-- column: a box in the input space on which every rule is either live
-- throughout or dead throughout. Its __live set__ is the rules that match there.
-- A table has finitely many regions, so a question about every input — does
-- any input match two rules of a @U@ table, does any input match none — is a
-- question about finitely many regions, and each region carries a concrete
-- input to ask it at.
--
-- Both readers use it: @DECISIONS.md@ D-22 part 2 refuses a conflict region on
-- scalar columns, through 'conflictErrors', a summand of
-- 'DMN.BuildTable.tableErrors'. The rest is groundwork. D-22 part 3 makes a
-- no-match region the one place an L4 result is a @MAYBE@, and D-21 puts the
-- region enumerator in dmnmd, where @emitAsserts@ will write one @#ASSERT@ per
-- region with the value 'DMN.DecisionTable.evalTable' gives at 'regionInput'.
--
-- __Cell meaning is dmnmd's own.__ Which rules a block admits is decided by
-- 'fEvals' — the function 'DMN.DecisionTable.matches' and so 'evalTable' use —
-- at one representative value per elementary piece of the column, and the
-- pieces are cut so that every cell is constant on each of them. So a region's
-- live set is by construction what dmnmd's matcher says at every point of the
-- region, including for multi-value cells, negation and declared domains.
-- Which row is a @U@ table's default is decided by 'uniqueCatchAll', the
-- function 'evalTable' asks. The test suite checks, at every region
-- representative of every table in the round-trip fixture corpus, both that
-- the matcher selects exactly the live set and that 'evalTable' answers as the
-- live set, the default and the hit policy say it must.
--
-- __What a region is not.__ It is not a decision-tree leaf: ROOTSTOCK step 0's
-- Reg CF table has 8 regions and a 5-leaf tree, and the tree compiler lives in
-- l4-ide (D-21). Nor does a region carry the answer 'evalTable' would give; it
-- carries the rules that could.
--
-- __Anything this module cannot model is a 'Left', never a partial answer.__
-- 'regionMap' refuses a table when a region would have to understand something
-- the matcher reads differently from what the author meant, or something
-- dmnmd's matcher cannot evaluate at all; 'UnsupportedKind' lists each case.
module DMN.Regions
  ( -- * Building a region map
    regionMap
  , RegionMap
  , rmTable
  , rmColumns
  , rmDefault
  , Default (..)
  , RuleIx
    -- * What is refused
  , Unsupported (..)
  , UnsupportedKind (..)
    -- * Blocks
  , ColumnBlocks (..)
  , Block (..)
  , blockRules
  , BlockValues (..)
  , Interval (..)
  , showBlock
    -- * Regions
  , Region
  , regionBlocks
  , liveRules
  , regionInput
  , regions
  , regionCount
  , isConflict
  , isNoMatch
  , conflictRegions
  , noMatchRegions
    -- * Refusing conflicts (D-22 rule 2)
  , Conflict (..)
  , conflicts
  , conflictErrors
  ) where

import           Control.Applicative ((<|>))
import           Data.Char          (isSpace)
import           Data.Function      (on)
import           Data.IntSet        (IntSet)
import qualified Data.IntSet        as IS
import           Data.List          (find, intercalate, isPrefixOf, nub, transpose)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe         (isJust, listToMaybe, mapMaybe)
import           Data.Scientific    (Scientific)

import           DMN.DecisionTable  (CellSite (..), fEvals, fnVars, getInputHeaders, getOutputHeaders,
                                     identicalGuards, showDomainMember, showHitPolicy, showSite,
                                     showType, trim, uniqueCatchAll)
import           DMN.Number         (showNumPlain)
import           DMN.ParseCell      (parseNumberCell)
import           DMN.Types

-- * Building a region map

-- | A rule's position in 'allrows', counting from 0. Not the row number the
-- author wrote, which may have gaps and repeats; @allrows dt !! i@ has that.
type RuleIx = Int

-- | A table cut into blocks, ready to enumerate. Build one with 'regionMap'.
data RegionMap = RegionMap
  { rmTable   :: DecisionTable
    -- ^ the table it was built from
  , rmColumns :: [ColumnBlocks]
    -- ^ one per input column, in header order
  , rmDefault :: Default
    -- ^ what answers where no rule does
  }

-- | What answers an input that no rule matches, under D-22's reading.
data Default
  = NoDefault
    -- ^ nothing: such an input is a no-match region
  | DeclaredDefault
    -- ^ the table carries a §8.2.11 default output value ('dtDefaultOutput')
  | TrailingCatchAll RuleIx
    -- ^ a @U@ table whose last rule is @-@ in every input column (vacuously so
    -- when there are none). D-22 part 1 reads that rule as the default, so it
    -- answers where no other rule does and conflicts with none of them. It
    -- wins over a declared default, which is then unreachable.
    --
    -- Decided by 'uniqueCatchAll', the function 'evalTable' splits the row off
    -- with, so the analyser and the interpreter cannot disagree about which
    -- row is the default.
  deriving (Eq, Show)

-- | Cut a table into blocks, or say why its regions cannot be computed.
--
-- The checks run in this order and the first failure is returned: the hit
-- policy, each row's arity, then each input column (its type, its cells, its
-- declared domain), then, for @A@, whether overlapping rules' outputs can be
-- compared.
regionMap :: DecisionTable -> Either Unsupported RegionMap
regionMap dt = do
  hitPolicyOK dt
  mapM_ (arityOK dt nIn) rows
  cols <- traverse (buildColumn dt) (zip ins cellsByColumn)
  let rm = RegionMap { rmTable = dt, rmColumns = cols, rmDefault = defaultOf dt }
  anyOutputsOK rm
  pure rm
  where
    rows = allrows dt
    ins  = getInputHeaders (header dt)
    nIn  = length ins
    -- Safe only after 'arityOK': every row then has exactly nIn cells.
    cellsByColumn
      | null rows = replicate nIn []
      | otherwise = transpose (row_inputs <$> rows)

-- | 'uniqueCatchAll' splits off the LAST row when it answers, so its index is
-- the last one. Its precedence over a declared default is 'evalTable'\'s
-- (@catchAllDefault <|> dtDefaultOutput@), as it is --to=l4's and js/ts/py's.
defaultOf :: DecisionTable -> Default
defaultOf dt = case uniqueCatchAll dt of
  (_, Just _)                         -> TrailingCatchAll (length (allrows dt) - 1)
  _ | isJust (dtDefaultOutput dt)     -> DeclaredDefault
    | otherwise                       -> NoDefault

-- * What is refused

-- | Why 'regionMap' declined a table.
data Unsupported = Unsupported
  { unsupportedKind    :: UnsupportedKind
  , unsupportedMessage :: String
    -- ^ in the house style, @table \"T\": @ then as much of
    -- @column \"C\": row N: @ as the refusal has
  }
  deriving (Eq, Show)

-- | Every shape 'regionMap' refuses, in the order it checks for them.
data UnsupportedKind
  = ListValuedHitPolicy
    -- ^ @C@ (with or without an aggregation), @R@, @O@, and the internal
    -- @HP_Aggregate@: the answer is built from every matching rule, so a
    -- region has no single rule to name, and D-22 rules only on @U@ and @A@.
  | RowArity
    -- ^ a row with more or fewer input cells than there are input columns.
    -- 'DMN.DecisionTable.matches' pairs them with 'zipWith', so a missing cell
    -- tests nothing (@symptom\/struct-short-row-truncated@).
  | CollectionColumn
    -- ^ an input column declared @[T]@. Its cells test membership, and a block
    -- would have to describe sets of collections. D-22 keeps dmnmd's overlap
    -- warning for these columns until regions model membership.
  | EmptyInputCell
    -- ^ an input cell holding no test at all, which 'fEvals' reads as matching
    -- nothing. The markdown reader turns an empty cell into @-@; this is here
    -- so that no other producer's empty list is silently a dead rule.
  | ComputedInputCell
    -- ^ arithmetic or a bare name in an input cell ('FFunction'), which 'fEval'
    -- cannot evaluate. Both readers already refuse it (@structuralErrors@), so
    -- this is reached only by a table built some other way.
  | FeelShapedStringCell
    -- ^ a String cell whose text dmnmd's own Number grammar
    -- ('DMN.ParseCell.parseNumberCell') reads as a comparison, an interval or a
    -- negation, or as arithmetic over one of the table's column names, or
    -- which begins @not(@. dmnmd matches such a cell as literal text, so a
    -- region would faithfully report a rule that can only fire on its own
    -- spelling (@symptom\/infer-explicit-type-contradiction-silent@,
    -- @symptom\/md-negation-in-string-column-silent@,
    -- @symptom\/md-string-col-arith-dead-rule@).
    --
    -- Deliberately NOT refused: a label 'parseNumberCell' does not read, such
    -- as @\<1 year@ or @Non-Participating@, even where
    -- 'DMN.ParseCell.unreadableTestShape' calls it comparison-shaped. dmnmd
    -- keeps those as text on purpose, and
    -- @policy\/md-test-shaped-label-declared-string-kept@ and
    -- @policy\/md-test-shaped-label-quoted-kept@ pin that. So is the
    -- dash-written range @[24 Sep 2026 -- 24 Oct 2026]@, which
    -- @unreadableTestShape@ leaves out by design
    -- (@symptom\/md-dash-written-interval-silent@). The cost of working on the
    -- built table: @unquoteCell@ has already removed any quotes, so a
    -- deliberately quoted @\"<= 8\"@ is refused too.
  | CellTypeMismatch
    -- ^ a cell or declared-domain member whose value is not of its column's
    -- type, or a String compared by @<@ or @>@. 'fEval' has no arm for either
    -- and would raise.
  | UncomparableAnyOutputs
    -- ^ an @A@ table with two rules that can both match, whose outputs differ
    -- as written and include arithmetic. Whether they agree depends on the
    -- input, which a region does not fix.
  deriving (Eq, Ord, Show)

unsupported :: UnsupportedKind -> String -> Either Unsupported a
unsupported k = Left . Unsupported k

tableAt :: DecisionTable -> String
tableAt dt = "table " ++ show (tableName dt) ++ ": "

hitPolicyOK :: DecisionTable -> Either Unsupported ()
hitPolicyOK dt = case hitpolicy dt of
  HP_Unique   -> Right ()
  HP_Any      -> Right ()
  HP_Priority -> Right ()
  HP_First    -> Right ()
  hp -> unsupported ListValuedHitPolicy $ tableAt dt ++ concat
    [ "hit policy ", showHitPolicy hp, " builds its answer from every matching"
    , " rule, so a region has no one rule to name. Regions cover U, A, P and F." ]

arityOK :: DecisionTable -> Int -> DTrow -> Either Unsupported ()
arityOK dt nIn r
  | n == nIn  = Right ()
  | otherwise = unsupported RowArity $ tableAt dt ++ concat
      [ maybe "" (\k -> "row " ++ show k ++ ": ") (row_number r)
      , "the row has ", show n, " input cell(s) for ", show nIn, " input column(s)."
      , " dmnmd's matcher pairs cells with columns by position and ignores the"
      , " difference, so a missing cell would match every value; regions will not"
      , " build that in." ]
  where n = length (row_inputs r)

-- | An @A@ table's conflict test compares outputs as written, which is what
-- 'evalTable' compares once arithmetic is evaluated. The two agree unless an
-- output is arithmetic and differs in spelling from one it might meet.
anyOutputsOK :: RegionMap -> Either Unsupported ()
anyOutputsOK rm = case hitpolicy dt of
  HP_Any -> case [ (i, j) | (i, oi) <- outs, (j, oj) <- outs, i < j, oi /= oj
                          , computed oi || computed oj, overlap i j ] of
    []           -> Right ()
    ((i, j) : _) -> unsupported UncomparableAnyOutputs $ tableAt dt ++ concat
      [ ruleLabel dt i, " and ", ruleLabel dt j, " can both match, and their"
      , " outputs differ as written and include arithmetic. Under hit policy"
      , " A (Any) they must agree, and whether they do depends on the input,"
      , " which a region does not fix." ]
  _ -> Right ()
  where
    dt   = rmTable rm
    outs = zip [0 ..] (row_outputs <$> allrows dt)
    computed = any (any (\case FFunction _ -> True; _ -> False))
    -- Regions are the whole product of blocks, so two rules share a region
    -- exactly when every column has a block admitting both.
    overlap i j = all (any (\b -> IS.member i (blockMembers b) && IS.member j (blockMembers b)) . cbBlocks)
                      (rmColumns rm)

ruleLabel :: DecisionTable -> RuleIx -> String
ruleLabel dt i = case drop i (allrows dt) of
  (r : _) | Just n <- row_number r -> "row " ++ show n
  _                                -> "the unnumbered row at position " ++ show (i + 1)

-- * Blocks

-- | One input column, cut into blocks.
data ColumnBlocks = ColumnBlocks
  { cbHeader :: ColHeader
  , cbBlocks :: [Block]
    -- ^ disjoint, and together the column's whole domain: the real line for a
    -- Number column, every string for a String one, @true@ and @false@ for a
    -- Boolean one, cut down to the declared domain when the sub-header row (or
    -- @\<inputValues\>@) declares one. Number blocks are in ascending order.
    -- String blocks are in order of their first value: the declared domain's
    -- members, then the literals as the cells mention them, then every other
    -- String. Boolean blocks put @true@ before @false@.
  }
  deriving (Eq, Show)

-- | A set of values of one column on which every cell of the column gives one
-- answer, and no neighbouring block gives the same one.
data Block = Block
  { blockValues  :: BlockValues
  , blockMembers :: IntSet
    -- ^ the rules whose cell in this column admits every value of the block
  , blockRep     :: DMNVal
    -- ^ one value in the block. For a Number block, the lowest value in it that
    -- some cell or domain member names, if there is one; else an integer inside
    -- it, if there is one; else its midpoint, computed exactly. For a String
    -- block of every other value, a string no cell or domain member names
    -- (@other@, or @other 2@, …).
  }
  deriving (Eq, Show)

-- | 'blockMembers' as a list.
blockRules :: Block -> [RuleIx]
blockRules = IS.toAscList . blockMembers

-- | Which values a block holds.
data BlockValues
  = Numbers [Interval]
    -- ^ ascending and disjoint. A single interval, unless a declared domain
    -- leaves gaps: then values the domain separates, and no rule tells apart,
    -- are one block of several intervals.
  | Values [DMNVal]
    -- ^ exactly these
  | AllExcept [DMNVal]
    -- ^ every String except these
  deriving (Eq, Show)

-- | An interval of the number line. 'Nothing' is unbounded on that side; a
-- single value is two equal 'BClosed' ends. Endpoints are 'Scientific', exact.
data Interval = Interval
  { ivLower :: Maybe (Bound, Scientific)
  , ivUpper :: Maybe (Bound, Scientific)
  }
  deriving (Eq, Show)

-- | A block as the S-FEEL unary test that selects it, as a table cell would
-- say it: @-@, @< 18@, @[18..21]@, @(20..30)@, @30@, @\"Fall\", \"Winter\"@,
-- @not(\"Fall\")@, @true@. Numbers are spelled by 'showNumPlain'. The test suite
-- reads every Number block's spelling back through dmnmd's own cell reader.
showBlock :: Block -> String
showBlock b = case blockValues b of
  Numbers ivs   -> intercalate ", " (showInterval <$> ivs)
  Values vs     -> intercalate ", " (showValue <$> vs)
  AllExcept []  -> "-"
  AllExcept vs  -> "not(" ++ intercalate ", " (showValue <$> vs) ++ ")"

showInterval :: Interval -> String
showInterval = \case
  Interval Nothing Nothing                 -> "-"
  Interval (Just (BClosed, a)) (Just (BClosed, z)) | a == z -> showNumPlain a
  Interval Nothing (Just (BOpen, z))       -> "< "  ++ showNumPlain z
  Interval Nothing (Just (BClosed, z))     -> "<= " ++ showNumPlain z
  Interval (Just (BOpen, a)) Nothing       -> "> "  ++ showNumPlain a
  Interval (Just (BClosed, a)) Nothing     -> ">= " ++ showNumPlain a
  Interval (Just (lb, a)) (Just (ub, z))   ->
    (if lb == BOpen then "(" else "[") ++ showNumPlain a ++ ".." ++ showNumPlain z
      ++ (if ub == BOpen then ")" else "]")

showValue :: DMNVal -> String
showValue = \case
  VS s     -> "\"" ++ s ++ "\""
  VN n     -> showNumPlain n
  VB True  -> "true"
  VB False -> "false"
  VL vs    -> "[" ++ intercalate ", " (showValue <$> vs) ++ "]"

-- | What a column's values are, for cutting.
data ValueType = VNum | VStr | VBool
  deriving (Eq, Show)

-- | An elementary piece: the finest cut of a column's domain. Every cell in
-- the column is constant on a piece, because the cuts are every endpoint and
-- literal any cell or domain member mentions.
data Piece
  = NumPiece Int Interval Scientific
    -- ^ its position among the pieces, the piece, and a value inside it
  | ValPiece DMNVal
  | OtherPiece DMNVal
    -- ^ every String no cell or domain member names, and one of them

pieceRep :: Piece -> DMNVal
pieceRep = \case
  NumPiece _ _ v -> VN v
  ValPiece v     -> v
  OtherPiece v   -> v

buildColumn :: DecisionTable -> (ColHeader, [[FEELexp]]) -> Either Unsupported ColumnBlocks
buildColumn dt (ch, cells) = do
  vt <- valueType dt ch
  mapM_ (\(r, cell) -> cellOK dt ch vt (row_number r) cell) (zip (allrows dt) cells)
  mapM_ (mapM_ (testOK dt vt (siteOf Nothing) "a member of the declared domain")) (enums ch)
  let domain  = enums ch
      pieces  = filter (\p -> maybe True (fEvals (FNullary (pieceRep p))) domain)
                       (piecesOf vt (concat domain ++ concat cells))
      admits p = IS.fromList [ i | (i, cell) <- zip [0 ..] cells, fEvals (FNullary (pieceRep p)) cell ]
      scored  = [ (p, admits p) | p <- pieces ]
  pure ColumnBlocks
    { cbHeader = ch
    , cbBlocks = if vt == VNum then mergeOrdered scored else mergeUnordered scored }
  where
    siteOf = showSite . CellSite (tableName dt) (varname ch)

valueType :: DecisionTable -> ColHeader -> Either Unsupported ValueType
valueType dt ch = case vartype ch of
  Just DMN_Number     -> Right VNum
  Just DMN_Boolean    -> Right VBool
  Just DMN_String     -> Right VStr
  -- 'DMN.DecisionTable.mkFEither' reads every cell of an untyped column as a string.
  Nothing             -> Right VStr
  Just t@(DMN_List _) -> unsupported CollectionColumn $ showSite (CellSite (tableName dt) (varname ch) Nothing)
    ++ concat
      [ "the column is a collection (", showType t, "), so its cells test"
      , " membership, and a region would have to describe which values a"
      , " collection holds. D-22 keeps dmnmd's overlap warning for collection"
      , " columns until regions model membership." ]

cellOK :: DecisionTable -> ColHeader -> ValueType -> Maybe Int -> [FEELexp] -> Either Unsupported ()
cellOK dt ch vt rn = \case
  [] -> unsupported EmptyInputCell $ site ++
          "the input cell holds no test at all, which dmnmd's matcher reads as"
          ++ " matching nothing. Write \"-\" to match everything."
  es -> mapM_ (testOK dt vt site "the input cell") es
  where site = showSite (CellSite (tableName dt) (varname ch) rn)

-- | One test, from a cell or a declared domain: is it one 'fEval' can decide
-- for every value of the column's type, and one it decides as the author meant?
testOK :: DecisionTable -> ValueType -> String -> String -> FEELexp -> Either Unsupported ()
testOK dt vt site what e = case (vt, e) of
  (_, FAnything)             -> Right ()
  (_, FNot inner)            -> testOK dt vt site what inner
  (_, FFunction _)           -> unsupported ComputedInputCell $ site ++ concat
    [ what, " reads ", show (showDomainMember e), ", which is computed rather"
    , " than tested; dmnmd's matcher has no way to evaluate it against a value." ]
  (VNum, FSection _ (VN _))  -> Right ()
  (VNum, FInRange {})        -> Right ()
  (VNum, FNullary (VN _))    -> Right ()
  (VStr, FNullary (VS s))    -> stringOK s
  (VStr, FSection Feq (VS s)) -> stringOK s
  (VBool, FNullary (VB _))   -> Right ()
  (VBool, FSection Feq (VB _)) -> Right ()
  _ -> unsupported CellTypeMismatch $ site ++ concat
    [ what, " reads ", show (showDomainMember e), ", which is not a test"
    , " dmnmd's matcher can apply to a ", show vt, " value." ]
  where
    stringOK s
      | feelShaped (varname <$> header dt) s = unsupported FeelShapedStringCell $ site ++ concat
          [ what, " reads ", show s, ", which dmnmd's own grammar reads as a FEEL"
          , " test or computation, but a String cell is matched as literal text,"
          , " so this rule can fire only on an input spelled exactly that way."
          , " If it is a test, make the column a Number column; regions will not"
          , " describe it as text." ]
      | otherwise = Right ()

-- | Does this String cell hold FEEL test syntax that dmnmd reads as text?
-- See 'FeelShapedStringCell' for what is caught and what is deliberately not.
feelShaped :: [String] -> String -> Bool
feelShaped names raw
  | "not(" `isPrefixOf` squeezed = True
  | otherwise = case parseNumberCell s of
      Right FSection {}            -> True
      Right FInRange {}            -> True
      Right FNot {}                -> True
      Right (FFunction f@FNF3 {})  -> any (`elem` names) (fnVars f)
      _                            -> False
  where
    s        = trim raw
    squeezed = filter (not . isSpace) (take 5 s)

-- | The elementary pieces of a column, from every test in its cells and domain.
piecesOf :: ValueType -> [FEELexp] -> [Piece]
piecesOf vt tests = case vt of
  VNum  -> numberPieces (nub (sortNums (concatMap numPoints tests)))
  VBool -> [ValPiece (VB True), ValPiece (VB False)]
  VStr  -> let lits = nub (concatMap literals tests)
           in (ValPiece <$> lits) ++ [OtherPiece (VS (fresh lits))]
  where
    sortNums = foldr insertNum []
    insertNum x ys = let (lo, hi) = span (< x) ys in lo ++ x : hi
    fresh taken = case [ c | c <- "other" : [ "other " ++ show n | n <- [2 :: Int ..] ]
                           , VS c `notElem` taken ] of
      (c : _) -> c
      []      -> "other"   -- unreachable: the candidates are infinite, the literals finite

numPoints :: FEELexp -> [Scientific]
numPoints = \case
  FSection _ (VN v) -> [v]
  FInRange _ a z _  -> [a, z]
  FNullary (VN v)   -> [v]
  FNot e            -> numPoints e
  _                 -> []

literals :: FEELexp -> [DMNVal]
literals = \case
  FNullary v@(VS _)     -> [v]
  FSection Feq v@(VS _) -> [v]
  FNot e                -> literals e
  _                     -> []

-- | The number line cut at each point: below the first, each point, each gap
-- between neighbours, and above the last. Given ascending distinct points.
numberPieces :: [Scientific] -> [Piece]
numberPieces [] = [NumPiece 0 (Interval Nothing Nothing) 0]
numberPieces pts@(p0 : _) =
  zipWith (\k (iv, v) -> NumPiece k iv v) [0 ..]
    ((Interval Nothing (Just (BOpen, p0)), below p0) : go pts)
  where
    go []           = []
    go (p : rest)   = (Interval (Just (BClosed, p)) (Just (BClosed, p)), p) : case rest of
      []      -> [(Interval (Just (BOpen, p)) Nothing, above p)]
      (q : _) -> (Interval (Just (BOpen, p)) (Just (BOpen, q)), between p q) : go rest
    -- the largest integer strictly below, and the smallest strictly above
    below z = fromInteger (ceiling z - 1)
    above a = fromInteger (floor a + 1)
    -- an integer if one fits, else the midpoint, which is exact: halving a
    -- terminating decimal terminates, and multiplying by 0.5 never divides
    between a z
      | above a < z = above a
      | otherwise   = (a + z) * 0.5

-- | Number pieces with equal member sets merge when adjacent among the pieces
-- the domain keeps, so a block is an interval (or, across a domain gap, a run
-- of them).
mergeOrdered :: [(Piece, IntSet)] -> [Block]
mergeOrdered scored =
  [ Block { blockValues  = Numbers (coalesce [ (k, iv) | NumPiece k iv _ <- ps ])
          , blockMembers = snd (NE.head grp)
          , blockRep     = maybe (pieceRep (NE.head (fst <$> grp))) pieceRep (find isPoint ps) }
  | grp <- NE.groupBy ((==) `on` snd) scored
  , let ps = fst <$> NE.toList grp ]
  where
    isPoint = \case
      NumPiece _ (Interval (Just (BClosed, a)) (Just (BClosed, z))) _ -> a == z
      _                                                            -> False
    coalesce = \case
      []                -> []
      ((k0, iv0) : more) -> go k0 iv0 more
    go _ cur [] = [cur]
    go prev cur ((k, iv) : more)
      | k == prev + 1 = go k (Interval (ivLower cur) (ivUpper iv)) more
      | otherwise     = cur : go k iv more

-- | String and Boolean pieces with equal member sets merge wherever they are,
-- since these columns have no order to keep.
mergeUnordered :: [(Piece, IntSet)] -> [Block]
mergeUnordered scored =
  [ Block { blockValues  = if any isOther ps then AllExcept (filter (`notElem` vals) allVals) else Values vals
          , blockMembers = key
          , blockRep     = maybe (VS "") pieceRep (find isOther ps <|> listToMaybe ps) }
  | key <- nub (snd <$> scored)
  , let ps   = [ p | (p, m) <- scored, m == key ]
        vals = [ v | ValPiece v <- ps ] ]
  where
    allVals = [ v | (ValPiece v, _) <- scored ]
    isOther = \case OtherPiece _ -> True; _ -> False

-- * Regions

-- | One block per input column, and the rules live throughout.
data Region = Region
  { regionBlocks :: [Block]
    -- ^ one per input column, in header order
  , regionLive   :: IntSet
  }
  deriving (Eq, Show)

-- | The rules that match every input in the region, ascending. Every rule
-- matches either all of a region or none of it.
liveRules :: Region -> [RuleIx]
liveRules = IS.toAscList . regionLive

-- | A concrete input in the region, one value per input column, in the shape
-- 'DMN.DecisionTable.evalTable' takes.
regionInput :: Region -> [FEELexp]
regionInput = fmap (FNullary . blockRep) . regionBlocks

-- | Every region, in lexicographic order of blocks, first column outermost.
-- Lazy, and 'regionCount' long; a table with no input column has one region.
regions :: RegionMap -> [Region]
regions = walk (\_ _ -> True) (const True)

-- | How many regions there are, without enumerating them.
regionCount :: RegionMap -> Integer
regionCount rm = product [ toInteger (length (cbBlocks c)) | c <- rmColumns rm ]

-- | A region where the table's hit policy has no single answer: under @U@, two
-- or more live rules, not counting a 'TrailingCatchAll'; under @A@, live rules
-- whose outputs differ. @P@ and @F@ choose among live rules, so never.
isConflict :: RegionMap -> Region -> Bool
isConflict rm = conflicting rm . regionLive

-- | A region no rule matches and no default answers.
isNoMatch :: RegionMap -> Region -> Bool
isNoMatch rm = unanswered rm . regionLive

-- | @filter (isConflict rm) (regions rm)@, without visiting the regions that
-- cannot conflict.
conflictRegions :: RegionMap -> [Region]
conflictRegions rm = walk (\_ live -> conflicting rm live) (conflicting rm) rm

-- | @filter (isNoMatch rm) (regions rm)@, without visiting the regions under a
-- rule that admits every block of every column still to be chosen.
noMatchRegions :: RegionMap -> [Region]
noMatchRegions rm = walk viable (unanswered rm) rm
  where viable coverRest live = rmDefault rm == NoDefault && IS.null (IS.intersection live coverRest)

conflicting :: RegionMap -> IntSet -> Bool
conflicting rm live = case hitpolicy (rmTable rm) of
  HP_Unique -> IS.size answering >= 2
  HP_Any    -> length (nub (mapMaybe outputOf (IS.toList live))) >= 2
  _         -> False
  where
    answering = case rmDefault rm of
      TrailingCatchAll k -> IS.delete k live
      _                  -> live
    outputOf i = row_outputs <$> case drop i (allrows (rmTable rm)) of
      (r : _) -> Just r
      []      -> Nothing

unanswered :: RegionMap -> IntSet -> Bool
unanswered rm live = rmDefault rm == NoDefault && IS.null live

-- | Depth first over the columns, carrying the live set. Before descending into
-- a column, @viable coverRest live@ may prune: @coverRest@ is the rules that
-- admit every block of this column and every later one. It must be False only
-- where no region below can satisfy @wanted@. The live set only shrinks on the
-- way down, which is what makes the two pruning predicates above sound.
walk :: (IntSet -> IntSet -> Bool) -> (IntSet -> Bool) -> RegionMap -> [Region]
walk viable wanted rm = go everyRule (zip cols covers) []
  where
    cols      = rmColumns rm
    everyRule = IS.fromList [0 .. length (allrows (rmTable rm)) - 1]
    admitsAll c = foldr (IS.intersection . blockMembers) everyRule (cbBlocks c)
    -- per column, the rules admitting every block of it and of every later
    -- column; scanr's trailing seed has no column, and the zip above drops it
    covers    = scanr (IS.intersection . admitsAll) everyRule cols
    go live [] acc = [ Region (reverse acc) live | wanted live ]
    go live ((c, coverRest) : more) acc
      | viable coverRest live =
          concat [ go (IS.intersection live (blockMembers b)) more (b : acc) | b <- cbBlocks c ]
      | otherwise = []

-- * Refusing conflicts (D-22 rule 2)

-- | Two rules that can both match where the hit policy allows only one answer:
-- under @U@ two rules, neither of them a 'TrailingCatchAll'; under @A@ two
-- rules whose outputs differ as written, which is what 'isConflict' compares.
-- Never under @P@ or @F@.
data Conflict = Conflict
  { conflictEarlier :: RuleIx
  , conflictLater   :: RuleIx
  , conflictRegion  :: Region
    -- ^ the first region, in 'regions' order, in which both are live. Its
    -- 'regionInput' is the witness a refusal names.
  }
  deriving (Eq, Show)

-- | For each rule that conflicts with an earlier one, the FIRST such earlier
-- rule, with the first region in which both are live. So a table has a
-- 'Conflict' exactly when it has a conflict region, and @n@ rules that all
-- overlap give @n - 1@ of them, each naming the first: the shape D-13's
-- 'DMN.DecisionTable.identicalGuards' already has, so the two refusals count
-- rows the same way.
--
-- __Pairwise, not by enumerating regions.__ Two rules share a region exactly
-- when every column has a block admitting both, and the first such region in
-- 'regions' order takes, in each column, the first such block. That is one
-- pass over the blocks per pair of rules. Enumerating the conflict regions
-- instead can cost the product of the columns' block counts, and this runs on
-- every table either reader accepts.
conflicts :: RegionMap -> [Conflict]
conflicts rm =
  [ Conflict i j (Region shared (foldr (IS.intersection . blockMembers) everyRule shared))
  | j <- ixs
  , (i, shared) <- take 1 [ (i, bs) | i <- takeWhile (< j) ixs, clash i j, Just bs <- [sharedBlocks i j] ]
  ]
  where
    dt        = rmTable rm
    outs      = row_outputs <$> allrows dt
    ixs       = [0 .. length outs - 1]
    everyRule = IS.fromList ixs
    answering k = rmDefault rm /= TrailingCatchAll k
    clash i j = case hitpolicy dt of
      HP_Unique -> answering i && answering j
      HP_Any    -> outs !! i /= outs !! j
      _         -> False
    sharedBlocks i j = traverse (find (admitsBoth i j) . cbBlocks) (rmColumns rm)

admitsBoth :: RuleIx -> RuleIx -> Block -> Bool
admitsBoth i j b = IS.member i (blockMembers b) && IS.member j (blockMembers b)

-- | D-22 rule 2: every conflict region on scalar columns, as a refusal.
-- A summand of 'DMN.BuildTable.tableErrors', beside D-13's
-- 'DMN.DecisionTable.uniquenessErrors', so both readers refuse the same
-- tables.
--
-- One message per 'Conflict', naming both rules by the numbers the author wrote,
-- one input both match (the representative of the first region they share),
-- and, where it is larger than that one input, the whole of their overlap,
-- column by column. Under @A@ it also names the output columns that disagree.
--
-- __Silent in two cases, both deliberate.__
--
-- * Where 'regionMap' cannot compute regions: a list-valued hit policy, a
--   collection column (whose @U@ and @A@ tables keep the overlap warning in
--   'DMN.DecisionTable.tableWarnings', as D-22 says, until regions model
--   membership), a String cell holding FEEL test syntax, a short row, a
--   computed cell. Such a table is refused or accepted exactly as it was
--   before D-22 part 2; not being able to analyse a table is not a reason to
--   refuse it.
-- * About a row 'DMN.DecisionTable.identicalGuards' reports. D-13's message
--   already refuses it, and says two things this one does not: that the row
--   can never match at all, and that 1.1 and 1.10 are the same guard. Two
--   messages about one row would be noise.
conflictErrors :: DecisionTable -> [String]
conflictErrors dt = case regionMap dt of
  Left _   -> []
  Right rm -> [ conflictMessage rm c | c <- conflicts rm, conflictLater c `notElem` reportedByD13 ]
  where reportedByD13 = snd <$> identicalGuards dt

conflictMessage :: RegionMap -> Conflict -> String
conflictMessage rm c = case hitpolicy dt of
  HP_Any -> concat
    [ li, " and ", lj, " both match ", whereText, " and disagree on ", disagreement
    , ": under hit policy A (Any), rules may overlap only where their outputs agree"
    , " (DMN 1.3 §8.2.10). Give the two rules the same outputs, or change an input"
    , " cell of ", li, " or ", lj, " so that they select different inputs." ]
  _ -> concat
    [ li, " and ", lj, " both match ", whereText
    , ": a table with hit policy Unique must not contain overlapping rules"
    , " (DMN 1.3 §8.2.10). Change an input cell of ", li, " or ", lj, " so that the"
    , " two rules select different inputs or, if the earlier rule is meant to win,"
    , " make the hit policy F (First)." ]
  where
    dt = rmTable rm
    i  = conflictEarlier c
    j  = conflictLater c
    li = ruleLabel dt i
    lj = ruleLabel dt j

    witness = intercalate ", "
      [ varname (cbHeader col) ++ " = " ++ showValue (blockRep b)
      | (col, b) <- zip (rmColumns rm) (regionBlocks (conflictRegion c)) ]
    overlaps = overlapIn i j <$> rmColumns rm
    whereText
      | all (== Nothing) overlaps             = "every input"
      | all (maybe False fst) overlaps        = witness
      | otherwise = witness ++ " (they overlap wherever "
                      ++ intercalate " and " [ d | Just (_, d) <- overlaps ] ++ ")"

    disagreement = case [ varname ch ++ " (" ++ showCell a ++ " against " ++ showCell b ++ ")"
                        | (ch, a, b) <- zip3 (getOutputHeaders (header dt)) (outsOf i) (outsOf j)
                        , a /= b ] of
      [] -> "their outputs"
      ds -> intercalate " and " ds
    outsOf k = maybe [] row_outputs (listToMaybe (drop k (allrows dt)))
    showCell = intercalate ", " . fmap (\case FNullary v -> showValue v; e -> showDomainMember e)

-- | Where two rules overlap in one column, as a test on that column: 'Nothing'
-- if everywhere (they do not narrow each other there), else whether it is a
-- single value, and the test. The union of the blocks admitting both is
-- exactly the intersection of the two cells within the column's domain,
-- because the blocks partition the domain and each cell admits a block whole.
overlapIn :: RuleIx -> RuleIx -> ColumnBlocks -> Maybe (Bool, String)
overlapIn i j col
  | length shared == length (cbBlocks col) = Nothing
  | otherwise = Just $ case concatMap (numbersOf . blockValues) shared of
      [] -> strings
      ivs -> case coalesceIntervals ivs of
        [iv@(Interval (Just (BClosed, a)) (Just (BClosed, z)))] | a == z -> (True, name ++ " = " ++ showInterval iv)
        [iv]  -> (False, name ++ oneInterval iv)
        ivs'  -> (False, name ++ " matches " ++ intercalate ", " (showInterval <$> ivs'))
  where
    name    = varname (cbHeader col)
    shared  = filter (admitsBoth i j) (cbBlocks col)
    numbersOf = \case Numbers ivs -> ivs; _ -> []
    -- A String block of every other value is in the union exactly when the
    -- union is everything except the literals of the blocks left out.
    strings
      | any isAllExcept shared = case concat [ vs | b <- cbBlocks col, not (admitsBoth i j b), Values vs <- [blockValues b] ] of
          [v] -> (False, name ++ " is not " ++ showValue v)
          vs  -> (False, name ++ " is none of " ++ intercalate ", " (showValue <$> vs))
      | otherwise = case concat [ vs | Values vs <- blockValues <$> shared ] of
          [v] -> (True, name ++ " = " ++ showValue v)
          vs  -> (False, name ++ " is one of " ++ intercalate ", " (showValue <$> vs))
    isAllExcept b = case blockValues b of AllExcept _ -> True; _ -> False
    oneInterval = \case
      Interval Nothing (Just (BOpen, z))   -> " < "  ++ showNumPlain z
      Interval Nothing (Just (BClosed, z)) -> " <= " ++ showNumPlain z
      Interval (Just (BOpen, a)) Nothing   -> " > "  ++ showNumPlain a
      Interval (Just (BClosed, a)) Nothing -> " >= " ++ showNumPlain a
      iv                                   -> " is in " ++ showInterval iv

-- | Join intervals that meet: one ends at a value the next begins at, and
-- exactly one of them includes it. Given them in ascending order, as the
-- blocks of a Number column are.
coalesceIntervals :: [Interval] -> [Interval]
coalesceIntervals = \case
  (a : b : rest)
    | Just (ub, x) <- ivUpper a, Just (lb, y) <- ivLower b, x == y, ub /= lb
    -> coalesceIntervals (Interval (ivLower a) (ivUpper b) : rest)
  (a : rest) -> a : coalesceIntervals rest
  []         -> []
