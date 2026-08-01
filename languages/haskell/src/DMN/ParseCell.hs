{-# LANGUAGE OverloadedStrings, LambdaCase #-}

-- | The cell language of a @Number@ column, written down once, as a grammar.
--
-- Rule numbers throughout are DMN 1.3 __§9.2__ (the S-FEEL subset, rules 1-38)
-- — the numbering "DMN.DecisionTable" already cites. NOT §10.3.1.2's full-FEEL
-- numbering, which is a different sequence.
--
-- This module is the only cell grammar in the tree. There was a second,
-- @DMN.SFeelGrammar@, which transcribed the same rule numbers and was imported
-- by nothing but its own test; @DECISIONS.md@ D-14 deleted it after measuring
-- that its parsers did not implement the EBNF its comments quoted.
--
-- Scope, deliberately narrow. This module replaces the @Just DMN_Number@ arm of
-- 'DMN.DecisionTable.mkFEither' and nothing else. The @String@, @Boolean@ and
-- untyped arms keep their existing, total behaviour, because a String cell is
-- whatever the author wrote (@5' 10\"@, @Non-Participating@, @Coming soon...@)
-- and running this grammar over one would refuse enormous amounts of input that
-- works today. Type inference is likewise untouched: it still decides which arm
-- a cell reaches, and it still decides wrongly in the cases recorded under
-- @test\/corpus\/cases\/symptom\/infer-*@.
--
-- What this fixes is that the arm it replaces was not a grammar but a chain of
-- six mutually blind string tests over the same raw text, so the accepted
-- language was whatever fell out of the ordering:
--
--  * @intersect \"+-*\/\"@ sat first, and §9.2 rule 27 puts @. \/ - ’ + *@
--    inside legal FEEL /names/ — so no character-set test can ever separate
--    arithmetic from an endpoint or an identifier;
--  * the interval recogniser was an /unanchored/ PCRE search, so @not([1..5])@
--    matched @[1..5]@ and the negation was discarded;
--  * the fallthrough was Haskell's @read@, whose grammar accepts @0x10@, @0o17@,
--    @1e5@, @Infinity@ and @NaN@ and rejects @.5@ — wrong in both directions.
module DMN.ParseCell
  ( parseNumberCell
  , numericLiteral
  , thousandsGrouped
  , namedRefusal
  ) where

import           Data.Char            (isAlpha)
import           Data.List            (isPrefixOf)
import           Data.Scientific       (Scientific)
import qualified Data.Text as T
import           Text.Megaparsec
import           Text.Megaparsec.Char (char, digitChar, string)

import           DMN.ParseFEEL        (parseFNumFunction)
import           DMN.ParsingUtils     (Parser, skipHorizontalSpace)
import           DMN.Types

-- | One comma-fragment of a cell in a @Number@ column.
--
-- The cell has already been split on commas by
-- 'DMN.DecisionTable.mkFsEither' (§9.2 rule 11's disjunction) and stripped of
-- surrounding whitespace, so what arrives here is one rule-5
-- @simple positive unary test@ — or, in an output column, one rule-3
-- @simple expression@. dmnmd cannot tell those apart at this point and does not
-- try to: it accepts the union, and refuses arithmetic in an /input/ position
-- later, in 'DMN.DecisionTable.structuralErrors', where the row number and
-- column name are in scope and the diagnostic can be located.
--
-- Recognisers are tried in __specificity order__, and arithmetic is __last__.
parseNumberCell :: String -> Either String FEELexp
parseNumberCell raw
  -- Named refusals FIRST, so that a construct dmnmd does not implement is
  -- refused by name rather than producing a megaparsec column-2 complaint about
  -- the second character of @floor@.
  -- §9.2 rule 12.b, @\"not\", \"(\", simple positive unary tests, \")\"@ (D-9).
  -- The inner text is parsed as a 'numericCell' rather than by recursing through
  -- 'parseNumberCell', and that is what makes the test POSITIVE: @not(not(x))@
  -- and @not(floor(x))@ do not parse and are refused, because neither a nested
  -- negation nor an invocation is a 'numericCell'.
  | Just inner <- peelNot cell =
      case runAnchored numericCell inner of
        -- Arithmetic is a rule 3 simple expression, not a unary test, so it has
        -- no business inside a negation even though 'numericCell' accepts the
        -- union elsewhere. Refused here rather than downstream: 'inputArithErrs'
        -- matches on a bare @FFunction@ cell and would not see one wrapped in
        -- 'FNot'.
        Right (FFunction _) -> Left (negationOfArithMsg cell inner)
        Right fx            -> Right (FNot fx)
        Left _              -> Left (negationMsg cell inner)
  -- A cell that LOOKS like a call to a function named @not@ is really a rule
  -- 12.b negation that the comma split has torn in half — @not(a, b)@ arrives
  -- here as the fragment @not(a@, so 'peelNot' finds no closing paren and falls
  -- through. Answering with the invocation message would tell the author that
  -- dmnmd does not implement @not()@, which since D-9 is false and sends them
  -- looking for the wrong repair.
  | Just "not" <- callName cell = Left (splitNegationMsg cell)
  | Just fn    <- callName cell = Left (invocationMsg cell fn)
  | otherwise = case runAnchored numericCell cell of
      Right fx -> Right fx
      Left _   -> Left (notATestMsg cell)
  where cell = trim' raw

-- | Run a parser over the whole fragment. The @eof@ is the point: it is what
-- makes @[1..5] [10..20]@ a refusal instead of a silently dropped second
-- interval, and @0x10@ a refusal instead of 16.
runAnchored :: Parser a -> String -> Either String a
runAnchored p s = case parse (sp *> p <* sp <* eof) "cell" (T.pack s) of
  Right a -> Right a
  Left e  -> Left (show e)

sp :: Parser ()
sp = skipHorizontalSpace

lx :: Parser a -> Parser a
lx p = p <* sp

-- | The union of §9.2 rule 5 (a numeric unary test) and rule 3 (a simple
-- expression), which is what a @Number@ column's cell may hold in dmnmd.
numericCell :: Parser FEELexp
numericCell = choice
  [ try interval                    -- unambiguous: starts with [ ( ]
  , try prefixCmp                   -- unambiguous: starts with an operator
  , try suffixCmp                   -- endpoint THEN operator
  -- 'arithmetic' MUST come before 'bareEndpoint', and both must be inside 'try'.
  -- The other order makes @40 - 50@ a spurious refusal: 'bareEndpoint' SUCCEEDS
  -- on the leading @40@, 'choice' commits, and the trailing @- 50@ then fails the
  -- outer @eof@ with no way back. Found by running the corpus against a draft of
  -- this module, not by reading it.
  , try arithmetic                  -- rule 3, restricted to an FNF3
  , bareEndpoint                    -- rule 5's operator is OPTIONAL
  ]

-- ** Rules 6-10: intervals

-- | Rules 6-10. __Nine__ surface spellings for __four__ semantics: the left
-- bracket is one of @[ ( ]@ (rules 7, 8) and the right one of @] ) [@ (rules 9,
-- 10), freely combined, because @]a..b[@ is the ISO 31-11 synonym for
-- @(a..b)@. Position disambiguates the two characters that appear on both
-- sides, so no backtracking is needed inside this parser.
--
-- The old recogniser hard-coded @[@ … @]@ and @(\\d+)@ for both bounds, so five
-- of the nine spellings were a parse error, the surviving four all meant
-- closed-closed, and @[-5..5]@ and @[1.5..2.5]@ were unrepresentable.
interval :: Parser FEELexp
interval = do
  lo <- lx intervalStart
  a  <- lx numericLiteral
  _  <- string ".."
  -- @notFollowedBy (char '.')@ is load-bearing and was found by an adversarial
  -- read of an earlier draft. Without it @[10...20]@ parses: @..@ is consumed
  -- and rule 31's leading-dot alternative then reads the third dot as the start
  -- of @.20@, yielding @FInRange 10.0 0.2@ — a permanently dead row, emitted
  -- silently at exit 0, and in a declared domain a @{[0.0..0.15]}@ that refuses
  -- valid cells. Three dots is a typo and must be refused.
  _  <- notFollowedBy (char '.')
  sp
  b  <- lx numericLiteral
  hi <- intervalEnd
  pure (FInRange lo a b hi)

intervalStart :: Parser Bound
intervalStart = choice
  [ BClosed <$ char '['        -- rule 8
  , BOpen   <$ char '('        -- rule 7
  , BOpen   <$ char ']'        -- rule 7, the ISO 31-11 spelling
  ]

intervalEnd :: Parser Bound
intervalEnd = choice
  [ BClosed <$ char ']'        -- rule 10
  , BOpen   <$ char ')'        -- rule 9
  , BOpen   <$ char '['        -- rule 9, the ISO 31-11 spelling
  ]

-- ** Rule 5: comparisons

-- | Longest match first. The deleted @DMN.SFeelGrammar@ had these the other way
-- round (@choice [Lt \<$ \"\<\", Le \<$ \"\<=\", …]@), which is why @\<= 8@ parsed
-- there as @\<@ and then died on the @=@ — a defect this comment recorded three
-- days before D-10 called that module faithful, and which D-14 cites.
--
-- @=@ is not in rule 5; it is a pre-existing dmnmd spelling for equality, kept
-- because it already shipped and @struct-outputorder-enum-honoured@ pins the
-- unspaced @\<18@ form alongside it.
cmpOp :: Parser FBinOp
cmpOp = choice
  [ Flte <$ string "<=", Fgte <$ string ">="
  , Flt  <$ string "<",  Fgt  <$ string ">"
  , Feq  <$ string "="
  ]

prefixCmp :: Parser FEELexp
prefixCmp = FSection <$> lx cmpOp <*> (VN <$> numericLiteral)

-- | dmnmd's suffix comparison, @5 \<=@, read as an operator __section__ over the
-- elided input: @5 \<= ?@, i.e. @input >= 5@. That is the __mirror__ of the
-- prefix form, and it is what 'DMN.DecisionTable.fEval'\'s own comment always
-- claimed ("we also deal with @\< x@ vs @x \<@ -- we canonicalize order to
-- @op val@"). What shipped was the __negation__ of the mirror, which is how
-- @5 \<=@ came to exclude 5 and how @5 >@ came to have no arm at all.
--
-- __Not S-FEEL, and documented as such.__ Rule 5's operator slot is prefix-only
-- and has no mirror production; rule 35's infix comparison needs /both/ operands
-- and is reachable only from rule 3, never from an input entry; and l4-ide's
-- independently written @IR.hs@ has no suffix constructor and no suffix
-- rendering. It is kept, mirrored, and named in README.md as a non-conformant
-- extension, because refusing it would leave three exit-0 wrong answers as
-- exit-1 refusals with no way to say what the author meant.
suffixCmp :: Parser FEELexp
suffixCmp = do
  v  <- lx numericLiteral
  op <- mirror <$> cmpOp
  pure (FSection op (VN v))
  where
    mirror = \case
      Flte -> Fgte; Flt -> Fgt
      Fgte -> Flte; Fgt -> Flt
      Feq  -> Feq

bareEndpoint :: Parser FEELexp
bareEndpoint = FNullary . VN <$> numericLiteral

-- ** Rule 31: the numeric literal

-- | Rule 31, exactly:
--
-- > numeric literal = [ "-" ] , ( digits , [ "." , digits ] | "." , digits ) ;
--
-- with rules 32-33 @digit = [0-9]@ and @digits = digit , {digit}@. That is the
-- entire production: no exponent, no radix prefix, no underscore, no percent,
-- no @+@, no trailing bare @.@, and no @Infinity@ or @NaN@.
--
-- Leading zeros are __legal__ (rule 33 forbids nothing), so @000@ is 0. Not an
-- oversight: it is exactly why @1,000@ is two well-formed unary tests rather
-- than a misparse, and hence why 'thousandsGrouped' has to exist.
--
-- The @try@ around the fraction is load-bearing. Without it @[1..5]@ cannot
-- parse: @some digitChar@ takes the @1@, @char '.'@ takes the first dot of
-- @..@, @some digitChar@ then fails having consumed input, and megaparsec does
-- not backtrack over consumed input.
-- Deliberately __not__ @Text.Megaparsec.Char.Lexer.scientific@, even though the
-- result type now matches. That combinator parses a different language from
-- rule 31 in both directions: it accepts an exponent, which the message below
-- names @1e5@ as refusing and @policy\/num-nonsfeel-exponent-refused@ pins, and
-- it requires an integer part, so it would reject the @.5@ that
-- @policy\/num-leading-dot-accepted@ pins. Swapping it in would silently flip
-- two policy recordings. @read@ over the accepted character run is all that is
-- wanted here, because the accepted grammar is a strict subset of Haskell's
-- decimal syntax — and @ds@ is the author's own text, so the 'Scientific' it
-- builds carries the author's digits and scale.
numericLiteral :: Parser Scientific
numericLiteral = do
  neg <- option False (True <$ char '-')
  ds  <- withIntPart <|> withoutIntPart
  let n = read ds :: Scientific
  pure (if neg then negate n else n)
  where
    withIntPart = do
      i <- some digitChar
      f <- option "" (try ((:) <$> char '.' <*> some digitChar))
      pure (i ++ f)
    withoutIntPart = char '.' *> (("0." ++) <$> some digitChar)

-- ** Rule 3: arithmetic

-- | Arithmetic, __last__ in the chain and __restricted to a real operator
-- application__.
--
-- Restricted, because demoting arithmetic from first to last opens
-- 'parseFNumFunction' to text the old @intersect \"+-*\/\"@ gate kept away from
-- it, and that parser is generous: @parseFNF1@ accepts a bare identifier and
-- @parseFNF0@ accepts a megaparsec @scientific@ literal and the words
-- @yes\/no\/true\/false@. Accepting an @FNF0@ or @FNF1@ here would mean:
--
--  * @Fall@ in a declared @Number@ column becomes @FFunction (FNF1 \"Fall\")@,
--    and 'DMN.Translate.FEELhelpers.showFeel' renders that as a __bare
--    identifier__ — TypeScript referencing an undefined name, at exit 0, where
--    today it is a refusal;
--  * @1e5@ relocates from @FNullary (VN 100000.0)@ to
--    @FFunction (FNF0 (VN 100000.0))@, which 'DMN.DecisionTable.fEval' cannot
--    evaluate at all.
--
-- @FNF3@ means an operator was actually applied, which is the only thing this
-- arm is being asked about. The cost, accepted and named: a bare qualified-name
-- output cell (@Age@, meaning "copy the input") stays unsupported, as it is
-- today.
arithmetic :: Parser FEELexp
arithmetic = do
  f <- parseFNumFunction
  case f of
    FNF3{} -> pure (FFunction f)
    _      -> fail "not an operator application"

-- ** Constructs recognised in order to be refused

-- | Rule 12.b, @\"not\", \"(\", simple positive unary tests, \")\"@.
--
-- dmnmd has no IR for negation, so this is refused. Refusing is a strict
-- improvement over the previous behaviour, which was to compile the cell to its
-- own exact logical complement at exit 0 — the unanchored interval search
-- matched @[1..5]@ inside @not([1..5])@ and threw the @not@ away.
peelNot :: String -> Maybe String
peelNot s
  | "not" `isPrefixOf` s
  , ('(':rest) <- trimLeft' (drop 3 s)
  , not (null rest)
  , last rest == ')' = Just (init rest)
  | otherwise = Nothing

-- | @name(…)@, used __only__ to choose a better refusal message, never to
-- accept anything, so a false positive costs wording rather than meaning.
--
-- Two spellings the previous draft of this function missed, both measured
-- against real input: a multi-argument call @decimal(Units, 2)@ arrives here
-- already shredded by the comma split, so its fragment @decimal(Units@ has no
-- closing paren; and the Camunda\/Trisotech output-entry spelling carries a
-- leading @=@, as @test\/safe2.dmn@ does. Both are handled.
callName :: String -> Maybe String
callName s0 = case break (== '(') s of
    (nm, '(':_) | let n = trim' nm
                , not (null n)
                , isAlpha (head n)
                , all nameish n -> Just n
    _ -> Nothing
  where
    s = case s0 of ('=':r) -> trimLeft' r; _ -> s0
    nameish c = c `elem` (" _." :: String) || isAlpha c || c `elem` ("0123456789" :: String)

-- ** Messages
--
-- Every one of these names the construct, cites the rule, and prints the
-- repair. The location — table, column, row — is prepended by the caller on the
-- markdown path; see 'DMN.DecisionTable.mkFsAt' and 'DMN.DecisionTable.mkFAt'.
-- The XML reader frames these itself. Neither path prints a file name; the
-- 'DMN.DecisionTable.CellSite' haddock says why.

-- | @not(…)@ whose CONTENTS are not a unary test.
--
-- Negation itself is implemented (D-9); what this refuses is the inside. Rule
-- 12.b admits @simple positive unary tests@, so a nested negation and an
-- invocation are both out, and so is anything that is not a test at all.
--
-- The multi-test form @not(a, b)@ also lands here, and the message says so
-- because the reason is not visible from the fragment: 'DMN.DecisionTable.mkFsEither'
-- splits every cell on commas before this module sees it, so @not(\"Fall\", \"Winter\")@
-- arrives already shredded into @not(\"Fall\"@ and @\"Winter\")@ and neither half
-- can be recognised. Fixing that needs a bracket-aware splitter, which exists
-- ('DMN.DecisionTable.splitArgs') but is deliberately not wired in here — see D-9.
negationMsg :: String -> String -> String
negationMsg cell inner = concat
  [ "the cell reads ", show cell
  , " — dmnmd implements FEEL negation (DMN 1.3 §9.2 rule 12.b), but ", show inner
  , " is not a simple positive unary test. Rule 12.b admits a value, a comparison"
  , " or an interval, so not([1..5]) and not(> 3) are fine while a nested not()"
  , " or a function call is not."
  , " If you meant several alternatives, note that dmnmd splits a cell on commas"
  , " before reading it, so not(a, b) is not yet supported; write the complement"
  , " as explicit rows instead."
  ]

-- | @not(…)@ around arithmetic, which is a rule 3 simple expression rather than
-- a unary test and is legal only in an output cell — where a negation in turn is
-- not. Separated from 'negationMsg' because the repair is different: the cell is
-- well-formed FEEL in the wrong place, not malformed.
-- | The multi-test negation @not(a, b)@, seen after the comma split has already
-- broken it up. Its own message because the fragment in hand is not what the
-- author wrote, so quoting it without explanation reads as a bug in the tool.
splitNegationMsg :: String -> String
splitNegationMsg cell = concat
  [ "the cell reads ", show cell, ", which is part of a negation over several"
  , " tests. dmnmd implements FEEL negation (DMN 1.3 §9.2 rule 12.b) for a single"
  , " test — not([1..5]), not(> 3) — but it splits a cell on commas before"
  , " reading it, so not(a, b) arrives already broken in two and the fragment"
  , " shown here is not what you wrote."
  , " Write the alternatives as separate rows, or negate one test."
  ]

negationOfArithMsg :: String -> String -> String
negationOfArithMsg cell inner = concat
  [ "the cell reads ", show cell
  , " — ", show inner, " is arithmetic (DMN 1.3 §9.2 rule 3, a simple"
  , " expression), and negation applies to a unary test (rule 12.b), not to a"
  , " value. Arithmetic is legal only in an OUTPUT cell, where a negation is not."
    -- Recommend a literal, NOT `not(< <inner>)`. That was the advice here until
    -- a verify lens ran it: dmnmd refuses `not(< Age * 2)` AND the unnegated
    -- `< Age * 2`, because a comparison's right-hand side must be a literal, so
    -- the suggestion was impossible in both forms. A refusal that names an
    -- impossible repair is worse than one that names none — the author trusts it
    -- and loses the time twice. Same family as the message that once told every
    -- author to declare a "Season" column they did not have.
  , " Negation needs a unary test whose comparison is against a literal, as in"
  , " not(< 10) or not([1..5]); dmnmd does not accept a comparison against"
  , " arithmetic in an input cell, negated or not."
  ]

invocationMsg :: String -> String -> String
invocationMsg cell fn = concat
  [ "the cell reads ", show cell
  , " — dmnmd does not implement the FEEL function call ", fn, "()."
  , " Invocation is outside S-FEEL entirely: DMN 1.3 §9.2's rules 1-38 contain no"
  , " invocation production (the only parenthesised forms in the whole grammar are"
  , " not(...) and the date/time/duration literals of rule 34), and at conformance"
  , " level 3 it takes the table out of the DMN-analysable fragment."
  , " Compute ", fn, "(...) in a preceding decision and pass it in as a column."
  ]

-- | Deliberately says dmnmd is /reading/ the column as Number rather than that
-- the author /typed/ it that way. Both routes reach here and they need different
-- repairs: an explicit @Season : Number@ header, or
-- 'DMN.DecisionTable.inferEvidence' resolving the column to Number off its other
-- cells. Saying "this column is typed Number" to an author who typed no such
-- thing is false and leaves them nothing to do, so the closing sentence names
-- the repair for the inferred case, exactly as R8's message does.
--
-- That closing sentence used to describe the pre-D-2 rule verbatim — "any cell
-- in the column containing @..@, @>@, @<@, @=@ or a spaced arithmetic operator".
-- It was true when written and is the reason @L1 > L2@ and @Coming soon...@ used
-- to reach here at all; under D-2 they no longer do, because this function is
-- now the oracle inference asks, so a cell it refuses is no longer evidence
-- that the column is numeric. Anchoring is what shrank the population that sees
-- this message: what is left is a genuinely mixed or genuinely declared column.
notATestMsg :: String -> String
notATestMsg cell = concat
  [ "the cell reads ", show cell
  , " — dmnmd is reading this column as Number, and that is not a number,"
  , " a comparison, an interval, or an arithmetic expression."
  , " A FEEL number is optionally-signed digits with at most one decimal point"
  , " (DMN 1.3 §9.2 rule 31): 5, -5, 5.25, .5 — NOT 0x10, 0o17, 1e5, 1_000, 5%,"
  , " \"5.\", \"+5\", Infinity or NaN."
  , " A comparison is < 5 or 5 <; an interval is [1..5], [1..5), (1..5] or"
  , " (1..5); two alternatives are separated by a comma (rule 11)."
    -- A placeholder, not a stock column name: this function is given the cell
    -- and not its header, so it cannot name the real column, and naming a
    -- fictional one ("Season") reads as a bug to an author whose column is
    -- called something else. Its sibling in DecisionTable.inputArithErrs does
    -- have the header in scope and does use the real name.
  , " If this column is not numeric, declare it (\"<column> : String\"): with no"
  , " declaration dmnmd infers Number from a column whose cells all read as one"
  , " of the forms above."
  ]

-- | Is this cell a construct only a @Number@ column could hold, which dmnmd
-- names and refuses rather than parses?
--
-- Type inference (D-2) asks 'parseNumberCell' whether a cell is numeric
-- evidence, and a @Left@ from there means \"not a number\" — except for the two
-- NAMED refusals, where it means \"unmistakably a numeric construct that dmnmd
-- does not implement\". Without this distinction @not([1..5])@ stops being
-- numeric evidence, its column types @String@, and the refusal recorded as
-- @symptom\/num-negation-not-implemented@ silently becomes an equality test
-- against the literal text @not([1..5])@ at exit 0 — the exact failure D-2
-- exists to remove, reintroduced by D-2. Found by running the corpus.
namedRefusal :: String -> Bool
namedRefusal raw = case trim' raw of
  cell -> maybe False (const True) (peelNot cell)
       || maybe False (const True) (callName cell)

-- | Is this raw cell text a number written with thousands separators?
--
-- FEEL has __no grouping production__: rule 31 admits no separator and rules
-- 32-33 are @digit = [0-9]@ and @digits = digit , {digit}@. So @1,000@ is
-- legally __two__ unary tests, @1@ and @000@, and @000@ is a legal literal
-- denoting 0 — which means dmnmd's old output for @>= 1,000@,
-- @(Amount >= 1.0 || Amount === 0.0)@, was /conformant/ and the recorded
-- diagnosis of those two corpus cases was wrong about the parse. What was
-- unambiguously wrong is the __silence__.
--
-- Rather than invent a thousands separator — a markdown-surface extension,
-- governed by @BUILD-SPEC-dmnmd-extensions.md@ §6, and impossible to add without
-- breaking @policy\/md-multivalue-cell@ and @test\/golden\/miles-card-dmn.md@,
-- whose @4111, 4112@ is the same shape — dmnmd refuses the shape and says how to
-- disambiguate.
--
-- __Whitespace is significant, and that is the whole discriminator.__ A
-- thousands separator is conventionally written without a space and a
-- multi-value list with one, so @1,000@ matches and @1, 000@ does not; the
-- repair for a false positive is one space. Group size does the rest:
-- @4111, 4112@ and @5311, 5411@ have four-digit groups and cannot match at all.
--
-- Checked on the RAW cell, before the comma split, and independently of the
-- column type — because the split happens before anything knows the type, so a
-- @String@ column is shredded identically.
thousandsGrouped :: String -> Bool
thousandsGrouped raw = case runAnchored grouped (trim' raw) of
  Right () -> True
  Left _   -> False
  where
    grouped = do
      _ <- optional (lx cmpOp)
      _ <- optional (char '-')
      _ <- count' 1 3 digitChar
      _ <- some (try (char ',' *> count 3 digitChar))
      _ <- optional (try (char '.' *> some digitChar))
      pure ()

-- Local copies: "DMN.DecisionTable" defines these but importing it here would
-- be a cycle, since it is this module's only caller.
trim' :: String -> String
trim' = trimLeft' . reverse . trimLeft' . reverse

trimLeft' :: String -> String
trimLeft' = dropWhile (== ' ')
