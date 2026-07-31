-- | The number policy, in one place.
--
-- @DECISIONS.md@ D-1 rules that dmnmd's numeric value is 'Scientific' rather
-- than 'Float', because FEEL's number __is__ decimal128 (DMN 1.3 §10.3.2.3.1):
-- a decimal representation matches the standard's own model instead of
-- approximating it from outside.
--
-- Everything that follows from that ruling — how a number is spelled back out,
-- what a division means, what magnitude we are willing to hold — lives here, so
-- that the four backends and the diagnostics cannot drift apart. Before this
-- module there were five independent number-to-text sites and they did not
-- agree with each other.
module DMN.Number
  ( -- * Rendering
    showNumPlain
  , showNumFloatish
    -- * Arithmetic
  , feelPrecision
  , divideFeel
  , powerFeel
    -- * Magnitude
  , maxBase10Exponent
  , spellable
  ) where

import Data.Ratio (numerator, denominator)
import Data.Scientific
  ( Scientific, base10Exponent, coefficient, formatScientific, isInteger
  , normalize, scientific, FPFormat (Fixed) )

-- * Rendering

-- | A number as the author wrote it: plain decimal, __never__ exponent
-- notation, and no trailing @.0@ on an integral value.
--
-- Used by the L4 backend and by every diagnostic that quotes a cell back at
-- the author. Both need the same thing for the same reason, and neither can
-- use 'show'.
--
-- Why not 'show'. @show \@Scientific@ inherits @show \@Float@'s fixed-vs-exponent
-- rule — fixed on @[0.1, 1e7)@, exponent outside it — so it would spell the
-- author's @16777217@ as @1.6777217e7@: the right value with the wrong text. In
-- the L4 case it is not merely ugly but wrong. L4 has no exponent production, so
-- @1.0e8@ lexes as the literal @1.0@ applied to a variable named @e8@ and the
-- emitted file does not typecheck.
--
-- Why not @showFFloat (Just 6)@, which is what this used to be: it collapsed
-- @0.0000001@ to @0.0@, rendering a nonzero value as zero (BUILD-SPEC §1.2/§9.5).
-- 'Scientific' stores the author's digits exactly, so @formatScientific Fixed
-- Nothing@ replaces the whole @floatToDigits@ shortest-round-trip dance: there
-- is no round trip to be shortest about.
--
-- Scale is __not__ preserved: @9.0@ and @9@ are the same number and both render
-- @\"9\"@. 'Scientific' does record the difference (coefficient 90 exponent -1
-- versus 9 and 0) but its 'Eq' does not compare on it, so a renderer that did
-- would distinguish values the IR treats as identical. What is preserved is
-- every significant digit.
showNumPlain :: Scientific -> String
showNumPlain n
  | isInteger n = show (round n :: Integer)
  | otherwise   = formatScientific Fixed Nothing n

-- | A number for a target language whose numeric literal wants to stay a
-- float: plain decimal, always with a fractional part. @5@ renders @\"5.0\"@,
-- @16777217@ renders @\"16777217.0\"@.
--
-- Used by the JS/TS and Python backends, which have always emitted a decimal
-- point on every number. That convention is load-bearing in Python and only
-- there: @{\"Fee\": 5}@ is an @int@ and @{\"Fee\": 5.0}@ is a @float@, and they
-- differ under @isinstance@, @repr@ and a JSON round trip. A DMN @Number@ column
-- is decimal, so the float is the honest emission.
--
-- Whether ts/js/py should instead drop the @.0@ and emit exactly what the author
-- wrote is a real question and a separate one; see the commit that introduced
-- this module. It is not forced by D-1, it changes generated Python's types, and
-- it would move about seventy corpus recordings — burying the two that D-1 is
-- actually about.
showNumFloatish :: Scientific -> String
showNumFloatish = formatScientific Fixed Nothing

-- * Arithmetic

-- | decimal128 carries 34 significant decimal digits (DMN 1.3 §10.3.2.3.1).
--
-- This is the precision an inexact result is rounded to. D-1 names it: rounding
-- at 34 significant digits is what the spec asks for, and being /more/ exact
-- than that is not more correct.
feelPrecision :: Int
feelPrecision = 34

-- | Division, rounded half-to-even to 'feelPrecision' significant digits.
--
-- 'Scientific' has a 'Fractional' instance and it is a trap: @(1 :: Scientific)
-- \/ 3@ raises @fromRational has been applied to a repeating decimal which
-- can't be represented as a Scientific!@ — a bare library @error@ carrying a
-- @Data.Scientific@ call stack, because a 'Scientific' is a terminating decimal
-- by construction and one third is not one. Under 'Float' the same expression
-- quietly answered @0.33333334@.
--
-- So the quotient is computed exactly as a 'Rational' and then rounded, which
-- is what decimal128 does and what the previous behaviour only appeared to do.
-- Half-to-even is 'round''s behaviour on a 'Rational' and is decimal128's
-- default rounding mode.
--
-- Division by zero is refused rather than answered. 'Float' returned
-- @Infinity@, at exit 0, which every backend then printed — a valid JavaScript
-- literal and a Python @NameError@. A decimal has no infinity to return.
divideFeel :: Scientific -> Scientific -> Either String Scientific
divideFeel _ 0 = Left "division by zero: FEEL numbers are decimal and have no infinity"
divideFeel a b = Right (roundToPrecision (toRational a / toRational b))

-- | Exponentiation.
--
-- @**@ needs a 'Floating' instance and 'Scientific' has none, on purpose: a
-- decimal cannot hold @2 ** 0.5@ at all. Routing through 'Double' would
-- reintroduce exactly the binary rounding D-1 exists to remove, on the one
-- operator most able to make it visible.
--
-- So an integral exponent is exact (negative exponents go through
-- 'divideFeel', hence the same 34-digit rounding), and a fractional exponent is
-- refused with a located message rather than approximated. A result too large
-- to spell is refused too — see 'maxBase10Exponent'.
powerFeel :: Scientific -> Scientific -> Either String Scientific
powerFeel base ex
  | not (isInteger ex) = Left $
      "fractional exponent " ++ showNumPlain ex ++
      ": the result is not a decimal number, and FEEL numbers are decimal"
  | n >= 0 =
      let r = base ^ n
      in if spellable r then Right r else tooBig
  | base == 0 = Left "0 raised to a negative power: division by zero"
  | otherwise =
      let d = base ^ negate n
      in if spellable d then divideFeel 1 d else tooBig
  where
    n = round ex :: Integer
    -- The exponent is bounded before the power is taken, not after: 10 ^ 10^9
    -- is not a big number, it is an out-of-memory.
    tooBig = Left $ "result of raising to the power " ++ showNumPlain ex ++
                    " is too large to represent"

-- | Round an exact 'Rational' to 'feelPrecision' significant decimal digits,
-- half to even.
roundToPrecision :: Rational -> Scientific
roundToPrecision 0 = 0
roundToPrecision q = normalize (scientific c (negate s))
  where
    s = feelPrecision - decimalMagnitude q
    c = round (q * (10 ^^ s)) :: Integer

-- | The @e@ with @10^(e-1) <= |q| < 10^e@: how many digits @|q|@ has before the
-- decimal point, which is @<= 0@ for a value below one.
--
-- Seeded from the digit counts of numerator and denominator, which is right to
-- within one, then corrected. Not @logBase 10@: a binary float log is the wrong
-- tool for deciding a decimal digit position.
decimalMagnitude :: Rational -> Int
decimalMagnitude q = fix (digits (numerator aq) - digits (denominator aq))
  where
    aq = abs q
    digits = length . show . abs
    fix k | aq >= 10 ^^ k       = fix (k + 1)
          | aq <  10 ^^ (k - 1) = fix (k - 1)
          | otherwise           = k

-- * Magnitude

-- | The largest decimal exponent dmnmd will hold, in either direction.
--
-- A bound is needed because 'Scientific' is arbitrary-precision in the
-- exponent, and every route out of it is proportional to the /spelled/ value:
-- @formatScientific Fixed@ on @1e1000000@ allocates a million digits, and
-- @round@ builds a million-digit 'Integer'. 'Float' hid this behind @Infinity@,
-- which the L4 backend then printed as @0@ and the others as a JavaScript
-- literal — both wrong answers at exit 0.
--
-- 1000 is a policy number, not a spec one. FEEL's decimal128 exponent range is
-- roughly ±6143; nothing a decision table means needs a thousand-digit number,
-- and refusing early keeps the failure a diagnostic rather than an
-- out-of-memory.
maxBase10Exponent :: Int
maxBase10Exponent = 1000

-- | Whether a value is inside 'maxBase10Exponent' and so can be written out.
spellable :: Scientific -> Bool
spellable n = abs (base10Exponent n') <= maxBase10Exponent
           && abs (base10Exponent n' + length (show (abs (coefficient n')))) <= maxBase10Exponent
  where n' = normalize n
