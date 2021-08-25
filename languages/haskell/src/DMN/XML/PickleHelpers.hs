{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module DMN.XML.PickleHelpers where

import Text.XML.HXT.Core (xp6Tuple, XmlPickler(..), xpWrap, PU, xpAlt, xpUnit, xpPair, xpElem)
import Control.Lens (iso, withIso, AnIso', view, Iso', review)
import qualified Data.List as L
import Data.List (intercalate)

import qualified GHC.Generics as GHC
import Generics.SOP
import Data.Maybe (fromJust)

isoToPair :: AnIso' s a -> (a -> s, s -> a)
isoToPair l = withIso l $ \to from -> (from, to)

wrapIso :: AnIso' a b -> PU b -> PU a
wrapIso x = xpWrap $ isoToPair x


pairsIso :: Iso' (a, b, c) (a,(b,c))
pairsIso = iso (\(a,b,c) -> (a,(b,c))) (\(a,(b,c)) -> (a,b,c))


data Foo a b = Foo a | Bar b
  deriving (Show, Eq, GHC.Generic, Generic)


getIndex :: Generic a => a -> Int
getIndex = hindex . from

type PickleableSum a = (Generic a, All (Compose XmlPickler (NP I)) (Code a))

-- | Pickler for an untagged sum type/union type
pickleUntaggedUnion :: PickleableSum a => PU a
pickleUntaggedUnion = pickleSumOf mkPicklersForSum

-- | Given a function for creating elements and a list of element names, constructs a pickler for a sum-type
pickleTaggedSumOf :: PickleableSum a => (String -> PU a -> PU a) -> NP (K String) (Code a) -> PU a
pickleTaggedSumOf mkElem = pickleSumOf . hzipWith (mapKKK (flip mkElem)) mkPicklersForSum

-- | Example of tagged sum
instance (XmlPickler a, XmlPickler b) => XmlPickler (Foo a b) where
 xpickle = pickleTaggedSumOf xpElem $ K "foo" :*  K "bar" :* Nil

-- >>> showPickled [] [Foo (), Bar ()]
-- "<foo/><bar/>"

-- | Takes a list of picklers, one for each constructor and returns a pickler for the whole thing
pickleSumOf :: Generic a => NP (K (PU a)) (Code a) -> PU a
pickleSumOf = xpAlt getIndex . hcollapse 

-- | Creates a list of picklers for a sum type, one for each constructor
mkPicklersForSum :: PickleableSum a => NP (K (PU a)) (Code a)
mkPicklersForSum = hczipWith (Proxy @(Compose XmlPickler (NP I))) (\f g -> K $ xpWrap (unK . apFn f, apFn g . K) xpickle) myInject myEject

-- | A list of all constructors for a sum type
myInject :: Generic a => NP (NP I -.-> K a) (Code a)
myInject = hmap (Fn . fmap (mapKK (to . SOP)) . apFn) injections

-- | /PARTIAL/ A list of all eliminators for a sum type. Crashes if the wrong eliminator is used
myEject :: Generic a => NP (K a -.-> NP I) (Code a)
myEject = hmap (Fn . ((fromJust . unComp).) . (.mapKK (unSOP . from)) . apFn) ejections

-- | Orphan instances for XmlPickler of NP I xs
instance XmlPickler (NP I '[]) where
  xpickle = xpWrap (\ () -> Nil, \ Nil -> ()) xpUnit

instance (XmlPickler x, XmlPickler (NP I xs)) => XmlPickler (NP I (x ': xs)) where
  xpickle = xpWrap (uncurry ((:*) . I), \(I i :* np') -> (i, np')) xpickle

-- >>> to . SOP . unK $ apFn (hd . tl $ injections @(Code (Foo () ()))) (I () :* Nil) :: Foo () ()

-- >>> map (to . SOP) $ hcollapse $ hcmap (Proxy @((~) '[ () ])) (`apFn` (I () :* Nil)) $ injections @(Code (Foo () ())) @(NP I) :: [Foo () ()]
-- [Foo (),Bar ()]


-- >>> unComp . ($ K (Z (I () :* Nil))) . apFn . hd $ ejections @(Code (Foo () Bool)) @(NP I)
-- Just (I () :* Nil)

-- >>> unComp . ($ K (Z (I () :* Nil))) . apFn . hd . tl $ ejections @(Code (Foo () Bool)) @(NP I)
-- Nothing

-- >>> unComp . ($ K (S $ Z (I True :* Nil))) . apFn . hd . tl $ ejections @(Code (Foo () Bool)) @(NP I)
-- Just (I True :* Nil)

-- >>> unComp . ($ K (unSOP $ from (Bar True))) . apFn . hd . tl $ ejections @(Code (Foo () Bool)) @(NP I)

-- >>> unComp . ($ K (unSOP $ from (Bar True))) . apFn . hd . tl $ ejections

{-
-}

{-
Generate XmlPickler instances for tuples of size >= 6.
This is contributed upstream in https://github.com/UweSchmidt/hxt/pull/86
-}

--- $> putStrLn $ mkInstance 6

-- | Generates XmlPickler instances for tuples of size 4 <= n <= 26
mkInstance :: Int -> String
mkInstance n =
    "instance (" ++ constrainsts ++ ") => XmlPickler (" ++
    tuple ++ ") where\n" ++
    "  xpickle = xp" ++ show n ++ "Tuple " ++ xpickleStrings
  where
    xpickleStrings = unwords (replicate n "xpickle")
    tuple = intercalate ", " letters
    letters = map (:[]) $ take n ['a'..'z']
    constrainsts = intercalate ", " $ map oneConstr letters
    oneConstr a = "XmlPickler " ++ a

mkInstances :: String
mkInstances = intercalate "\n\n" $ mkInstance <$> [6..24]

myHeader :: String
myHeader =
    "module Text.XML.HXT.Pickle.TupleInstances where\n" ++
    "import Text.XML.HXT.Core\n\n"

--- $> :! mkdir -p src/Text/XML/HXT/Pickle

--- $> writeFile "src/Text/XML/HXT/Pickle/TupleInstances.hs" $ myHeader ++ mkInstances
