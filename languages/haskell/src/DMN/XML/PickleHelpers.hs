module DMN.XML.PickleHelpers where

import Text.XML.HXT.Core (xp6Tuple, XmlPickler(..), xpWrap, PU)
import Control.Lens (iso, withIso, AnIso', view, Iso', review)
import qualified Data.List as L
import Data.List (intercalate)

isoToPair :: AnIso' s a -> (a -> s, s -> a)
isoToPair l = withIso l $ \to from -> (from, to)

wrapIso :: AnIso' a b -> PU b -> PU a
wrapIso x = xpWrap $ isoToPair x


pairsIso :: Iso' (a, b, c) (a,(b,c))
pairsIso = iso (\(a,b,c) -> (a,(b,c))) (\(a,(b,c)) -> (a,b,c))

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
    xpickleStrings = intercalate " " (replicate n "xpickle")
    tuple = intercalate ", " letters
    letters = map (:[]) $ take n ['a'..'z']
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
