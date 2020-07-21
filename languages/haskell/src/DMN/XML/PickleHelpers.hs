module DMN.XML.PickleHelpers where

import Text.XML.HXT.Core (xpWrap, PU)
import Control.Lens (withIso, AnIso', view, Iso', review)

isoToPair :: AnIso' s a -> (a -> s, s -> a)
isoToPair l = withIso l $ \to from -> (from, to)

wrapIso :: AnIso' a b -> PU b -> PU a
wrapIso x = xpWrap $ isoToPair x