{-# LANGUAGE RankNTypes #-}
module DMN.XML.PickleHelpers where

import Text.XML.HXT.Core (xpWrap, PU)
import Control.Lens (view, Iso', review)

isoToPair :: Iso' s a -> (a -> s, s -> a)
isoToPair l = (review l, view l)

wrapIso :: Iso' a b -> PU b -> PU a
wrapIso x = xpWrap $ isoToPair x