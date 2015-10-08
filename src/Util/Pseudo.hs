{-# LANGUAGE MultiParamTypeClasses #-}
module Util.Pseudo where

import Control.Applicative
import Control.Monad
import Data.Monoid

class PseudoFunctor pd where
    pdfmap :: (pd -> pd) -> pd -> pd

class PseudoFoldable pd where
    pdfoldMap :: Monoid b => (pd -> b) -> pd -> b

class (Applicative m, Monad m) => PseudoTraversable m pd where
    pdmapM :: (pd -> m pd) -> pd -> m pd
    pdforM :: pd -> (pd -> m pd) -> m pd
    pdforM = flip pdmapM

class Convert a b where
    convert :: a -> b


