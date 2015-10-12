module Lambda.Util where

import DeepControl.Applicative
import DeepControl.Monad

import Data.List (subsequences, sortBy)
import Data.Function (on)

semiInfinitePostfixes :: [String]
semiInfinitePostfixes = tail $ subsequences ['1'..'9'] >- sortBy (compare `on` length)


