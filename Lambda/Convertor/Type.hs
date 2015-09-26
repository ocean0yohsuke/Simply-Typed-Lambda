{-# LANGUAGE MultiParamTypeClasses #-}
module Lambda.Convertor.Type where

import MonadX.Applicative
import MonadX.Monad hiding (forM, mapM)

import Lambda.DataType (Name, Lambda)
import Lambda.DataType.Type
import Util.Pseudo

import Prelude hiding (forM, mapM)
import Data.Monoid
import Data.Foldable
import Data.Traversable

instance PseudoFunctor Type where
    pdfmap f (x :-> y)  = f x :-> f y
    pdfmap f (TUPLE xs) = TUPLE (f |$> xs)
    pdfmap f (CONS x)   = CONS (f x)
    pdfmap _ pm         = pm

instance PseudoFoldable Type where
    pdfoldMap f (x :-> y)  = f x <> f y
    pdfoldMap f (TUPLE xs) = fold $ f |$> xs
    pdfoldMap f (CONS x)   = f x 
    pdfoldMap f x          = f x

instance PseudoTraversable Lambda Type where
    pdmapM f (x :-> y)  = f x <$|(:->)|*> f y
    pdmapM f (TUPLE xs) = TUPLE |$> mapM f xs
    pdmapM f (CONS x)   = CONS |$> f x
    pdmapM _ pm         = (*:) pm


