{-# LANGUAGE MultiParamTypeClasses #-}
module Lambda.Convertor.PatternMatch where

import MonadX.Applicative
import MonadX.Monad hiding (forM, mapM)

import Lambda.DataType (Lambda)
import Lambda.DataType.PatternMatch
import Lambda.Action
import Util.Pseudo

import Prelude hiding (forM, mapM)
import Data.Monoid
import Data.Foldable
import Data.Traversable

instance PseudoFunctor PatternMatch where
    pdfmap f (CONS x y msp)    = CONS (f x) (f y) msp
    pdfmap f (TUPLE xs msp)    = TUPLE (f |$> xs) msp
    pdfmap f (TAG name xs msp) = TAG name (f |$> xs) msp
    pdfmap _ pm                = pm

instance PseudoFoldable PatternMatch where
    pdfoldMap f (CONS x y _)    = f x <> f y
    pdfoldMap f (TUPLE xs _)    = fold $ f |$> xs
    pdfoldMap f (TAG name xs _) = fold $ f |$> xs
    pdfoldMap f x               = f x

instance PseudoTraversable Lambda PatternMatch where
    pdmapM f (CONS x y msp)    = localMSPBy msp $ CONS |$> f x |*> f y |* msp
    pdmapM f (TUPLE xs msp)    = localMSPBy msp $ TUPLE |$> mapM f xs |* msp
    pdmapM f (TAG name xs msp) = localMSPBy msp $ TAG name |$> mapM f xs |* msp
    pdmapM _ pm                = (*:) pm


