{-# LANGUAGE MultiParamTypeClasses #-}
module Lambda.Convertor.Expr where

import DeepControl.Applicative
import DeepControl.Monad hiding (forM, mapM)

import Lambda.DataType (Lambda)
import Lambda.DataType.Expr
import Lambda.Action
import Lambda.Convertor.PatternMatch 
import Util.Pseudo

import Prelude hiding (forM, mapM)
import Data.Monoid
import Data.Foldable
import Data.Traversable

instance PseudoFunctor Expr where
    pdfmap f (LAM p x msp)   = LAM p (f x) msp
    pdfmap f (LAMM p x msp)  = LAMM p (f x) msp
    pdfmap f (FIX x msp)     = FIX (f x) msp
    pdfmap f (APP x1 x2 msp) = APP (f x1) (f x2) msp
    pdfmap f (APPSeq xs msp) = APPSeq (f |$> xs) msp
    -- gadget
    pdfmap f (SYN g msp)   = SYN (fmap f g) msp
    pdfmap f (SEN g msp)   = SEN (fmap f g) msp
    pdfmap f (QUT g msp)   = QUT (fmap f g) msp
    pdfmap f (LIST g msp)  = LIST (fmap f g) msp
    pdfmap f (TPL g msp)   = TPL (fmap f g) msp
    pdfmap f (TAG g msp)   = TAG (fmap f g) msp
    --
    pdfmap f (THUNK t)       = THUNK (f t)

    pdfmap _ x               = x

instance PseudoFoldable Expr where
    pdfoldMap f (LAM p x _)     = f x
    pdfoldMap f (LAMM p x _)    = f x
    pdfoldMap f (FIX x _)       = f x
    pdfoldMap f (APP x1 x2 _)   = f x1 <> f x2
    pdfoldMap f (APPSeq xs _)   = fold $ f |$> xs
    -- gadget
    pdfoldMap f (SYN g _)     = foldMap f g
    pdfoldMap f (SEN g _)     = foldMap f g
    pdfoldMap f (QUT g _)     = foldMap f g
    pdfoldMap f (LIST g _)    = foldMap f g
    pdfoldMap f (TPL g _)     = foldMap f g
    pdfoldMap f (TAG g _)     = foldMap f g
    --
    pdfoldMap f (THUNK t)       = f t

    pdfoldMap f x               = f x

instance PseudoTraversable Lambda Expr where
    pdmapM f (LAM p x msp)   = localMSPBy msp $ LAM p |$> localLAMPush p (f x) |* msp
    pdmapM f (LAMM p x msp)  = localMSPBy msp $ LAMM p |$> localLAMPush p (f x) |* msp
    pdmapM f (FIX x msp)     = localMSPBy msp $ FIX |$> f x |* msp
    pdmapM f (APP x1 x2 msp) = localMSPBy msp $ APP |$> f x1 |*> f x2 |* msp
    pdmapM f (APPSeq xs msp) = localMSPBy msp $ APPSeq |$> mapM f xs |* msp
     -- gadget
    pdmapM f (SYN g msp)   = localMSPBy msp $ SYN |$> mapM f g |* msp
    pdmapM f (SEN g msp)   = localMSPBy msp $ SEN |$> mapM f g |* msp
    pdmapM f (QUT g msp)   = localMSPBy msp $ QUT |$> mapM f g |* msp
    pdmapM f (LIST g msp)  = localMSPBy msp $ LIST |$> mapM f g |* msp
    pdmapM f (TPL g msp)   = localMSPBy msp $ TPL |$> mapM f g |* msp
    pdmapM f (TAG g msp)   = localMSPBy msp $ TAG |$> mapM f g |* msp
    --
    pdmapM f (THUNK t)       = THUNK |$> f t

    pdmapM _ x               = (*:) x


