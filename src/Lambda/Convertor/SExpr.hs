{-# LANGUAGE MultiParamTypeClasses #-}
module Lambda.Convertor.SExpr where

import DeepControl.Applicative
import DeepControl.Monad hiding (forM, mapM)

import Lambda.DataType (Lambda)
import Lambda.DataType.SExpr
import Lambda.Action
import Util.Pseudo

import Prelude hiding (forM, mapM)
import Data.Monoid
import Data.Foldable
import Data.Traversable

instance PseudoFunctor SExpr where
    -- tuple
    pdfmap f (TUPLE xs msp)       = TUPLE (f |$> xs) msp
    pdfmap f (TPLPrj x n)         = TPLPrj (f x) n 
    -- tag
    pdfmap f (TAGPrj x pair)      = TAGPrj (f x) pair 
    --
    pdfmap f (FIX x msp)          = FIX (f x) msp
    -- 
    pdfmap f (IF x1 x2 x3 msp)    = IF (f x1) (f x2) (f x3) msp
    pdfmap f (CASE x ps msp)      = CASE (f x) (ps <$| (\(pm,x) -> (pm, f x))) msp
    pdfmap f (DEF name ps)        = DEF name (ps <$| (\(pms,x,msp) -> (pms, f x, msp))) 
    -- quote
    pdfmap f (QUT t msp)          = QUT (f t) msp
    pdfmap f (QQUT t msp)         = QQUT (f t) msp
    pdfmap f (UNQUT t msp)        = UNQUT (f t) msp
    -- syntactic-sugar
    pdfmap f (APP x xs msp)       = APP (f x) (f |$> xs) msp
    pdfmap f (APPSeq xs msp)      = APPSeq (f |$> xs) msp
    pdfmap f (LAM p x msp)        = LAM p (f x) msp
    pdfmap f (LAMM p x msp)       = LAMM p (f x) msp
    pdfmap f (AS (x,ty) msp)      = AS (f x, ty) msp
    pdfmap f (LET p x1 x2 msp)    = LET p (f x1) (f x2) msp
    pdfmap f (LETREC p x1 x2 msp) = LETREC p (f x1) (f x2) msp
    -- list
    pdfmap f (CONS a d msp)       = CONS (f a) (f d) msp
    pdfmap f (HEAD x)             = HEAD (f x)
    pdfmap f (TAIL x)             = TAIL (f x)

    pdfmap _ x                    = x

instance PseudoFoldable SExpr where
    -- tuple
    pdfoldMap f (TUPLE xs _)       = fold (f |$> xs)
    pdfoldMap f (TPLPrj x n)       = f x
    -- tag
    pdfoldMap f (TAGPrj x pair)    = f x
    -- 
    pdfoldMap f (FIX x _)          = f x
    --
    pdfoldMap f (IF x1 x2 x3 _)    = f x1 <> f x2 <> f x3
    pdfoldMap f (CASE x ps _)      = f x <> fold (ps <$| (\(_,x) -> f x)) 
    pdfoldMap f (DEF name ps)      = fold (ps <$| (\(_,x,_) -> f x))
    -- quote
    pdfoldMap f (QUT t _)          = f t
    pdfoldMap f (QQUT t _)         = f t
    pdfoldMap f (UNQUT t _)        = f t
    -- syntactic-sugar
    pdfoldMap f (APP x xs _)       = f x <> fold (f |$> xs)
    pdfoldMap f (APPSeq xs _)      = fold (f |$> xs)
    pdfoldMap f (LAM p x _)        = f x
    pdfoldMap f (LAMM p x _)       = f x
    pdfoldMap f (AS (x,ty) _)      = f x
    pdfoldMap f (LET p x1 x2 _)    = f x1 <> f x2
    pdfoldMap f (LETREC p x1 x2 _) = f x1 <> f x2
    -- list 
    pdfoldMap f (CONS a d _)     = f a <> f d
    pdfoldMap f (HEAD x)         = f x
    pdfoldMap f (TAIL x)         = f x
  
    pdfoldMap f x                  = f x


instance PseudoTraversable Lambda SExpr where
    -- tuple
    pdmapM f (TUPLE xs msp)   = localMSPBy msp $ TUPLE |$> mapM f xs |* msp
    pdmapM f (TPLPrj x n)     = TPLPrj |$> f x |* n
    -- tag
    pdmapM f (TAGPrj x p)     = TAGPrj |$> f x |* p
    --
    pdmapM f (FIX x msp)      = localMSPBy msp $ FIX |$> f x |* msp
    --
    pdmapM f (IF x1 x2 x3 msp) = localMSPBy msp $ IF |$> f x1 |*> f x2 |*> f x3 |* msp
    pdmapM f (CASE x ps msp)   = localMSPBy msp $ CASE |$> f x |*> for ps (\(pm,x) -> pm <|(,)|$> f x) |* msp
    pdmapM f (DEF name ps)     = DEF name |$> for ps (\(pms,x,msp) -> localMSPBy msp $ ((,,) pms) |$> f x |* msp)
    -- quote
    pdmapM f (QUT t msp)      = localMSPBy msp $ QUT |$> f t |* msp
    pdmapM f (QQUT t msp)     = localMSPBy msp $ QQUT |$> f t |* msp
    pdmapM f (UNQUT t msp)    = localMSPBy msp $ UNQUT |$> f t |* msp
    -- syntactic-sugar
    pdmapM f (APP x xs msp)   = localMSPBy msp $ APP |$> f x |*> mapM f xs |* msp
    pdmapM f (APPSeq xs msp)  = localMSPBy msp $ APPSeq |$> mapM f xs |* msp
    pdmapM f (LAM p x msp)    = localMSPBy msp $ LAM p |$> f x |* msp
    pdmapM f (LAMM p x msp)   = localMSPBy msp $ LAMM p |$> f x |* msp
    pdmapM f (AS (x,ty) msp)      = localMSPBy msp $ AS |$> (f x <$|(,)|* ty) |* msp
    pdmapM f (LET p x1 x2 msp)    = localMSPBy msp $ LET p |$> f x1 |*> f x2 |* msp
    pdmapM f (LETREC p x1 x2 msp) = localMSPBy msp $ LETREC p |$> f x1 |*> f x2 |* msp
    -- list
    pdmapM f (CONS a d msp)   = localMSPBy msp $ CONS |$> f a |*> f d |* msp
    pdmapM f (HEAD x)         = HEAD |$> f x 
    pdmapM f (TAIL x)         = TAIL |$> f x 

    pdmapM _ x                = (*:) x


