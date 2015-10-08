{-# LANGUAGE MultiParamTypeClasses #-}
module Lambda.Convertor.PatternMatch where

import DeepControl.Applicative
import DeepControl.Monad hiding (forM, mapM)

import Lambda.DataType (Name, Lambda, Type)
import qualified Lambda.DataType as T
import Lambda.DataType.PatternMatch
import qualified Lambda.DataType.Type as Ty
import Lambda.Action
import Util.Pseudo

import Prelude hiding (forM, mapM, concat)
import Data.Monoid
import Data.Foldable
import Data.Traversable
import qualified Data.Map as M

import Debug.Trace

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

-----------------------------------------------------------------------
-- localLAMPush
-----------------------------------------------------------------------

localLAMPush :: (PM, Type) -> Lambda a -> Lambda a
localLAMPush (pm, ty) lam = do
    matches <- patternMatch (pm, ty)
    let newctx = matches <$| \(name, ty) -> (name, (ty, T.unit))
    ctx <- askContext
    lam >- localContextBy (newctx ++ ctx)
  where
    patternMatch :: (PM, Type) -> Lambda [(Name, Type)]
    patternMatch (pm,                  Ty.UNIT)          = (*:) $ (pdfoldMap takename pm) <$|(,)|* Ty.UNIT
    patternMatch (VAR name _,          ty)               = (*:) [(name, ty)]
    patternMatch (CONS a d msp,        Ty.CONS ty)       = localMSPBy msp $ patternMatch (a, ty) <$|(++)|*> patternMatch (d, Ty.CONS ty) 
    patternMatch (TUPLE pms msp,       Ty.TUPLE tys)     = localMSPBy msp $ foldlM (\acc (pm, ty) -> (acc++)|$> patternMatch (pm, ty)) [] (zip pms tys)
    patternMatch (TAG tagname pms msp, Ty.DATA typename) = localMSPBy msp $ do
        mtys <- lookupTypeDef typename tagname
        case mtys of
          Nothing  -> error $ "patternMatch: TAG: nothing: lookup " ++ show typename ++" "++ show tagname
          Just tys -> foldlM (\acc (pm, ty) -> (acc++)|$> patternMatch (pm, ty)) [] (zip pms tys)
    patternMatch _ = (*:) []

takename :: PM -> [Name]
takename (VAR name _) = [name]
takename (CONS a d _) = takename a ++ takename d
takename (TUPLE xs _) = concat $ takename |$> xs
takename (TAG _ xs _) = concat $ takename |$> xs
takename _            = []

