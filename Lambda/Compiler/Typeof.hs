module Lambda.Compiler.Typeof (
    applyTypeVars, 
    localLAMPush,
) where

import MonadX.Applicative
import MonadX.Monad

import Lambda.Action
import Lambda.Convertor
import qualified Lambda.DataType.PatternMatch as PM
import qualified Lambda.DataType.Type as Ty
import Lambda.DataType

import Data.List (lookup)
import qualified Data.Map as M
import Data.Foldable
import Debug.Trace

-----------------------------------------------------------------------
-- applyTypeVars
-----------------------------------------------------------------------

applyTypeVars :: (Type, Type) -> Type -> Type
applyTypeVars match target = 
    let matchevars = patternMatchVars match
    in  substitute matchevars target
  where
    patternMatchVars :: (Type, Type) -> [(Name,Type)]
    patternMatchVars pair = rec pair []
      where
        rec :: (Type,Type) -> [(Name,Type)] -> [(Name,Type)]
        rec (Ty.VAR name, ty)               matches = (name,ty):matches
        rec (x :-> y, x' :-> y')            matches = (rec (x,x') <$|(++)|*> rec (y,y')) matches
        rec (Ty.TUPLE xs, Ty.TUPLE xs')     matches = fold $ rec |$> zip xs xs' |* matches
        rec (Ty.CONS x, Ty.CONS x')         matches = rec (x,x') matches
        rec _                               matches = matches 
    substitute :: [(Name,Type)] -> Type -> Type
    substitute matches (Ty.VAR name) = 
        case lookup name matches of
          Nothing -> Ty.VAR name
          Just ty -> ty
    substitute matches (x :-> y)       = substitute matches x :-> substitute matches y
    substitute matches (Ty.TUPLE xs)   = Ty.TUPLE $ substitute matches |$> xs
    substitute matches (Ty.CONS x)     = Ty.CONS $ substitute matches x
    substitute _       ty              = ty

-----------------------------------------------------------------------
-- localLAMPush
-----------------------------------------------------------------------

localLAMPush :: (PM, Type) -> Lambda a -> Lambda a
localLAMPush (pm, ty) lam = do
    newctx <- patternMatch (pm, ty)
    let newbindnames = pdfoldMap bindname pm
    localContextPush (M.fromList newctx) $ localBindVarsPush newbindnames lam

bindname :: PM -> [Name]
bindname (PM.VAR name _) = [name]
bindname _               = []

patternMatch :: (PM, Type) -> Lambda [(Name, Type)]
patternMatch (pm,                     Ty.UNIT)          = (*:) $ (pdfoldMap bindname pm) <$|(,)|* Ty.UNIT
patternMatch (PM.VAR name _,          ty)               = (*:) [(name, ty)]
patternMatch (PM.CONS a d msp,        Ty.CONS ty)       = localMSPBy msp $ patternMatch (a, ty) <$|(++)|*> patternMatch (d, Ty.CONS ty) 
patternMatch (PM.TUPLE pms msp,       Ty.TUPLE tys)     = localMSPBy msp $ foldlM (\acc (pm, ty) -> (acc++)|$> patternMatch (pm, ty)) [] (zip pms tys)
patternMatch (PM.TAG tagname pms msp, Ty.DATA typename) = localMSPBy msp $ do
    mtys <- lookupTypeDef typename tagname
    case mtys of
      Nothing  -> error $ "patternMatch: TAG: nothing: lookup " ++ show typename ++" "++ show tagname
      Just tys -> foldlM (\acc (pm, ty) -> (acc++)|$> patternMatch (pm, ty)) [] (zip pms tys)
patternMatch _ = (*:) []

