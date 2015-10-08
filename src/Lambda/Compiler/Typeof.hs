module Lambda.Compiler.Typeof where

import DeepControl.Applicative
import DeepControl.Monad
import MonadX.MonadTrans

import Lambda.Action
import Lambda.Convertor
import Lambda.Compiler.Desugar
import Lambda.DataType.Type
import Lambda.DataType (Lambda, Name)
import Util.Pseudo

import Prelude hiding (concat)
import Data.List (lookup, nub)
import Data.Foldable
import Data.Traversable

import Debug.Trace

-----------------------------------------------------------------------
-- typeunify
-----------------------------------------------------------------------

type TyUnify = [(Name, Type)]

typeunify :: [(Type, Type)] -> Lambda TyUnify -- TODO: (Type, Type) -> Lambda TyUnify
typeunify pairs = do
    unify <- solve pairs []
    (*:) unify
  where
    solve :: [(Type, Type)] -> TyUnify -> Lambda TyUnify
    solve []                acc = (*:) acc
    solve ((ty1,ty2):pairs) acc = do
        if ty1 == ty2 
        then solve pairs acc
        else case (ty1, ty2) of
            (ty11:->ty12, ty21:->ty22) -> solve ((ty11,ty21):(ty12,ty22):pairs) acc
            (CONS ty1, CONS ty2)       -> solve ((ty1,ty2):pairs) acc
            (TUPLE tys1, TUPLE tys2)   -> 
                if length tys1 /= length tys2
                then throwTypeofError $ "unification failed0"
                else solve ((zip tys1 tys2)++pairs) acc
            (VAR name, _) -> 
                if occurs name ty2 
                then throwTypeofError $ "unification failed1"
                else let pairs' = pairs <$| (\(l, r) -> (substTyUnifyforType [(name,ty2)] l, substTyUnifyforType [(name,ty2)] r))
                         acc' = pushTyUnify (name,ty2) acc
                     in  solve pairs' acc'
            (_, VAR name) -> 
                if occurs name ty1 
                then throwTypeofError $ "unification failed2"
                else let pairs' = pairs <$| (\(l, r) -> (substTyUnifyforType [(name,ty1)] l, substTyUnifyforType [(name,ty1)] r))
                         acc' = pushTyUnify (name,ty1) acc
                     in  solve pairs' acc'
            _ -> throwTypeofError $ "unification failed3"
      where
        occurs :: Name -> Type -> Bool
        occurs name (VAR x)       = name == x
        occurs name (ty1 :-> ty2) = occurs name ty1 || occurs name ty2
        occurs _    _             = False

substTyUnifyforType :: TyUnify -> Type -> Type
substTyUnifyforType unify (VAR name) = case lookup name unify of
    Nothing -> VAR name
    Just ty -> ty
substTyUnifyforType unify ty         = pdfmap (substTyUnifyforType unify) ty
    
pushTyUnify :: (Name, Type) -> TyUnify -> TyUnify
pushTyUnify p@(name,ty) unify = 
    let unify' = unify <$| (\(name, ty) -> (name, substTyUnifyforType [p] ty))
    in  case lookup name unify of
            Nothing -> (name,ty) : unify'
            Just _  -> unify'

composeTyUnify :: TyUnify -> TyUnify -> TyUnify
composeTyUnify []     unify = unify
composeTyUnify (p:ps) unify = 
    let unify' = pushTyUnify p unify
    in  composeTyUnify ps unify'
 
-----------------------------------------------------------------------
-- refreshTypeVars
-----------------------------------------------------------------------

refreshTypeVars :: Type -> Lambda Type
refreshTypeVars UNIT = do
    newtypevar <- newTypeVar
    (*:) newtypevar
refreshTypeVars ty = do
    let varnames = nub $ pdfoldMap takeTypeVar ty
    unify1 <- seq varnames []
    (*:) $ substTyUnifyforType unify1 ty
  where
    seq []         unify = (*:) unify
    seq (var:vars) unify = do
        newtypevar <- newTypeVar
        unify1 <- typeunify [(var, newtypevar)]
        seq vars $ unify `composeTyUnify` unify1
    takeTypeVar :: Type -> [Type]
    takeTypeVar (VAR name) = [VAR name]
    takeTypeVar (x :-> y)  = takeTypeVar x ++ takeTypeVar y
    takeTypeVar (CONS x)   = takeTypeVar x 
    takeTypeVar (TUPLE xs) = concat $ takeTypeVar |$> xs
    takeTypeVar _          = []

