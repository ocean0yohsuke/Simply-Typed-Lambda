module Lambda.Evaluator.Eval.Util (
    gointoCaseRoute,
    fromBNFtoLAM, 
) where

import MonadX.Applicative
import MonadX.Monad

import Lambda.Action
import Lambda.Compiler
import Lambda.DataType
import qualified Lambda.DataType.PatternMatch as PM
import qualified Lambda.DataType.Type as Ty
import Lambda.Util

----------------------------------------------------------------------
-- gointoCaseRoute
----------------------------------------------------------------------

gointoCaseRoute :: Term -> [(PM, Term)] -> Lambda Term
gointoCaseRoute x ((pm,t):pairs) = do
    if x == convert pm
    then (*:) t
    else case pairs of
            [] -> do e <- restore x 
                     throwEvalError $ strMsg $ "CASE: unexhausted pattern: "++ show e
            _  -> gointoCaseRoute x pairs

----------------------------------------------------------------------
-- fromBNFtoLAM
----------------------------------------------------------------------

fromBNFtoLAM :: Name -> (Name, [Type]) -> Lambda (Term, Type)
fromBNFtoLAM typename (tagname, tys) = do
    lamtag <- makeLambdaTag (tagname, tys)
    let ty = makeType tys
    (*:) (lamtag, ty)
  where 
    makeLambdaTag :: (Name, [Type]) -> Lambda Term
    makeLambdaTag (tagname, tys) = do
        let vars = var |$> take (length tys) newIndexes
            tagas = tag (TAGAs tagname vars)
            pms = PM.var |$> take (length tys) newVars
        (*:) $ foldr (\(pm,ty) acc -> lam (pm, ty) acc) tagas (zip pms tys)
      where
        newIndexes = [0..]
        newVars :: [Name]
        newVars = ("x"++) |$> semiInfinitePostfixes
    makeType :: [Type] -> Type
    makeType tys = foldr (:->) (Ty.DATA typename) tys




