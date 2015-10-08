module Lambda.Compiler.Desugar where

import DeepControl.Applicative
import DeepControl.Monad

import Lambda.Action
import Lambda.Convertor
import qualified Lambda.DataType.PatternMatch as PM
import qualified Lambda.DataType.Type as Ty
import qualified Lambda.DataType.SExpr as SE
import Lambda.DataType
import Lambda.Util

-- for debug
import Debug.Trace 

----------------------------------------------------------------------
-- makeLAMFold
----------------------------------------------------------------------

makeLAMFold :: (PM, Type, SExpr) -> SExpr -> Lambda SExpr
makeLAMFold (pm, ty, arg) s = do
    matches <- patternMatch (pm, ty) arg
    (*:) $ foldr (\(param, arg) acc -> SE.app (SE.lam [param] acc) [arg]) s matches
  where
    patternMatch :: (PM, Type) -> SExpr -> Lambda [((PM, Type), SExpr)]
    patternMatch (PM.VAR name msp, ty) arg = (*:) [((PM.VAR name msp, ty), arg)]
    -- cons
    patternMatch (PM.CONS a d msp, ty) arg = localMSPBy msp $ case ty of
        Ty.UNIT     -> patternMatch (a, Ty.UNIT) (SE.HEAD arg) <$|(++)|*> patternMatch (d, Ty.UNIT) (SE.TAIL arg)
        Ty.CONS ty' -> patternMatch (a, ty') (SE.HEAD arg) <$|(++)|*> patternMatch (d, Ty.CONS ty') (SE.TAIL arg)
        _           -> throwDesugarError $ "patternMatch failed: "++ show ty ++" on "++ show (PM.cons a d)
    -- tuple
    patternMatch (PM.TUPLE pms msp, ty) arg = localMSPBy msp $ case ty of
        Ty.UNIT      -> fold 1 pms (repeat Ty.UNIT) arg
        Ty.TUPLE tys -> fold 1 pms tys arg
        _            -> throwDesugarError $ "patternMatch failed: "++ show ty ++" on "++ show (PM.tuple pms)
        where
          fold i []       _        _               = (*:) []
          fold i (pm:pms) (ty:tys) (SE.TUPLE xs _) = patternMatch (pm, ty) (xs!!(i-1)) <$|(++)|*> fold (i+1) pms tys arg
          fold i (pm:pms) (ty:tys) arg             = patternMatch (pm, ty) (SE.TPLPrj arg i) <$|(++)|*> fold (i+1) pms tys arg
    -- tag
    patternMatch (PM.TAG tagname pms msp, ty) arg = localMSPBy msp $ case ty of
        Ty.UNIT          -> fold 1 pms (repeat Ty.UNIT) arg
        Ty.DATA typename -> do
            mtys <- lookupTypeDef typename tagname
            case mtys of
              Nothing  -> error $ "pmType: TAG: nothing: lookup " ++ show typename ++" "++ show tagname
              Just tys -> fold 1 pms tys arg
        _                -> throwDesugarError $ "patternMatch failed: "++ show ty ++" on "++ show (PM.tag tagname pms)
        where
          fold i []       _        _   = (*:) []
          fold i (pm:pms) (ty:tys) arg = patternMatch (pm, ty) (SE.TAGPrj arg (tagname,i)) <$|(++)|*> fold (i+1) pms tys arg    
    patternMatch _ _ = (*:) []

----------------------------------------------------------------------
-- defToLambdaPairs
----------------------------------------------------------------------

defToLambdaPairs :: [PM] -> Type -> Lambda [(PM, Type)]
defToLambdaPairs []       _            = (*:) []
defToLambdaPairs (pm:pms) Ty.UNIT      = ((pm,Ty.UNIT):) |$> defToLambdaPairs pms Ty.UNIT
defToLambdaPairs (pm:pms) (ty :-> tys) = ((pm,ty):) |$> defToLambdaPairs pms tys
defToLambdaPairs (pm:pms) _            = localMSPByPM pm $ throwDesugarError $ "Too many parameters exist: "++ show pm

defsToCasePairs :: [([PM], SExpr, MSP)] -> Lambda [(PM, SExpr)]
defsToCasePairs [] = (*:) []
defsToCasePairs ((pms,s,msp):defs) = localMSPBy msp $ case pms of -- TODO: エラー処理: pms の数の一致,
    pm:[] -> ((pm,s):) |$> defsToCasePairs defs
    _     -> ((PM.TUPLE pms Nothing, s):) |$> defsToCasePairs defs

newVars :: Int -> [Name]
newVars len = ("@"++) |$> (take len semiInfinitePostfixes)

----------------------------------------------------------------------
-- localMSPByPM
----------------------------------------------------------------------

localMSPByPM :: PM -> Lambda a -> Lambda a
localMSPByPM (PM.UNIT  msp) lam = localMSPBy msp lam
localMSPByPM (PM.VAR _ msp) lam = localMSPBy msp lam
localMSPByPM _              lam = lam

----------------------------------------------------------------------
-- compensateTypeUNIT
----------------------------------------------------------------------

compensateTypeUNIT :: Type -> Lambda Type
compensateTypeUNIT Ty.UNIT = newTypeVar
compensateTypeUNIT ty      = pdmapM compensateTypeUNIT ty

-----------------------------------------------------------------------
-- newTypeVar
-----------------------------------------------------------------------

newTypeVar :: Lambda Type
newTypeVar = do
    counter <- getTypeVarCounter
    (*:) $ Ty.VAR $ "a"++ show counter 




