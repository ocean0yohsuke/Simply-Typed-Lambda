module Lambda.Compiler.Restore where

import MonadX.Applicative
import MonadX.Monad

import Lambda.Action
import Lambda.Convertor
import qualified Lambda.DataType.PatternMatch as PM
import qualified Lambda.DataType.Type as Ty
import qualified Lambda.DataType.Expr as E
import Lambda.DataType
import Lambda.Util
import qualified Util.LISP as L

-- for debug
import Debug.Trace

----------------------------------------------------------------------
-- restore pattern match
----------------------------------------------------------------------

restorePM :: PM -> Lambda (PM, BindVars)
restorePM (PM.VAR name msp) = localMSPBy msp $ do
    binds <- askBindVars
    frees <- getFreeVars
    if name `elem` frees
    then do
        newvar <- newVar 
        (*:) (PM.VAR newvar msp, newvar:binds)
    else (*:) (PM.VAR name msp, name:binds) 
  where
    newVar :: Lambda String
    newVar = do
        frees <- getFreeVars
        (*:) $ (name++) |$> semiInfinitePostfixes
               >- filter (\x -> not (x `elem` frees))
               >- head
restorePM pm = case pm of
    PM.CONS _ _ msp     -> localMSPBy msp $ do
        (pms, binds) <- rec (L.toList pm) []
        (*:) $ (L.fromList pms, binds)
    PM.TUPLE pms msp    -> localMSPBy msp $ do
        (pms, binds) <- rec pms []
        (*:) $ (PM.TUPLE pms msp, binds)
    PM.TAG name pms msp -> localMSPBy msp $ do
        (pms, binds) <- rec pms []
        (*:) $ (PM.TAG name pms msp, binds)
    _ -> do
        binds <- askBindVars
        (*:) (pm, binds)
  where
    rec :: [PM] -> [PM] -> Lambda ([PM], BindVars)
    rec []       pms = do
        binds <- askBindVars
        (*:) (pms, binds)
    rec (pm:pms) pms' = do
        (pm', binds) <- restorePM pm
        localBindVars (const binds) $ rec pms (pms'++[pm'])


