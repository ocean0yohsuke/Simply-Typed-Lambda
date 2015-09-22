module Lambda.Evaluator.Micro where

import MonadX.Applicative
import MonadX.Monad

import Lambda.Evaluator.Eval
import Lambda.Evaluator.LazyEval (delay)
import Lambda.Compiler 
import Lambda.Parser (readSExpr)
import Lambda.Action
import Lambda.Convertor

import Lambda.DataType
import Lambda.DataType.Error.Eval (EvalError)
import qualified Lambda.DataType.Error.Eval as EErr
import qualified Lambda.DataType.PatternMatch as PM

-- for debug
import Debug.Trace 

----------------------------------------------------------------------
-- primitive
----------------------------------------------------------------------

evalIsZero :: Term -> Lambda Term
evalIsZero (INT 0 _) = (*:) $ bool True
evalIsZero (INT _ _) = (*:) $ bool False

evalPred :: Term -> Lambda Term
evalPred (INT 0 _) = (*:) $ int 0
evalPred (INT n _) = (*:) $ int (n-1)

evalSucc :: Term -> Lambda Term
evalSucc (INT n _) = (*:) $ int (n+1)

evalEqual :: Term -> Lambda Term
evalEqual t = (*:) $ AFUNC ("("++ show t ++"==)") $ f t
  where
    f :: Term -> Term -> Lambda Term
    f t1　t2 = (*:) $ toTerm $ t1 == t2
      where
        toTerm True  = bool True
        toTerm False = bool False

----------------------------------------------------------------------
-- list
----------------------------------------------------------------------

evalHead :: Term -> Lambda Term
evalHead x = thisEval_ $ list (HEAD x)

evalTail :: Term -> Lambda Term
evalTail x = thisEval_ $ list (TAIL x)

evalCons :: Term -> Lambda Term
evalCons x = (*:) $ WAFUNC ("("++ show x ++":)") $ f x
  where
    f :: Term -> Term -> Lambda Term
    f a d = do
        cfg <- askConfig
        if isLazyMode cfg
        then list |$> (CONS |$> delay a |*> delay d)
        else (*:) $ list (CONS a d)

evalNull :: Term -> Lambda Term
evalNull (LIST NIL _) = (*:) $ bool True  
evalNull _            = (*:) $ bool False





