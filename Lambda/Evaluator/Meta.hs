module Lambda.Evaluator.Meta where

import MonadX.Applicative
import MonadX.Monad

import Lambda.Evaluator.Eval
import Lambda.Evaluator.LazyEval
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

iterateM :: (Applicative m, Monad m) => Int -> (a -> m a) -> a -> m a
iterateM 0 f = return
iterateM n f = f >=> (iterateM (n-1) f)

----------------------------------------------------------------------
-- eval
----------------------------------------------------------------------

evalEval :: Term -> Lambda ReturnT
evalEval t = t >- eval

evalEvalN :: Term -> Lambda ReturnT
evalEvalN int@(INT n _) = (*:) $ RETURN $ META ("(evalN "++ show int ++")") $ fromInteger n >- f
  where
    f :: Int -> Term -> Lambda ReturnT
    f n　t = do
        x <- t >- (iterateM n eval1)
        e <- restore x
        liftIO $ putStrLn $ show e  
        (*:) VOID

evalLazyEval :: Term -> Lambda ReturnT
evalLazyEval t = t >- lazyEval

evalLazyEvalN :: Term -> Lambda ReturnT
evalLazyEvalN int@(INT n _) = (*:) $ RETURN $ META ("(lazyEvalN "++ show int ++")") $ fromInteger n >- f
  where
    f :: Int -> Term -> Lambda ReturnT
    f n　t = do
        x <- t >- (iterateM n lazyEval1)
        e <- restore x
        liftIO $ putStrLn $ show e  
        (*:) VOID

evalTypeof :: Term -> Lambda ReturnT
evalTypeof t = do
    ty <- t >- typeofTerm
    liftIO $ putStrLn $ show ty
    (*:) VOID

evalMacroExpand :: Term -> Lambda Term
evalMacroExpand t = t >- evalMacro

----------------------------------------------------------------------
-- compile
----------------------------------------------------------------------

evalDesugar :: Term -> Lambda ReturnT
evalDesugar t = do
    case readSExpr (show t >- (init >-> tail)) of
      Left err           -> liftIO $ putStrLn $ "evalDesugar: "++ show err
      Right (se, [])   -> do
          e <- se >- desugarSExpr
          liftIO $ putStrLn $ show e
      Right (expr, rest) -> liftIO $ putStrLn $ "evalDesugar: rest: "++ rest
    (*:) VOID  

evalToTerm :: Term -> Lambda ReturnT
evalToTerm t = do
    case readSExpr (show t >- (init >-> tail)) of
      Left err           -> liftIO $ putStrLn $ "evalToTerm: "++ show err
      Right (se, [])     -> do
          t <- se >- (desugarSExpr >=> desugarExpr >=> toTerm)
          liftIO $ putStrLn $ show t
      Right (expr, rest) -> liftIO $ putStrLn $ "evalToTerm: rest: "++ rest
    (*:) VOID  

evalRestore :: Term -> Lambda ReturnT
evalRestore t = do
    case readSExpr (show t >- (init >-> tail)) of
        Left err           -> liftIO $ putStrLn $ "evalRestore: "++ show err
        Right (se, [])     -> do
            e <- se >- (desugarSExpr >=> desugarExpr >=> toTerm >=> restore)
            liftIO $ putStrLn $ show e
        Right (expr, rest) -> liftIO $ putStrLn $ "evalRestore: rest: "++ rest
    (*:) VOID  

