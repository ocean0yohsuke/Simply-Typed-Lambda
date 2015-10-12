{-# LANGUAGE BangPatterns #-}
module Lambda.Evaluator.IO where

import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.MonadTrans (liftIO)

import Lambda.Evaluator.Eval
import Lambda.Action
import Lambda.Convertor
import Lambda.Compiler

import Lambda.DataType
import qualified Lambda.DataType.Type as Ty
import qualified Lambda.DataType.PatternMatch as PM

import System.FilePath ((</>))

-- for debug
import Debug.Trace 

-- import System.IO.Unsafe (unsafePerformIO)  -- TODO: 

-------------------------------------------------------------
-- 
-------------------------------------------------------------

evalImport :: Term -> Lambda ReturnT
evalImport t = do
    let filename = show t >- (init >-> tail)
    config <- askConfig
    let loadpath = startDir config
    mv <- liftIO $ loadFile (loadpath </> filename)
    case mv of
      Left err    -> throwParseError $ show err
      Right codes -> do
        mapM_ (compile >=> thisEval) codes
        (*:) VOID

-------------------------------------------------------------
-- 
-------------------------------------------------------------

evalPrint :: Term -> Lambda Term
evalPrint t = do
    liftIO $ print t
    (*:) NULL

{-
evalSeq :: Term -> Lambda Term 
evalSeq x = (*:) $ AFUNC ("("++ show x ++" >>)") $ f x
  where
    f :: Term -> Term -> Lambda Term
    f _ _ = (*:) NULL
-}


