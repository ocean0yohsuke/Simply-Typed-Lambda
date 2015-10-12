module Lambda.Evaluator.Debug where

import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.MonadTrans

import Lambda.Evaluator.Eval
import Lambda.Compiler 
import Lambda.Parser (readSExpr)
import Lambda.Action
import Lambda.Convertor
import Lambda.Debug

import Lambda.DataType
import Lambda.DataType.Error.Eval (EvalError)
import qualified Lambda.DataType.Error.Eval as EErr
import qualified Lambda.DataType.PatternMatch as PM

-- for debug
import Debug.Trace 

----------------------------------------------------------------------
-- unittest
----------------------------------------------------------------------

evalUnitTest :: Term -> Lambda ReturnT
evalUnitTest name = (*:) $ RETURN $ META ("(evalUnitTest "++ show name ++")") $ f (show name)
  where
    f :: Name -> Term -> Lambda ReturnT
    f name　unit@(LIST g msp) = localMSPBy msp $ do
        let xs = toList g
        str <- rec (length xs, 0, 0, 0, "") $ xs <$| \(TPL (TUPLE [expr, answer]) msp) -> (expr, answer, msp)
        liftIO $ putStrLn str
        (*:) VOID
      where
        rec :: (Int, Int, Int, Int, String) -> [(Term, Term, MSP)] -> Lambda String
        rec (cases, tried, 0, 0, "") [] = (*:) $ 
            "unittest ["++ name ++"] - "++ 
            "Cases: "++ show cases ++"  "++ 
            "Tried: "++ show tried ++"  "++ 
            "Errors: 0  "++ 
            "Failures: 0"
        rec (cases, tried, errors, failures, mes) [] = (*:) $ 
            "////////////////////////////////////////////////////////////////////"++"\n"++
            "/// - unittest ["++ name ++"]"++"\n"++
            mes++
            "/// - "++
                "Cases: "++ show cases ++"  "++
                "Tried: "++ show tried ++"  "++
                "Errors: "++ show errors ++"  "++
                "Failures: "++ show failures ++"\n"++
            "/// - unittest ["++ name ++"]"++"\n"++
            "////////////////////////////////////////////////////////////////////"
        rec (cases, tried, errors, failures, mes) ((term, answer, msp):xs) = localMSPBy msp $ catch $ do 
            expr <- restore term
            v <- thisEval_ term
            if show v /= show answer
            then do
                answer <- restore answer
                v <- restore v
                let str1 = "/// ### failured in: "++ name ++": at "++ showMSP msp ++"\n"
                    str2 = "///    tried: "++ show expr ++"\n"++ 
                           "/// expected: "++ show answer ++"\n"++ 
                           "///  but got: "++ show v ++"\n"
                rec (cases, tried+1, errors, failures+1, mes ++ str1 ++ str2) xs     
            else do
                rec (cases, tried+1, errors, failures, mes) xs
          where
            catch x = x `catchError` \e -> do
                let str = "/// ### errored in: "++ name ++": at "++ showMSP msp ++"\n"
                          ++ (show e >- lines 
                                     >- foldr (\line acc -> "/// "++ line ++"\n"++ acc) "") ++"/// \n"
                rec (cases, tried+1, errors+1, failures, mes ++ str) xs
            showMSP :: MSP -> String
            showMSP Nothing   = "---"
            showMSP (Just sp) = show sp

---------------------------------------------------------------------------------------
-- show
---------------------------------------------------------------------------------------

evalShowSExpr :: Term -> Lambda ReturnT
evalShowSExpr t = do
    case readSExpr (show t) of
      Left err        -> liftIO $ putStrLn $ "evalDesugar: "++ show err
      Right (s, [])   -> liftIO $ putStrLn $ show (convert s :: SExpr_)
      Right (s, rest) -> liftIO $ putStrLn $ "evalDesugar: rest: "++ rest
    (*:) VOID

evalShowExpr :: Term -> Lambda ReturnT
evalShowExpr t = do
    case readSExpr (show t) of
      Left err           -> liftIO $ putStrLn $ "evalDesugar: "++ show err
      Right (s, [])   -> do
          e <- s >- desugarSExpr
          liftIO $ putStrLn $ show (convert e :: Expr_)
      Right (s, rest) -> liftIO $ putStrLn $ "evalDesugar: rest: "++ rest
    (*:) VOID

---------------------------------------------------------------------------------------
-- misc
---------------------------------------------------------------------------------------

evalShowContext :: Lambda ReturnT
evalShowContext = do
    showContext "---"
    (*:) VOID

evalShowDef :: Lambda ReturnT
evalShowDef = do
    showDef "---"
    (*:) VOID
evalShowDef_ :: Lambda ReturnT
evalShowDef_ = do
    showDef_ "---"
    (*:) VOID



