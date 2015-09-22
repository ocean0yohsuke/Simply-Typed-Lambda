module Lambda.Evaluator.Prelude where

import MonadX.Applicative
import MonadX.Monad

import Lambda.DataType
import Lambda.Action
import Lambda.Convertor
import Lambda.Compiler

-- for debug
import Debug.Trace 

---------------------------------------------------------------------------------------
-- Arithmetic 
---------------------------------------------------------------------------------------

--
-- template 
--
arithOpr :: Name -> (Integer -> Integer -> Integer) -> Term -> Lambda Term
arithOpr name opr t = (*:) $ AFUNC ("("++ show t ++ name ++")") $ f t
  where
    f :: Term -> Term -> Lambda Term
    f (INT n1 _)　(INT n2 _) = (*:) $ int (n1 `opr` n2)
    -- f x y = error $ show x ++ ", " ++ show y
compareOpr :: Name -> (Integer -> Integer -> Bool) -> Term -> Lambda Term
compareOpr name opr t = (*:) $ AFUNC ("("++ show t ++ name ++")") $ f t
  where
    f :: Term -> Term -> Lambda Term
    f (INT n1 _)　(INT n2 _) = (*:) $ bool (n1 `opr` n2)

evalPlus, evalMinus, evalMult, evalPower, evalDiv, evalMod :: Term -> Lambda Term
evalPlus = arithOpr "+" (+)
evalMinus = arithOpr "-" (-)
evalMult = arithOpr "*" (*)
evalPower = arithOpr "^" (^)
evalDiv (INT n1 msp) = localMSPBy msp $ (*:) $ AFUNC ("("++ show n1 ++ "/)") $ f n1
  where
    f :: Integer -> Term -> Lambda Term
    f n1　(INT 0 msp) = localMSPBy msp $ throwEvalError $ strMsg $ "devide by zero"
    f n1　(INT n2 _)  = (*:) $ int (div n1 n2)
--evalDiv x = error $ "x: " ++ show x
evalMod (INT n1 msp) = localMSPBy msp $ (*:) $ AFUNC ("(mod "++ show n1 ++ ")") $ f n1
  where
    f :: Integer -> Term -> Lambda Term
    f n1　(INT 0 msp) = localMSPBy msp $ throwEvalError $ strMsg $ "devide by zero"
    f n1　(INT n2 _)  = (*:) $ int (mod n1 n2)

evalLtNum, evalGtNum, evalLtEq, evalGtEq :: Term -> Lambda Term
evalLtNum = compareOpr "<" (<)
evalGtNum = compareOpr ">" (>)
evalLtEq = compareOpr "<=" (<=)
evalGtEq = compareOpr ">=" (>=)

---------------------------------------------------------------------------------------
-- other 
---------------------------------------------------------------------------------------

evalShow :: Term -> Lambda Term
evalShow t = do
    e <- restore t
    (*:) $ list (fromList $ char |$> show e)






