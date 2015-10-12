module Lambda.DataType.Error.Eval where

import DeepControl.Monad.Except

import Lambda.DataType.Common

--------------------------------------------------
-- Data
--------------------------------------------------

data EvalError = VOID
               | OTHER String

instance Error EvalError where
    strMsg s = OTHER s

instance Show EvalError where
    show VOID        = "Void returned."
    show (OTHER mes) = mes 


