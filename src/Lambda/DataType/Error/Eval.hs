module Lambda.DataType.Error.Eval where

import Lambda.DataType.Common
import MonadX.Monad.Error

--------------------------------------------------
-- Data
--------------------------------------------------

data EvalError = VOID
               | OTHER String

instance Show EvalError where
    show VOID        = "Void returned."
    show (OTHER mes) = mes 

instance Error EvalError where
    strMsg s = OTHER s

