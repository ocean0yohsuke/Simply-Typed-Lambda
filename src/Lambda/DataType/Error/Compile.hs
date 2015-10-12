module Lambda.DataType.Error.Compile where

import DeepControl.Monad.Except

import Lambda.DataType.Common

--------------------------------------------------
-- Data
--------------------------------------------------

data CompileError = DESUGAR String
                  | RESTORE String
                  | TYPEOF String
                  | OTHER String

instance Error CompileError where
    strMsg s = OTHER s

instance Show CompileError where
    show (DESUGAR mes) = "Desugar error: "++ mes
    show (RESTORE mes) = "Restore error: "++ mes
    show (TYPEOF mes)  = "Typeof error: "++ mes
    show (OTHER mes)   = mes 


