module Lambda.DataType.Error (
    Error(..), MonadError(..),

    LambdaError(..),
    CompileError(..),
    EvalError(..),
) where

import DeepControl.Monad.Except

import Lambda.DataType.Common
import Lambda.DataType.Error.Compile (CompileError)
import Lambda.DataType.Error.Eval (EvalError)

--------------------------------------------------
-- Data
--------------------------------------------------

data LambdaError = COMPILE MSP CompileError 
                 | EVAL MSP EvalError
                 | OTHER MSP String
                 | PARSE String

instance Error LambdaError where
    strMsg s = OTHER Nothing s

--------------------------------------------------
-- Show
--------------------------------------------------

instance Show LambdaError where
    show (COMPILE (Just sp) err) = "Compile error: " ++ (showSourcePos sp) ++ "\n" ++ show err
    show (COMPILE Nothing err)   = "Compile error: " ++ show err
    show (EVAL (Just sp) err)    = "Eval error: " ++ (showSourcePos sp) ++ "\n" ++ show err
    show (EVAL Nothing err)      = "Eval error: " ++ show err
    show (OTHER (Just sp) mes)   = "Lambda error: " ++ (showSourcePos sp) ++ "\n" ++ show mes
    show (OTHER Nothing mes)     = "Lambda error: " ++ mes
    show (PARSE mes)             = "Parse error: " ++ mes

