module Lambda.Evaluator.Eval.EvalMacro where

import MonadX.Applicative
import MonadX.Monad

import Lambda.Action
import Lambda.Convertor
import Lambda.Compiler

import Lambda.Evaluator.Eval.BetaReduce
import Lambda.Evaluator.Eval.Util
import Lambda.DataType
import Lambda.DataType.Error.Eval (EvalError)
import qualified Lambda.DataType.PatternMatch as PM
import qualified Lambda.DataType.Type as Ty
import qualified Lambda.DataType.Error.Eval as EErr
import Lambda.Util
import Lambda.Debug

-- for debug
import Debug.Trace 


