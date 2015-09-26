module Lambda.Evaluator.Eval.Util (
    gointoCaseRoute,
) where

import MonadX.Applicative
import MonadX.Monad

import Lambda.Action
import Lambda.Compiler
import Lambda.DataType
import qualified Lambda.DataType.PatternMatch as PM
import qualified Lambda.DataType.Type as Ty
import Lambda.Util

----------------------------------------------------------------------
-- gointoCaseRoute
----------------------------------------------------------------------

gointoCaseRoute :: Term -> [(PM, Term)] -> Lambda Term
gointoCaseRoute x ((pm,t):pairs) = do
    if x == convert pm
    then (*:) t
    else case pairs of
            [] -> do e <- restore x 
                     throwEvalError $ strMsg $ "CASE: unexhausted pattern: "++ show e
            _  -> gointoCaseRoute x pairs



