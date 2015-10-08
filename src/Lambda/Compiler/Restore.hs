module Lambda.Compiler.Restore where

import DeepControl.Applicative
import DeepControl.Monad

import Lambda.Action
import Lambda.Convertor
import qualified Lambda.DataType.PatternMatch as PM
import qualified Lambda.DataType.Type as Ty
import qualified Lambda.DataType.Expr as E
import Lambda.DataType
import Lambda.Util
import qualified Util.LISP as L

-- for debug
import Debug.Trace

----------------------------------------------------------------------
-- restore pattern match
----------------------------------------------------------------------

restorePM :: PM -> Lambda PM
restorePM (PM.VAR name msp) = localMSPBy msp $ do
    genv <- getDef
    let freevars = fst |$> genv
    if name `elem` freevars
    then do
        let newvar = newVar freevars
        (*:) $ PM.VAR newvar msp
    else (*:) $ PM.VAR name msp
  where
    newVar :: [Name] -> String
    newVar frees = (name++) |$> semiInfinitePostfixes
                   >- filter (\x -> not (x `elem` frees))
                   >- head
restorePM pm = (*:) pm

