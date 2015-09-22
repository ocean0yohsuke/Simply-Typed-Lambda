module Lambda.Convertor.Util where

import MonadX.Applicative
import MonadX.Monad hiding (forM, mapM)

import Lambda.DataType 
import Lambda.DataType.Expr
import qualified Lambda.DataType.PatternMatch as PM
import Lambda.Action
import Lambda.Convertor.PatternMatch


localLAMPushPM :: PM -> Lambda a -> Lambda a
localLAMPushPM pm lam = do
    let newbindnames = pdfoldMap bindname pm
    localBindVarsPush newbindnames lam
  where
    bindname :: PM -> [String]
    bindname (PM.VAR name _) = [name]
    bindname _            = []


