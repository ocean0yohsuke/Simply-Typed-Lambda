module Lambda.Evaluator.Eval.BetaReduce (
    betaReduce, betaReducePM,

    shift, substitute
) where

import MonadX.Applicative
import MonadX.Monad hiding (foldl, foldr)

import Lambda.Action
import Lambda.Convertor
import Lambda.DataType
import qualified Lambda.DataType.PatternMatch as PM

import Prelude hiding (foldl, foldr)
import Data.Foldable
import Data.Traversable

----------------------------------------------------------------------
-- betaReduce
----------------------------------------------------------------------

-- | [ var -> arg ] term
substitute :: Index -> Term -> Term -> Term
substitute var arg (VAR n msp) 
    | n == var  = arg
    | otherwise = VAR n msp
substitute var arg (LAM p@(pm,_) t _)  = lam p $ case pm of
    PM.UNIT _ -> substitute var arg t
    _         -> substitute (var+1) (shift 1 0 arg) t
substitute var arg (LAMM p@(pm,_) t _) = lamm p $ case pm of
    PM.UNIT _ -> substitute var arg t
    _         -> substitute (var+1) (shift 1 0 arg) t
substitute var arg (QUT (QQUOTE x) msp) = QUT (QUOTE (unquote x)) msp
  where
    unquote :: Term -> Term
    unquote (QUT (UNQUOTE t@(VAR _ _)) _) = substitute var arg t
    unquote x                             = pdfmap unquote x
{-
substitute var arg (QUT qut msp) = QUT (case qut of
    QUOTE x   -> qut
    QQUOTE x  -> QQUOTE (unquote x)
    UNQUOTE x -> error $ "betaReduce: substitute: UNQUOTE: "++ show qut
    ) msp
  where
    unquote :: Term -> Term
    unquote (QUT (UNQUOTE t@(VAR _ _)) _) = substitute var arg t
    unquote x                             = pdfmap unquote x
-}
--substitute var arg t@(THUNK _)  = t　
substitute var arg t = pdfmap (substitute var arg) t

-- | ↑　d cutoff term
shift :: Index -> Index -> Term -> Term
shift d cutoff (VAR n msp) 
    | n < cutoff  = VAR n msp
    | n >= cutoff = VAR (n+d) msp
shift d cutoff (LAM p@(pm,_) t _)  = lam p $ shift d (cutoff+1) t
shift d cutoff (LAMM p@(pm,_) t _) = lamm p $ shift d (cutoff+1) t
shift d cutoff t = pdfmap (shift d cutoff) t

betaReduce :: Term -> Term -> Term
betaReduce sub target = target >- substitute 0 (shift 1 0 sub) 
                               >- shift (-1) 0 

----------------------------------------------------------------------
-- betaReducePM
----------------------------------------------------------------------

betaReducePM :: (PM, Term) -> Term -> Lambda Term -- (Term, BindVars)
betaReducePM (pm, arg) t = do
    matches <- patternMatch (pm, arg)
    (*:) $ foldl (\acc (_, t) -> betaReduce t acc) t matches

patternMatch :: (PM, Term) -> Lambda [(PM, Term)]
patternMatch (PM.VAR name msp, arg) = (*:) [(PM.VAR name msp, arg)]
patternMatch (PM.CONS a d _,   arg) = patternMatch (a, list (HEAD arg)) <$|(++)|*> patternMatch (d, list (TAIL arg))
patternMatch (PM.TUPLE pms _, arg) = fold 1 pms arg
  where
    fold i []       _   = (*:) []
    fold i (pm:pms) arg = patternMatch (pm, tpl (TPLPrj arg i)) <$|(++)|*> fold (i+1) pms arg
patternMatch (PM.TAG name pms _, arg) = fold 1 pms arg
  where
    fold i []       _   = (*:) []
    fold i (pm:pms) arg = patternMatch (pm, tag (TAGPrj arg (name,i))) <$|(++)|*> fold (i+1) pms arg    
patternMatch _ = (*:) []
-- patternMatch x = error $ "patternMatch: Non-exhaustive patterns: "++ show x

