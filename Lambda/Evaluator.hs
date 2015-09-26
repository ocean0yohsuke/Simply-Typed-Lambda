module Lambda.Evaluator (
    runDefault, 
    runInterpretE, runInterpretT,
    interpretT, interpretE,

    initEnv, initStates, 
) where

import MonadX.Applicative
import MonadX.Monad

import Config
import Lambda.Evaluator.Eval
import Lambda.Evaluator.LazyEval
import Lambda.Evaluator.Def (initDef)
import Lambda.Compiler 
import qualified Lambda.DataType.Type as Ty
import Lambda.DataType

import qualified Data.Map as M
import Data.Traversable as Trav

-- for debug
import Debug.Trace 

-------------------------------------
-- init
-------------------------------------

initEnv :: LambdaEnv
initEnv = LambdaEnv initContext initMSP initConfig
  where
    initContext :: Context
    initContext = []
    initMSP :: MSP
    initMSP = Nothing

initStates :: LambdaStates
initStates = (initDef, initTypeDef, initTypeVarCounter)
  where
    initTypeDef :: TypeDef
    initTypeDef = M.empty
    initTypeVarCounter :: TypeVarCounter
    initTypeVarCounter = 1

-------------------------------------
-- runDefault
-------------------------------------

runDefault :: Lambda a -> IO (Either LambdaError a, LambdaStates, ())
runDefault lam = runLambda lam initEnv initStates

runInterpretT :: (LambdaEnv, LambdaStates) -> SExpr -> IO (Either LambdaError ReturnT, LambdaStates, ())
runInterpretT (env, states) s = runLambda (s >- (compile >=> thisEval)) env states

runInterpretE :: (LambdaEnv, LambdaStates) -> SExpr -> IO (Either LambdaError ReturnE, LambdaStates, ())
runInterpretE (env, states) s = runLambda (s >- (compile >=> thisEval >=> \return -> 
                                    case return of
                                      VOID     -> (*:) VOID
                                      RETURN t -> RETURN |$> restore t)) env states
{-
runInterpretT :: (LambdaEnv, LambdaStates) -> SExpr -> IO (Either LambdaError ReturnT, LambdaStates, ())
runInterpretT (env, states) s = do
    (mv, s, w) <- runLambda (compile s) env states
    case mv of 
      Left err   -> (*:) (Left err, s, w)
      Right term -> runLambda (thisEval term) env states
runInterpretE :: (LambdaEnv, LambdaStates) -> SExpr -> IO (Either LambdaError ReturnE, LambdaStates, ())
runInterpretE (env, states) s = do
    (mv, s, w) <- runLambda (compile s) env states
    case mv of 
      Left err   -> (*:) (Left err, s, w)
      Right term -> runLambda (thisEval term >>= \return -> 
                        case return of
                          VOID     -> (*:) VOID
                          RETURN t -> RETURN |$> restore t) env states
-}

