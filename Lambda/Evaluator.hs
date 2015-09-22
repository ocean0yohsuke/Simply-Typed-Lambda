module Lambda.Evaluator where

import MonadX.Applicative
import MonadX.Monad

import Config
import Lambda.Evaluator.Eval
import Lambda.Evaluator.LazyEval
import Lambda.Evaluator.GEnv (initGEnv)
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
initEnv = LambdaEnv initBindVars initContext initMSP initConfig

initStates :: LambdaStates
initStates = (initFreeVars, initGEnv, initTypeDef)
  
-------------------------------------
-- init
-------------------------------------

initBindVars :: BindVars
initBindVars = []
initContext :: Context
initContext = M.empty
initMSP :: MSP
initMSP = Nothing

initFreeVars :: FreeVars
initFreeVars = []

initTypeDef :: TypeDef
initTypeDef = M.empty

-------------------------------------
-- eval
-------------------------------------

runLambdaE :: (LambdaEnv, LambdaStates) -> SExpr -> IO (Either LambdaError ReturnE, LambdaStates, ())
runLambdaE (env, states) s = runLambda (returnE s) env states

runLambdaT :: (LambdaEnv, LambdaStates) -> SExpr -> IO (Either LambdaError ReturnT, LambdaStates, ())
runLambdaT (env, states) s = runLambda (returnT s) env states

