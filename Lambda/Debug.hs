module Lambda.Debug where

import MonadX.Applicative
import MonadX.Monad

import Lambda.DataType
import Lambda.Action
import Lambda.Convertor

-- for debug
import Debug.Trace 

showEnv :: String -> Lambda ()
showEnv label = do
    binds <- askBindVars
    frees <- getFreeVars
    liftIO $ putStrLn $ label ++": [BindVars]: "++ show binds
    liftIO $ putStrLn $ label ++": [FreeVars]: "++ show frees


showContext :: String -> Lambda ()
showContext label = do
    ctx <- askContext
    liftIO $ putStrLn $ label ++": [Context]: "++ show ctx


