module Lambda.Debug where

import DeepControl.Applicative
import DeepControl.Monad

import Lambda.DataType
import Lambda.Action
import Lambda.Convertor

import qualified Data.Map as M

-- for debug
import Debug.Trace 

showDef :: String -> Lambda ()
showDef label = do
    genv <- getDef
    liftIO $ putStrLn $ label ++": [Def]: "++ show genv
showDef_ :: String -> Lambda ()
showDef_ label = do
    genv <- getDef
    liftIO $ putStrLn $ label ++": [Def]: "++ show (zip [0..] (fst |$> genv))

showContext :: String -> Lambda ()
showContext label = do
    ctx <- askContext
    liftIO $ putStrLn $ label ++": [Context]: "++ show ctx


