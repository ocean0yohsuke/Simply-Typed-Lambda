{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module Lambda.Action where

import DeepControl.Applicative
import DeepControl.Monad
import MonadX.Monad.RWS
import MonadX.Monad.Error

import Lambda.Parser (readSFile, ParseError)
import Lambda.DataType
import qualified Lambda.DataType.Type as Ty
import qualified Lambda.DataType.Error.Compile as CE
import qualified Lambda.DataType.Error.Eval as EE

import Data.List (lookup, elemIndex)
import qualified Data.Map as M
import System.IO 

-- for debug
import Debug.Trace

----------------------------------------------------------------------
-- particular Monad function
----------------------------------------------------------------------

--
-- MonadError
--
throwLambdaError :: LambdaError -> Lambda a
throwLambdaError (COMPILE Nothing err)   = do
    msp <- askMSP
    throwError $ COMPILE msp err
throwLambdaError (COMPILE (Just sp) err) = Lambda . throwError $ COMPILE (Just sp) err
throwLambdaError (EVAL Nothing err)      = do
    msp <- askMSP
    throwError $ EVAL msp err
throwLambdaError (EVAL (Just sp) err)    = Lambda . throwError $ EVAL (Just sp) err
throwLambdaError (OTHER Nothing err)     = do
    msp <- askMSP
    throwError $ OTHER msp err
throwLambdaError (OTHER (Just sp) err)   = Lambda . throwError $ OTHER (Just sp) err
throwLambdaError (PARSE err)             = Lambda . throwError $ PARSE err

--
-- Error
--
throwCompileError :: CompileError -> Lambda a
throwCompileError err = throwLambdaError $ COMPILE Nothing err
throwEvalError :: EvalError -> Lambda a
throwEvalError err = throwLambdaError $ EVAL Nothing err
throwOtherError :: String -> Lambda a
throwOtherError mes = throwLambdaError $ OTHER Nothing mes
throwParseError :: String -> Lambda a
throwParseError mes = throwLambdaError $ PARSE mes

catchVoid :: Return a -> Lambda a
catchVoid VOID       = throwEvalError EE.VOID
catchVoid (RETURN x) = (*:) x
{-
catchVoid_ :: Return Term -> Lambda (Return Term)
catchVoid_ VOID       = (*:) $ Char ''
catchVoid_ (RETURN x) = (*:) x
-}

--
-- Compile Error
--
throwDesugarError :: String -> Lambda a
throwDesugarError mes = throwCompileError $ CE.DESUGAR mes
throwRestoreError :: String -> Lambda a
throwRestoreError mes = throwCompileError $ CE.RESTORE mes
throwTypeofError :: String -> Lambda a
throwTypeofError mes = throwCompileError $ CE.TYPEOF mes

--
-- LambdaEnv
--
askContext :: Lambda Context
askContext = context |$> ask
localContext :: (Context -> Context) -> Lambda a -> Lambda a
localContext f = local (\env -> setContext (f (context env)) env)
localContextBy :: Context -> Lambda a -> Lambda a
localContextBy ctx = localContext (const ctx)

askMSP :: Lambda MSP
askMSP = msp |$> ask
localMSP :: (MSP -> MSP) -> Lambda a -> Lambda a
localMSP f = local (\env -> setMSP (f (msp env)) env)
localMSPBy :: MSP -> Lambda a -> Lambda a
localMSPBy x = localMSP (const x)

askConfig :: Lambda Config
askConfig = config |$> ask
localConfig :: (Config -> Config) -> Lambda a -> Lambda a
localConfig f = local (\env -> setConfig (f (config env)) env)
localConfigBy :: Config -> Lambda a -> Lambda a
localConfigBy x = localConfig (const x)

--
-- LambdaStates
--
getDef :: Lambda Def
getDef = do
    (x,_,_) <- get
    (*:) x
getTypeDef :: Lambda TypeDef
getTypeDef = do
    (_,x,_) <- get
    (*:) x
getTypeVarCounter :: Lambda TypeVarCounter
getTypeVarCounter = do
    (a,b,x) <- get
    put (a,b,x+1)
    (*:) x

putDef :: Def -> Lambda ()
putDef x = do
    (_,b,c) <- get
    put (x,b,c) 
putTypeDef :: TypeDef -> Lambda ()
putTypeDef x = do
    (a,_,c) <- get
    put (a,x,c)

----------------------------------------------------------------------
-- 
----------------------------------------------------------------------

--
-- Context
--
lookupContext :: Name -> Lambda (Maybe (Type, Term))
lookupContext name = do
    ctx <- askContext
    (*:) $ lookup name ctx

--
-- Def
--
lookupDef :: Name -> Lambda (Maybe (Type, Term))
lookupDef name = do
    genv <- getDef
    (*:) $ lookup name genv
insertDef :: Name -> (Type, Term) -> Lambda ()
insertDef name pair = do
    genv <- getDef
    case elemIndex name (fst |$> genv) of
      Nothing -> do
        genv <- getDef
        putDef $ genv ++ [(name, pair)]
      Just n  -> do
        let (l,r) = splitAt n genv
        putDef $ l ++ ((name, pair) : (tail r))

--
-- TypeDef
--
lookupTypeDef :: Name -> Name -> Lambda (Maybe [Type])
lookupTypeDef typename tagname = do
    td <- getTypeDef
    case M.lookup typename td of
      Nothing   -> (*:) Nothing
      Just tags -> (*:) $ lookup tagname tags
pushTypeDef :: Name -> [(Name, [Type])] -> Lambda ()
pushTypeDef name tags = do
    td <- getTypeDef
    putTypeDef $ M.insert name tags td
   
----------------------------------------------------------------------
-- de Bruijn index
----------------------------------------------------------------------

nameToIndex :: Name -> Lambda (Maybe Index)
nameToIndex name = do
    ctx <- askContext
    case elemIndex name (fst |$> ctx) of
      Just n  -> (*:) $ Just n
      Nothing -> do
        genv <- getDef
        case elemIndex name (fst |$> genv) of
          Just n  -> (*:) $ Just $ length ctx + n  
          Nothing -> (*:) Nothing

indexToName :: Int -> Lambda (Maybe Name)
indexToName index = do
    ctx <- askContext
    if length ctx > index
    then (*:) $ Just $ fst (ctx !! index)
    else do
        genv <- getDef
        let index' = index - length ctx 
        if length genv > index'
        then (*:) $ Just $ fst (genv !! index')
        else (*:) Nothing

indexToType :: Int -> Lambda (Maybe Type)
indexToType index = do
    ctx <- askContext
    if length ctx > index
    then (*:) $ Just $ (fst . snd) $ ctx !! index
    else do
        genv <- getDef
        let index' = index - length ctx
        if length genv > index'
        then (*:) $ Just $ (fst . snd) $ genv !! index'
        else (*:) Nothing

indexToTerm :: Index -> Lambda (Maybe Term)
indexToTerm index = do
    ctx <- askContext
    if length ctx > index
    then (*:) $ Just $ (snd . snd) (ctx !! index)
    else do
        genv <- getDef
        let index' = index - length ctx 
        if length genv > index'
        then (*:) $ Just $ (snd . snd) (genv !! index')
        else (*:) Nothing

----------------------------------------------------------------------------------------------------------
-- IO action
----------------------------------------------------------------------------------------------------------

loadFile :: Filename -> IO (Either ParseError [SExpr])
loadFile path = 
    withFile path ReadMode $ \h -> do
        contents <- hGetContents h
        case readSFile path contents of
            Left err         -> (*:) $ Left err
            Right (codes, _) -> (**:) $ filter isEXPR codes <$| (\(EXPR e) -> e)
  where
    isEXPR (EXPR _) = True
    isEXPR _        = False


