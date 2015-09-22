{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module Lambda.Action where

import MonadX.Applicative
import MonadX.Monad
import MonadX.Monad.RWS
import MonadX.Monad.Error

import Lambda.Parser (readSFile, ParseError)
import Lambda.DataType
import qualified Lambda.DataType.Type as Ty
import qualified Lambda.DataType.Error.Compile as CE
import qualified Lambda.DataType.Error.Eval as EE

import Data.List (lookup, elemIndex)
import System.IO 
import qualified Data.Map as M

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
askBindVars :: Lambda BindVars
askBindVars = bindVars |$> ask
localBindVars :: (BindVars -> BindVars) -> Lambda a -> Lambda a
localBindVars f = local (\env -> setBindVars (f (bindVars env)) env)

askContext :: Lambda Context
askContext = context |$> ask
localContext :: (Context -> Context) -> Lambda a -> Lambda a
localContext f = local (\env -> setContext (f (context env)) env)

askMSP :: Lambda MSP
askMSP = msp |$> ask
localMSP :: (MSP -> MSP) -> Lambda a -> Lambda a
localMSP f = local (\env -> setMSP (f (msp env)) env)

askConfig :: Lambda Config
askConfig = config |$> ask
localConfig :: (Config -> Config) -> Lambda a -> Lambda a
localConfig f = local (\env -> setConfig (f (config env)) env)

--
-- LambdaStates
--
getFreeVars :: Lambda FreeVars
getFreeVars = do
    (x,_,_) <- get
    (*:) x
getGEnv :: Lambda GEnv
getGEnv = do
    (_,x,_) <- get
    (*:) x
getTypeDef :: Lambda TypeDef
getTypeDef = do
    (_,_,x) <- get
    (*:) x

putFreeVars :: FreeVars -> Lambda ()
putFreeVars fv = do
    (_,b,c) <- get
    put (fv,b,c) 
putGEnv :: GEnv -> Lambda ()
putGEnv genv = do
    (a,_,c) <- get
    put (a,genv,c)
putTypeDef :: TypeDef -> Lambda ()
putTypeDef td = do
    (a,b,_) <- get
    put (a,b,td)

----------------------------------------------------------------------
-- 
----------------------------------------------------------------------

--
-- BindVar
--
localBindVarsPush :: BindVars -> Lambda a -> Lambda a
localBindVarsPush newbinds = localBindVars (newbinds++)
isBindVar :: Name -> Lambda Bool
isBindVar name = do
    bindvars <- askBindVars
    (*:) $ name `elem` bindvars

--
-- Context
--
localContextPush :: Context -> Lambda a -> Lambda a
localContextPush newctx = localContext (M.union newctx)
localContextModify :: Name -> Type -> Lambda a -> Lambda a 
localContextModify name ty lam = do
    ctx <- askContext
    let newctx = M.insert name ty ctx
    localContext (const newctx) lam
lookupContext :: Name -> Lambda (Maybe Type)
lookupContext name = do
    ctx <- askContext
    (*:) $ M.lookup name ctx

--
-- MSP: Maybe SourcePos
--
localMSPBy :: MSP -> Lambda a -> Lambda a
localMSPBy msp = localMSP (const msp)

--
-- FreeVar
--
isFreeVar :: Name -> Lambda Bool
isFreeVar name = do
    freevars <- getFreeVars
    (*:) $ name `elem` freevars
pushFreeVar :: Name -> Lambda ()
pushFreeVar name = do
    freevars <- getFreeVars
    putFreeVars $ freevars ++ [name]

--
-- GEnv
--
lookupGEnv :: Name -> Lambda (Maybe (Type, Term))
lookupGEnv name = do
    genv <- getGEnv
    (*:) $ M.lookup name genv
pushGEnv :: Name -> (Type, Term) -> Lambda ()
pushGEnv name pair = do
    genv <- getGEnv
    putGEnv $ M.insert name pair genv


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

toIndex :: BindVar -> Lambda Index
toIndex name = do
    binds <- askBindVars
    frees <- getFreeVars
    let index = case elemIndex name binds of
                  Just n  -> n
                  Nothing -> case elemIndex name frees of
                    Just n  -> n + length binds
                    Nothing -> length frees + length binds
    (*:) index

restoreIndex :: Int -> Lambda Name
restoreIndex index = do
    binds <- askBindVars
    frees <- getFreeVars
    if length binds > index  
    then (*:) $ binds !! index
    else if length frees > index - length binds
         then (*:) $ frees !! (index - length binds)
         else do
            throwRestoreError $ "restoreIndex: wrong index detected: "++ show index ++"\n"
                                                                     ++"binds: "++ show binds ++"\n"
                                                                     ++"frees: "++ show frees

----------------------------------------------------------------------
-- other
----------------------------------------------------------------------

makeContext :: Lambda Context
makeContext = do
    freevars <- getFreeVars
    ctx <- askContext
    rec freevars ctx
  where
    rec :: FreeVars -> Context -> Lambda Context
    rec []         ctx = (*:) ctx
    rec (name:fvs) ctx = do
        mv <- lookupGEnv name
        case mv of
          Just (ty,_) -> rec fvs (M.insert name ty ctx)
          --Nothing -> throwCompileError $ strMsg $ "makeContext error: lookup: not found: " ++ name
          Nothing     -> rec fvs (M.insert name Ty.UNIT ctx)

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


