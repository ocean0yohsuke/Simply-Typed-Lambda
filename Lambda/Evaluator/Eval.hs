module Lambda.Evaluator.Eval (
    compile, 
    -- runInterpretE, runInterpretT,
    interpretT, interpretE,

    thisEval, thisEval_,
    eval1, eval, 
    evalMacro,
) where

import MonadX.Applicative
import MonadX.Monad

import Lambda.Action
import Lambda.Convertor
import Lambda.Compiler

import Lambda.Evaluator.LazyEval (lazyEval, force)
import Lambda.Evaluator.Eval.BetaReduce
import Lambda.Evaluator.Eval.Util
import Lambda.DataType
import Lambda.DataType.Error.Eval (EvalError)
import qualified Lambda.DataType.PatternMatch as PM
import qualified Lambda.DataType.Type as Ty
import qualified Lambda.DataType.Expr as E
import qualified Lambda.DataType.SExpr as SE
import qualified Lambda.DataType.Error.Eval as EErr
import Lambda.Util
import Lambda.Debug

import qualified Data.Map as M
import Data.List

-- for debug
import Debug.Trace 

----------------------------------------------------------------------
-- 
----------------------------------------------------------------------

compile :: SExpr -> Lambda Term
compile code = do
    e <- desugarSExpr code
    t <- e >- (desugarExpr >=> toTerm)
    ty <- t >- typeofTerm
    case ty of
      Ty.UNIT -> t >- evalMacro
      _       -> t >- evalMacro

interpretT :: SExpr -> Lambda ReturnT
interpretT = compile >=> thisEval
interpretE :: SExpr -> Lambda ReturnE
interpretE se = do
    v <- se >- (compile >=> thisEval)
    case v of
      VOID     -> (*:) VOID
      RETURN t -> RETURN |$> restore t

----------------------------------------------------------------------
-- 
----------------------------------------------------------------------

thisEval :: Term -> Lambda ReturnT
thisEval t = do
    cfg <- askConfig
    if isLazyMode cfg
    then do
        x <- lazyEval t
        case x of
            VOID     -> (*:) VOID
            RETURN t -> RETURN |$> force t
    else eval t

thisEval_ :: Term -> Lambda Term
thisEval_ = thisEval >=> catchVoid

eval_ :: Term -> Lambda Term
eval_ = eval >=> catchVoid

----------------------------------------------------------------------
-- eval1
----------------------------------------------------------------------

eval1 :: Term -> Lambda Term
eval1 (VAR index msp) = localMSPBy msp $ do
    mv <- indexToTerm index
    case mv of
      Just (CONST _ fc) -> fc 
      Just (COMND _ fc) -> fc >>= catchVoid
      Just v -> do
        ctx <- askContext 
        (*:) $ shift (length ctx) 0 v
      Nothing -> throwCompileError $ strMsg $ "eval1: index is too large: "++ show index
eval1 (FIX lam@(LAM _ t _) msp) = localMSPBy msp $ (*:) $ betaReduce (FIX lam msp) t
eval1 (APP t arg msp) = case t of
    AFUNC _ fc  -> 
        if isValue arg
        then fc arg
        else APP t |$> eval1 arg |* msp
    WAFUNC _ fc  -> do
        if isValue arg
        then fc arg
        else APP t |$> eval1 arg |* msp
    PROC _ fc  -> 
        if isValue arg
        then arg >- (fc >=> catchVoid)
        else APP t |$> eval1 arg |* msp
    META _ fc  -> 
        if isVAR arg 
        then APP t |$> eval1 arg |* msp >>= eval1
        else arg >- (fc >=> catchVoid)
    LAM (pm, _) body msp -> localMSPBy msp $ 
        if isQUOTE arg || isQQUOTE arg
        then eval1 arg >>= \arg -> betaReducePM (pm, arg) body
        else if isValue arg 
             then betaReducePM (pm, arg) body
             else APP t |$> eval1 arg |* msp
    _ -> 
        if isValue t
        then throwEvalError $ strMsg $ "eval1: APP: invalid operator detected: "++ show t
        else if isVAR t
             then do
                 t' <- eval1 t
                 case t' of
                   LAM _ _ _  -> (*:) $ APP t' arg msp
                   _          -> eval1 $ APP t' arg msp
             else APP |$> eval1 t |* arg |* msp
eval1 (SYN syn msp) = localMSPBy msp $ case syn of 
    IF cond t1 t2 -> case cond of
        BOOL True  _ -> eval1 t1
        BOOL False _ -> eval1 t2 
        _            -> SYN |$> (IF |$> eval1 cond |* t1 |* t2) |* msp
    CASE x pairs -> 
        if isValue x
        then gointoCaseRoute x pairs 
        else SYN |$> (CASE |$> eval1 x |* pairs) |* msp
-- quote
eval1 t@(QUT _ _) = eval_ t
-- list
eval1 (LIST g msp) = localMSPBy msp $ case g of -- TODO: Micro.hs 経由で eval が使われてしまう。
    NIL      -> (*:) $ LIST NIL msp
    CONS a d -> 
        if isValue a
        then if isValue d
             then (*:) $ LIST (CONS a d) msp
             else LIST |$> (CONS a |$> eval1 d) |* msp
        else LIST |$> (CONS |$> eval1 a |* d) |* msp
    HEAD t -> case t of
        LIST NIL _        -> throwEvalError $ strMsg $ "head: null list detected"
        LIST (CONS a _) _ -> (*:) a
        _                 -> LIST |$> (HEAD |$> eval1 t) |* msp
    TAIL t -> case t of
        LIST NIL _        -> throwEvalError $ strMsg $ "tail: null list detected"
        LIST (CONS _ d) _ -> (*:) d
        _                 -> LIST |$> (TAIL |$> eval1 t) |* msp
eval1 (TPL g msp) = localMSPBy msp $ case g of
    TUPLE ts    -> TPL |$> (TUPLE |$> mapM eval1 ts) |* msp
    TPLPrj t n -> case t of
        TPL (TUPLE ts) _ -> (*:) $ ts !! (n-1) 
        _                -> TPL |$> (TPLPrj |$> eval1 t |* n) |* msp
eval1 (TAG g msp) = localMSPBy msp $ case g of
    TAGAs name ts        -> TAG |$> (TAGAs name |$> mapM eval1 ts) |* msp
    TAGPrj t (tagname,n) -> case t of
        TAG (TAGAs name ts) _ -> (*:) $ ts !! (n-1)
        _                     -> TAG |$> (TAGPrj |$> eval1 t |* (tagname,n)) |* msp
-- value
eval1 v = (*:) v
--eval1 v = throwEvalError $ strMsg $ "invalid eval1 form: "++ show v

----------------------------------------------------------------------
-- eval
----------------------------------------------------------------------

eval :: Term -> Lambda ReturnT
eval (VAR index msp) = localMSPBy msp $ do
    mv <- indexToTerm index
    case mv of
      Just (CONST _ fc) -> RETURN |$> fc 
      Just (COMND _ fc) -> fc 
      Just v            -> (*:) $ RETURN v
      Nothing -> throwCompileError $ strMsg $ "eval: index is too large: "++ show index
eval (FIX lam@(LAM _ t _) msp) = localMSPBy msp $ eval $ betaReduce (FIX lam msp) t
eval (APP t arg msp) = localMSPBy msp $ do
    x <- eval_ t
    case x of
      AFUNC _ fc         -> RETURN |$> (arg >- (eval_ >=> fc))
      WAFUNC _ fc        -> RETURN |$> (arg >- (eval_ >=> fc))
      PROC _ fc          -> arg >- (eval_ >=> fc)
      META _ fc          -> arg >- fc
      LAM (pm, _) t msp  -> localMSPBy msp $ do
        arg <- eval_ arg
        t >- (betaReducePM (pm, arg) >=> eval)
      _ -> do
        e <- restore t
        throwEvalError $ strMsg $ "eval: APP: invalid operator detected: " ++ show e
eval (SYN s msp) = localMSPBy msp $ case s of 
    IF cond t1 t2 -> do
        bool <- eval_ cond
        case bool of
          BOOL True _  -> eval t1
          BOOL False _ -> eval t2
    CASE x pairs -> do
        x' <- eval_ x
        gointoCaseRoute x' pairs >>= eval
eval (SEN s msp) = localMSPBy msp $ case s of 
    TYPESig (name, ty) -> insertDef name (ty, unit) >> (*:) VOID
    DEF name t        -> do
        v <- eval_ t 
        insertDef name (Ty.UNIT, v)
        (*:) VOID
    BNF typename pairs -> do
        pushTypeDef typename pairs
        forM_ pairs $ \(tagname, tys) -> do
            (lamtag, ty) <- fromBNFtoLAM typename (tagname, tys)
            eval $ SEN (DEF tagname lamtag) Nothing
        (*:) VOID
-- quote
eval t@(QUT qut msp) = localMSPBy msp $ case qut of
    QUOTE x   -> (*:) $ RETURN x
    QQUOTE x  -> RETURN |$> unquote x
    UNQUOTE _ -> restore t >>= \e -> throwEvalError $ strMsg $ "unquote appeard outside quasi-quote: "++ show e
  where
    unquote :: Term -> Lambda Term
    unquote (QUT (UNQUOTE t@(VAR _ _)) _) = eval_ t
    unquote x                             = pdmapM unquote x
-- list
eval (LIST g msp) = localMSPBy msp $ RETURN |$> case g of
    NIL -> (*:) $ LIST NIL msp
    CONS a d -> LIST |$> (CONS |$> eval_ a |*> eval_ d) |* msp
    HEAD t -> do
        LIST x _ <- eval_ t
        case x of
          NIL      -> throwEvalError $ strMsg $ "head: null list detected"
          CONS a _ -> (*:) a
    TAIL t -> do
        LIST x _ <- eval_ t    
        case x of
          NIL      -> throwEvalError $ strMsg $ "tail: null list detected"
          CONS _ d -> (*:) d
eval t@(TPL g msp) = localMSPBy msp $ case g of
    TUPLE ts   -> RETURN |$> (TPL |$> (TUPLE |$> mapM eval_ ts) |* msp)
    TPLPrj t n -> do
        TPL (TUPLE ts) _ <- eval_ t
        eval $ ts !! (n-1)
eval t@(TAG g msp) = localMSPBy msp $ case g of
    TAGAs name ts         -> RETURN |$> (TAG |$> (TAGAs name |$> mapM eval_ ts) |* msp)
    TAGPrj t (tagname, n) -> do
        TAG (TAGAs name ts) _ <- eval_ t 
        eval $ ts !! (n-1)
-- value
eval v = (*:) $ RETURN v
-- eval v = throwEvalError $ strMsg $ "invalid eval form: "++ show v

----------------------------------------------------------------------
-- evalMacro
----------------------------------------------------------------------

evalMacro :: Term -> Lambda Term
evalMacro t@(VAR index msp) = localMSPBy msp $ do
    mv <- indexToTerm index 
    case mv of
      Just v@(LAMM _ _ _) -> do
        ctx <- askContext
        (*:) $ shift (length ctx) 0 v
      Just _ -> (*:) t
      Nothing -> throwCompileError $ strMsg $ "evalMacro: index is too large: "++ show index
evalMacro (APP x arg msp) = localMSPBy msp $ do
    v <- evalMacro x
    case v of
      LAMM (pm, _) t msp -> localMSPBy msp $ do
        arg <- evalMacro arg
        t >- (betaReducePM (pm, arg) >=> eval_)
      _ -> APP v |$> evalMacro arg |* msp
evalMacro t@(QUT _ _) = (*:) t
evalMacro x = pdmapM evalMacro x


