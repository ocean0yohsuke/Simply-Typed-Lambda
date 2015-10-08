module Lambda.Evaluator.LazyEval (
    lazyEval1, lazyEval,
    force, delay, 
) where

import DeepControl.Applicative
import DeepControl.Monad

import Lambda.Action
import Lambda.Convertor
import Lambda.Compiler
import Lambda.Evaluator.Eval.BetaReduce
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

lazyEval_ :: Term -> Lambda Term
lazyEval_ = lazyEval >=> catchVoid

----------------------------------------------------------------------
-- lazyEval
----------------------------------------------------------------------

actual :: Term -> Lambda Term
actual t = 
    if isThunked t
    then t >- (force >=> actual)
    else t >- (lazyEval_ >=> force)
weekactual :: Term -> Lambda Term
weekactual t@(THUNK _) = t >- (weekforce >=> weekactual)
weekactual t = if isValue t
               then (*:) t
               else t >- (lazyEval_ >=> weekforce)

force :: Term -> Lambda Term
force t@(THUNK _) = t >- (deThunk >=> force)
force t           = pdmapM force t
weekforce :: Term -> Lambda Term
weekforce t@(THUNK _) = t >- (deThunk >=> weekforce)
weekforce t           = (*:) t

deThunk :: Term -> Lambda Term
deThunk (THUNK t) = do
    v <- t >- lazyEval_
    --memoize v            -- TODO: IORef か Referenceモナド で実装する必要あり
    (*:) $ v
deThunk t = (*:) t

delay :: Term -> Lambda Term
delay t
    | isValue t = (*:) t
    | isTHUNK t = (*:) t
    | otherwise = do
        env <- ask
        (*:) $ THUNK t

----------------------------------------------------------------------
-- lazyEval1
----------------------------------------------------------------------

lazyEval1 :: Term -> Lambda Term
lazyEval1 (VAR index msp) = localMSPBy msp $ do
    mv <- indexToTerm index
    case mv of
      Just (CONST _ fc) -> fc 
      Just (COMND _ fc) -> fc >>= catchVoid
      Just v -> do
        ctx <- askContext
        (*:) $ shift (length ctx) 0 v
      Nothing -> throwCompileError $ strMsg $ "lazyEval1: index is too large: "++ show index
lazyEval1 (FIX lam@(LAM p t _) msp) = localMSPBy msp $ case t of
    THUNK _ -> FIX |$> (LAM p |$> weekforce t |* Nothing) |* msp -- TODO:
    _       -> (*:) $ betaReduce (FIX lam msp) t
lazyEval1 (APP t arg msp) = localMSPBy msp $ case t of 
    AFUNC _ fc -> 
        if isValue arg
        then fc arg
        else APP t |$> actual arg |* msp
    WAFUNC _ fc -> 
        if isTHUNK arg
        then APP t |$> weekactual arg |* msp
        else fc arg
    PROC _ fc -> 
        if isValue arg
        then APP t |$> weekactual arg |* msp
        else fc arg >>= catchVoid
    META _ fc -> fc arg >>= catchVoid
    LAM (pm, ty) body msp -> localMSPBy msp $ do
        arg <- delay arg
        betaReducePM (pm, arg) body
    THUNK _ -> APP |$> weekforce t |* arg |* msp
    _       -> APP |$> lazyEval1 t |* arg |* msp
lazyEval1 (SYN s msp) = localMSPBy msp $ case s of 
    IF cond t1 t2 -> case cond of
        BOOL True _  -> (*:) t1
        BOOL False _ -> (*:) t2
        _            -> SYN |$> (IF |$> actual cond |* t1 |* t2) |* msp
    CASE x pairs -> 
        if isThunked x
        then SYN |$> (CASE |$> actual x |* pairs) |* msp 
        else gointoCaseRoute x pairs 
-- quote
lazyEval1 t@(QUT _ _) = lazyEval_ t
-- list
lazyEval1 (LIST g msp) = localMSPBy msp $ case g of
    NIL      -> (*:) $ LIST NIL msp
    CONS a d -> (*:) $ LIST (CONS a d) msp
    HEAD t   -> case t of
        LIST NIL _        -> throwEvalError $ strMsg $ "head: null list detected"
        LIST (CONS a _) _ -> (*:) a
        _                 -> LIST |$> (HEAD |$> weekactual t) |* msp
    TAIL t   -> case t of
        LIST NIL _        -> throwEvalError $ strMsg $ "tail: null list detected"
        LIST (CONS _ d) _ -> (*:) $ d
        _                 -> LIST |$> (TAIL |$> weekactual t) |* msp
lazyEval1 t@(TPL g msp) = localMSPBy msp $ case g of
    TUPLE _    -> (*:) t
    TPLPrj t n -> case t of
        TPL (TUPLE ts) _ -> (*:) $ ts !! (n-1)
        _                -> TPL |$> (TPLPrj |$> weekactual t |* n) |* msp
lazyEval1 t@(TAG g msp) = localMSPBy msp $ case g of
    TAGAs _ _            -> (*:) t
    TAGPrj t (tagname,n) -> case t of
        TAG (TAGAs name ts) _ -> (*:) $ ts !! (n-1)
        _                     -> TAG |$> (TAGPrj |$> weekactual t |* (tagname,n)) |* msp
-- value
lazyEval1 t@(THUNK _) = weekforce t
lazyEval1 v = (*:) v

----------------------------------------------------------------------
-- lazyEval
----------------------------------------------------------------------

lazyEval :: Term -> Lambda ReturnT
lazyEval (VAR index msp) = localMSPBy msp $ do
    mv <- indexToTerm index
    case mv of
      Just (CONST _ fc) -> RETURN |$> fc 
      Just (COMND _ fc) -> fc 
      Just v            -> do
        ctx <- askContext
        (*:) $ RETURN $ shift (length ctx) 0 v
      Nothing -> throwCompileError $ strMsg $ "lazyEval: index is too large: "++ show index
lazyEval (FIX lam@(LAM _ t _) msp) = localMSPBy msp $ do
    t <- weekactual t
    lazyEval $ betaReduce (FIX lam msp) t
lazyEval (APP t arg msp) = localMSPBy msp $ do
    t <- weekactual t  
    case t of
      AFUNC _ fc            -> RETURN |$> (arg >- (actual >=> fc))
      WAFUNC _ fc           -> RETURN |$> (arg >- (weekforce >=> fc))
      PROC _ fc             -> arg >- (actual >=> fc)
      META _ fc             -> arg >- fc
      LAM (pm, _) body msp  -> localMSPBy msp $ do
        arg <- delay arg
        body >- (betaReducePM (pm, arg) >=> weekforce >=> lazyEval)  
      _                     -> restore t >>= \e -> throwEvalError $ strMsg $ "lazyEval: APP: invalid operator detected: "++ show e
lazyEval (SYN s msp) = localMSPBy msp $ case s of 
    IF cond t1 t2 -> do
        bool <- actual cond
        case bool of
          BOOL True _  -> t1 >- (weekforce >=> lazyEval)
          BOOL False _ -> t2 >- (weekforce >=> lazyEval)
    CASE x pairs -> do
        x <- weekactual x
        gointoCaseRoute x pairs >>= (weekforce >=> lazyEval)
lazyEval (SEN s msp) = localMSPBy msp $ case s of
    TYPESig (name, _) -> insertDef name (Ty.UNIT, unit) >> (*:) VOID
    DEF name t        -> do
        v <- lazyEval_ t 
        insertDef name (Ty.UNIT, v)
        (*:) VOID
    BNF typename pairs -> do
        pushTypeDef typename pairs
        forM_ pairs $ \(tagname, tys) -> do
            (lamtag, ty) <- fromBNFtoLAM typename (tagname, tys)
            lazyEval $ SEN (DEF tagname lamtag) Nothing
        (*:) VOID
-- quote
lazyEval t@(QUT g msp) = localMSPBy msp $ case g of
    QUOTE x   -> (*:) $ RETURN x
    QQUOTE x  -> RETURN |$> unquote x
    UNQUOTE _ -> restore t >>= \e -> throwEvalError $ strMsg $ "unquote appeard outside quasi-quote: "++ show e
  where
    unquote :: Term -> Lambda Term
    unquote (QUT (UNQUOTE t@(VAR _ _)) _) = lazyEval_ t
    unquote x                             = pdmapM unquote x
-- list
lazyEval (LIST g msp) = localMSPBy msp $ RETURN |$> case g of
    NIL      -> (*:) $ LIST NIL msp
    CONS a d -> (*:) $ LIST (CONS a d) msp
    HEAD t   -> do
        LIST x _ <- weekactual t
        case x of
          NIL      -> throwEvalError $ strMsg $ "head: null list detected"
          CONS a _ -> (*:) a
    TAIL t   -> do
        LIST x _ <- weekactual t
        case x of
          NIL      -> throwEvalError $ strMsg $ "tail: null list detected"
          CONS _ d -> (*:) d
lazyEval t@(TPL g msp) = localMSPBy msp $ case g of
    TUPLE _    -> (*:) $ RETURN t
    TPLPrj t n -> do
        TPL (TUPLE ts) _ <- weekactual t
        lazyEval $ ts!!(n-1)
lazyEval t@(TAG g msp) = localMSPBy msp $ case g of
    TAGAs _ _ -> (*:) $ RETURN t
    TAGPrj t (tagname, n) -> do
        TAG (TAGAs name ts) _ <- weekactual t 
        lazyEval $ ts!!(n-1)
-- value
lazyEval v = case v of
    THUNK _ -> restore v >>= \e -> throwEvalError $ strMsg $ "invalid lazyEval form: "++ show e
    _ -> (*:) $ RETURN v

------------------------------------------------------
-- other 
------------------------------------------------------

gointoCaseRoute :: Term -> [(PM, Term)] -> Lambda Term
gointoCaseRoute x ((pm,t):pairs) = do
    bool <- equal x pm
    if bool
    then (*:) t
    else case pairs of
            [] -> do restore x >>= \e -> throwEvalError $ strMsg $ "CASE: unexhausted pattern: "++ show e
            _  -> gointoCaseRoute x pairs
  where
    -- TODO: to be a function of some class?
    equal :: Term -> PM -> Lambda Bool
    equal _                        (PM.UNIT _)          = (*:) True 
    equal (UNIT _)                 _                    = (*:) True
    equal _                        (PM.VAR _ _)         = (*:) True
    equal (BOOL b1 _)              (PM.BOOL b2 _)       = (*:) $ b1 == b2
    equal (INT n1 _)               (PM.INT n2 _)        = (*:) $ n1 == n2
    equal (CHAR c1 _)              (PM.CHAR c2 _)       = (*:) $ c1 == c2
    equal (LIST NIL _)             (PM.NIL _)           = (*:) True
    equal (LIST (CONS a1 d1) _)    (PM.CONS a2 d2 _)    = equal a1 a2 <$|(&&)|*> equal d1 d2
    equal (TPL (TUPLE xs) _)       (PM.TUPLE pms _)     = and |$> zipWithM equal xs pms
    equal (TAG (TAGAs name1 xs) _) (PM.TAG name2 pms _) = ((name1 == name2)&&) |$> (and |$> zipWithM equal xs pms)
    equal t@(THUNK _)              pm                   = weekactual t >>= (`equal` pm)
    equal _                        _                    = (*:) False


