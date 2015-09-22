module Lambda.Compiler (
    desugarSExpr, makeNewContext, localContextBy, 
    desugarExpr, typeofExpr, 
    toTerm, restore, 
    
    rtM,
) where

import MonadX.Applicative
import MonadX.Monad hiding (mapM, forM)

import Lambda.Action
import Lambda.Convertor
import Lambda.Parser (readSLexeme)
import Lambda.Compiler.Desugar
import Lambda.Compiler.Restore
import Lambda.Compiler.ToTerm
import Lambda.Compiler.Typeof
import qualified Lambda.DataType.PatternMatch as PM
import qualified Lambda.DataType.Type as Ty
import qualified Lambda.DataType.Expr as E
import qualified Lambda.DataType.SExpr as SE
import Lambda.DataType
import qualified Lambda.DataType.Error.Compile as CE
import qualified Lambda.DataType.Error.Eval as EE

import Lambda.Util
import Lambda.Debug

import Prelude hiding (foldl, foldr, mapM, forM)
import Data.List hiding (foldl, foldr)
import Data.Function (on)
import qualified Data.Map as M
import Data.Foldable
import Data.Traversable

-- for debug
import Debug.Trace 

makeNewContext :: Expr -> Lambda Context
makeNewContext e = do
    makeFreeVars e
    ctx <- makeContext
    (*:) ctx
  where
    makeFreeVars :: Expr -> Lambda Expr
    makeFreeVars e@(E.VAR name msp) = localMSPBy msp $ do
        isbind <- isBindVar name
        isfree <- isFreeVar name
        if isbind || isfree
        then (*:) e
        else pushFreeVar name >> (*:) e
    makeFreeVars (E.OPR sym msp)    = makeFreeVars $ E.VAR sym msp
    makeFreeVars e = pdmapM makeFreeVars e

localContextBy :: Context -> Lambda a -> Lambda a
localContextBy ctx = localContext (const ctx)

----------------------------------------------------------------------
-- desugar
----------------------------------------------------------------------

desugarSExpr :: SExpr -> Lambda Expr
desugarSExpr se = convert |$> desugar se
  where
    desugar :: SExpr -> Lambda SExpr
    desugar (SE.CASE x pairs msp) = localMSPBy msp $ do
        SE.CASE |$> desugar x |*> (do
            forM pairs $ \(pm, route) -> do
                tyPM <- typeofPM pm
                e <- makeLAMSugar (pm, tyPM, x) route >>= desugar
                (*:) (pm, e)
            ) |* msp
    desugar (SE.DEF name defs) = case defs of
        (pms,s,msp):[] -> localMSPBy msp $ do
            mv <- lookupGEnv name
            case mv of
              Nothing      -> throwDesugarError $ "Couldn't find type-signature of '"++ name ++"'"
              Just (ty, _) -> do
                pairs <- defToLambdaPairs pms ty
                s <- desugar s
                (*:) $ SE.DEF name [([], SE.LAM pairs s Nothing, msp)]
        (pms,_,msp):_ -> localMSPBy msp $ do
            pairs <- defsToCasePairs defs
            let len = length pms
            if len == 1
            then do
                let newvar = head $ newVars 1
                    caseform = SE.CASE (SE.var newvar) pairs msp
                desugar $ SE.DEF name [([PM.var newvar], caseform, msp)]
            else do
                let newvars = newVars len
                    caseform = SE.CASE (SE.tuple (SE.var |$> newvars)) pairs msp
                desugar $ SE.DEF name [(PM.var |$> newvars, caseform, msp)]
    desugar (SE.LET p s1 s2 msp) = localMSPBy msp $ desugar $ SE.app (SE.lam [p] s2) [s1]
    desugar (SE.AS (s, ty) msp) = localMSPBy msp $ do
        s <- desugar s
        -- (λx:(_ -> ty). x _) (λ_. e) -- TODO: (λx:ty. x) e
        (*:) $ SE.app (SE.lam [(PM.var "x", Ty.UNIT :-> ty)] (SE.app (SE.var "x") [SE.unit]))
                      [(SE.lam [(PM.unit, Ty.UNIT)] s)]
    desugar (SE.LETREC (var,ty) s1 s2 msp) = localMSPBy msp $ desugar $ SE.let_ (PM.var var, ty) (SE.fix (SE.lam [(PM.var var, ty)] s1)) s2
    desugar (SE.APP s                       []    msp) = localMSPBy msp $ desugar s
    desugar (SE.APP (SE.LAM [(pm, ty)] s _) [arg] msp) = localMSPBy msp $ do
        case pm of
          PM.CONS _ _ msp -> localMSPBy msp $ makeLAMSugar (pm, ty, arg) s >>= desugar
          PM.TAG _ _ msp  -> localMSPBy msp $ makeLAMSugar (pm, ty, arg) s >>= desugar
          PM.TUPLE _ msp  -> localMSPBy msp $ makeLAMSugar (pm, ty, arg) s >>= desugar
          _ -> do
            s <- desugar s
            arg <- desugar arg
            (*:) $ SE.app (SE.lam [(pm,ty)] s) [arg]
    desugar (SE.APP s [arg] msp) = do
        s <- desugar s 
        arg <- desugar arg
        (*:) $ SE.APP s [arg] msp
    desugar (SE.APP s args msp) = localMSPBy msp $ desugar $ foldl (\acc arg -> SE.APP acc [arg] msp) s args
    desugar x = pdmapM desugar x

desugarExpr :: Expr -> Lambda Expr
desugarExpr (E.APPSeq es msp) = localMSPBy msp $ desugar $ E.APPSeq (reverse es) msp
  where
    typeofOPR :: Expr -> Lambda Type
    typeofOPR opr@(E.OPR name msp) = localMSPBy msp $ do
        mv <- lookupContext name
        case mv of 
          Nothing -> throwTypeofError $ "desugarExpr: VAR: failed to find: "++ name
          Just ty -> (*:) ty
    typeofOPR e               = throwDesugarError $ "typeofOPR: invalid form: "++ show e
    desugar :: Expr -> Lambda Expr
    desugar (E.APPSeq (opr@(E.OPR name msp):l:[]) _) = localMSPBy msp $ do
        x <- typeofOPR opr
        let opr = E.VAR name msp
        (*:) $ E.app opr l
    desugar (E.APPSeq (r:opr@(E.OPR name msp):[]) _) = localMSPBy msp $ do
        x <- typeofOPR opr
        case x of
          ty :-> _ :-> _ -> do
            let l = E.var "l"
                opr = E.VAR name msp
            (*:) $ E.lam (PM.var "l", ty) $ E.app (E.app opr l) r
          _ -> throwTypeofError $ "infix operator's type is invalid: "++ show opr ++" :: "++ show x
    desugar (E.APPSeq (r:(E.OPR name msp):l:[]) _) = localMSPBy msp $ do
        l <- desugarExpr l
        r <- desugarExpr r
        let opr = E.VAR name msp
        (*:) $ E.app (E.app opr l) r
    desugar (E.APPSeq (opr@(E.OPR name msp):es) _)  = localMSPBy msp $ do
        l <- desugar (E.APPSeq es Nothing)
        case l of
          E.LAM pm l msp -> localMSPBy msp $ do
            x <- typeofOPR opr
            case x of
              ty :-> _  :-> _ -> do
                let r = E.var "r"
                    opr = E.VAR name msp
                (*:) $ E.lam (PM.var "r", ty) $ E.lam pm $ E.app (E.app opr l) r
              _ -> throwTypeofError $ "infix operator's type is invalid: "++ show opr ++" :: "++ show x
          _          -> do
            x <- typeofOPR opr
            case x of
              _ :-> ty :-> _ -> do
                let r = E.var "r"
                    opr = E.VAR name msp
                (*:) $ E.lam (PM.var "r", ty) $ E.app (E.app opr l) r
              _ -> throwTypeofError $ "infix operator's type is invalid: "++ show opr ++" :: "++ show x
    desugar (E.APPSeq (r:opr@(E.OPR name msp):es) _) = localMSPBy msp $ do 
        l <- desugar (E.appseq es)
        let opr = E.VAR name msp
        case l of
          E.LAM pm l _ -> (*:) $ E.lam pm $ E.app (E.app opr l) r
          _            -> (*:) $ E.app (E.app opr l) r
desugarExpr x = pdmapM desugarExpr x

----------------------------------------------------------------------
-- typeofExpr
----------------------------------------------------------------------

typeofExpr :: Expr -> Lambda Type
typeofExpr E.NULL           = (*:) Ty.NULL
typeofExpr (E.UNIT _)       = (*:) Ty.UNIT
typeofExpr (E.VAR name msp) = localMSPBy msp $ do
    mv <- lookupContext name
    case mv of 
      Nothing -> throwTypeofError $ "typeofExpr: VAR: failed to find: " ++ name
      Just ty -> (*:) ty
typeofExpr (E.OPR name msp) = typeofExpr (E.VAR name msp)
typeofExpr (E.FIX lam msp) = localMSPBy msp $ do
    a :-> _ <- typeofExpr lam
    (*:) a
typeofExpr (E.APP e1 e2 msp) = localMSPBy msp $ do
    opd <- typeofExpr e2
    opr <- case e1 of
        E.LAM (pm,ty) e msp -> localMSPBy msp $ do
            if ty == opd
            then typeofExpr $ E.LAM (pm,opd) e msp
            else throwTypeofError $ "APP: type mismatch1: '"++ show e2 ++" :: "++ show opd ++"' with '"++ show ty ++"'"
        _               -> typeofExpr e1
    case opr of
      opr_left :-> opr_right -> 
        if opr_left == opd
        then do
            (*:) $ applyTypeVars (opr_left, opd) opr_right
        else throwTypeofError $ "APP: type mismatch2: '"++ show e2 ++" :: "++ show opd ++"' is applied for '"++ show e1 ++" :: "++ show opr ++"'"
      Ty.UNIT                -> (*:) $ Ty.UNIT
      _                      -> throwTypeofError $ "APP: expected an application type, but detected: '"++ show e1 ++"::"++ show opr ++"'"
-- gadget
typeofExpr (E.SYN syn msp) = localMSPBy msp $ case syn of
    IF e1 e2 e3 -> do
        ty1 <- typeofExpr e1
        if ty1 == Ty.BOOL
        then do
            ty2 <- typeofExpr e2
            ty3 <- typeofExpr e3
            if ty2 == ty3
            then (*:) ty2
            else throwTypeofError $ "IF: arms of conditional have different types: '"++ show e2 ++"::"++ show ty2 ++ "' and '"++ show e3 ++"::"++ show ty3 ++"'"
        else throwTypeofError $ "IF: guard of conditional is not a boolean: '"++ show e1 ++"::"++ show ty1 ++"'"
    CASE e pairs -> do
        ty <- typeofExpr e
        let (pms, es) = (fst |$> pairs, snd |$> pairs)
        mapM (checkPM ty) pms
        headtype <- typeofExpr (head es)
        mapM (checkExpr headtype) (tail es)
        (*:) headtype
      where
        checkPM :: Type -> PM -> Lambda ()
        checkPM ty pm = do
            tyPM <- typeofPM pm
            if tyPM /= ty
            then throwTypeofError $ "CASE: wrong type detected on pattern match: expected '"++ show ty ++"', but detected '"++ show tyPM ++"' on '"++ show pm ++"'"
            else (*:) ()
        checkExpr :: Type -> Expr -> Lambda ()
        checkExpr ty e = do
            tyE <- typeofExpr e
            if tyE /= ty
            then throwTypeofError $ "CASE: Couldn't match expected type '"++ show ty ++"', but detected: '"++ show tyE ++"'\n on: "++ show e
            else (*:) ()
typeofExpr (E.SEN sen msp) = localMSPBy msp $ case sen of
    TYPESig (_, ty) -> (*:) ty
    DEF name e      -> do
        mv <- lookupGEnv name
        case mv of
          Nothing      -> throwTypeofError $ strMsg $ "DEF: type-signature is missing for the declaration '"++ name ++"'" 
          Just (ty,_) -> do
            ty' <- typeofExpr e
            if ty /= ty'
            then throwTypeofError $ "DEF: Couldn't much type `" ++ show ty' ++ "` of `"++ name ++"`, with type-signature `"++ name ++" :: "++ show ty ++"`"
            else (*:) ty'
    BNF name _      -> (*:) $ Ty.DATA name
typeofExpr (E.QUT qut msp) = (*:) Ty.QUT
-- list
typeofExpr (E.LIST g msp) = localMSPBy msp $ case g of
    NIL -> (*:) $ Ty.CONS Ty.UNIT
    CONS x y -> do
        tyX <- typeofExpr x
        tyY <- typeofExpr y
        case tyY of
            Ty.UNIT    -> (*:) $ Ty.CONS tyX
            Ty.VAR _   -> (*:) $ Ty.CONS tyX
            Ty.CONS ty -> if tyX == ty
                          then (*:) $ Ty.CONS tyX
                          else throwTypeofError $ "typeofPM: CONS: type mismatch: "++ show g ++" :: "++ show tyX ++":"++ show tyY  
            _          -> throwTypeofError $ "typeofPM: CONS: wrong type detected: "++ show g ++" :: "++ show tyX ++":"++ show tyY  
    HEAD x -> do
        v <- typeofExpr x
        case v of
          Ty.CONS ty -> (*:) ty
          _ -> throwTypeofError $ "HEAD: not a list: " ++ show x
    TAIL x -> do
        v <- typeofExpr x
        case v of
          Ty.CONS ty -> (*:) $ Ty.CONS ty
          _ -> throwTypeofError $ "TAIL: not a list: " ++ show x
typeofExpr (E.TPL g msp) = localMSPBy msp $ case g of
    TUPLE xs   -> Ty.TUPLE |$> mapM typeofExpr xs
    TPLPrj e n -> do
        v <- typeofExpr e
        case v of
          Ty.TUPLE tys -> if length tys < n
                          then throwTypeofError $ "TPLPrj: tuple's index number exceeded: " ++ show n
                          else (*:) $ tys !! (n-1)
          _ -> throwTypeofError $ "TPLPrj: not a tuple: " ++ show e
typeofExpr (E.TAG g msp) = localMSPBy msp $ case g of
    TAGAs name es -> do
        mv <- lookupGEnv name
        case mv of
          Nothing     -> throwTypeofError $ "TAG: not found:" ++ name
          Just (ty,_) -> do
            tys <- mapM typeofExpr es
            foldM app ty (zip tys es)
    TAGPrj e (tagname, n) -> do
        v <- typeofExpr e
        case v of
          Ty.DATA typename -> do
            mtys <- lookupTypeDef typename tagname
            case mtys of
              Nothing  -> throwTypeofError $ "TAGPrj: Nothing returned on 'lookupTypeDef "++ show typename ++" "++ show tagname ++"'"
              Just tys -> if length tys < n
                          then throwTypeofError $ "TAGPrj: tag's index number exceeded: " ++ show n
                          else (*:) $ tys !! (n-1)
          _ -> throwTypeofError $ "TAGPrj: not a tag: " ++ show e ++"::"++ show v
  where
    app :: Type -> (Type, Expr) -> Lambda Type
    app (a :-> b) (c,e) = if a == c
                          then (*:) b
                          else throwTypeofError $ "TAG: type mismatch: '"++ show e ++"::"++ show c ++"'"
    app x         _     = throwTypeofError $ "TAG: tag function's type is invalid: "++ show x
-- 
typeofExpr e = case e of
    E.BOOL _ _  -> (*:) Ty.BOOL
    E.INT _ _   -> (*:) Ty.INT
    E.CHAR _ _  -> (*:) Ty.CHAR
    E.LAM (pm, ty) e msp  -> localMSPBy msp $ (ty:->) |$> (localLAMPush (pm, ty) $ typeofExpr e)
    E.LAMM (pm, ty) e msp -> localMSPBy msp $ (ty:->) |$> (localLAMPush (pm, ty) $ typeofExpr e)
    _ -> throwTypeofError $ "invalid form: " ++ show e

----------------------------------------------------------------------
-- typeofPM: type of pattern match
----------------------------------------------------------------------

typeofPM :: PM -> Lambda Type
typeofPM (PM.VAR name _)   = (*:) Ty.UNIT
typeofPM (PM.NIL _)        = (*:) $ Ty.CONS Ty.UNIT
typeofPM (PM.CONS x y msp) = localMSPBy msp $ do
    tyX <- typeofPM x
    tyY <- typeofPM y
    case tyY of
      Ty.UNIT    -> (*:) $ Ty.CONS tyX
      Ty.VAR _   -> (*:) $ Ty.CONS tyX
      Ty.CONS ty -> if tyX == ty
                    then (*:) $ Ty.CONS tyX
                    else throwTypeofError $ "typeofPM: CONS: type mismatch: "++ show (PM.CONS x y msp) ++" :: "++ show tyX ++":"++ show tyY  
      _          -> throwTypeofError $ "typeofPM: CONS: wrong type detected: "++ show (PM.CONS x y msp) ++" :: "++ show tyX ++":"++ show tyY  
typeofPM (PM.UNIT _)       = (*:) Ty.UNIT
typeofPM (PM.BOOL _ _)     = (*:) Ty.BOOL
typeofPM (PM.INT _ _)      = (*:) Ty.INT
typeofPM (PM.CHAR _ _)     = (*:) Ty.CHAR
typeofPM (PM.TUPLE xs msp) = localMSPBy msp $ Ty.TUPLE |$> mapM typeofPM xs
typeofPM (PM.TAG name xs msp) = localMSPBy msp $ do
    mv <- lookupGEnv name
    case mv of
      Nothing     -> throwTypeofError $ "TAG: not found:" ++ name
      Just (ty,_) -> do
        tys <- mapM typeofPM xs
        foldM app ty (zip tys xs)
  where
    app :: Type -> (Type, PM) -> Lambda Type
    app (a :-> b) (c,e) = if a == c
                          then (*:) b
                          else throwTypeofError $ "TAG: type mismatch: '"++ show e ++"::"++ show c ++"'"
    app x         _     = throwTypeofError $ "TAG: tag function's type is invalid: "++ show x

----------------------------------------------------------------------
-- toTerm: to de Bruijn Index term
----------------------------------------------------------------------

toTerm :: Expr -> Lambda Term
toTerm (E.VAR name msp) = localMSPBy msp $ do
    isbind <- isBindVar name
    isfree <- isFreeVar name
    if isbind || isfree
    then VAR |$> toIndex name |* msp
    else do
         pushFreeVar name
         VAR |$> toIndex name |* msp
toTerm (E.OPR sym msp) = toTerm $ E.VAR sym msp
toTerm e@(E.APPSeq es msp) = localMSPBy msp $ e >- (desugarExpr >=> toTerm) -- TODO:
--
toTerm (E.FIX lam msp)     = localMSPBy msp $ FIX |$> toTerm lam |* msp
toTerm (E.APP opr opd msp) = localMSPBy msp $ APP |$> toTerm opr |*> toTerm opd |* msp
-- gadget
toTerm (E.SYN g msp) = localMSPBy msp $ SYN |$> mapM toTerm g |* msp
toTerm (E.SEN g msp) = localMSPBy msp $ SEN |$> mapM toTerm g |* msp
toTerm (E.QUT g msp) = localMSPBy msp $ QUT |$> mapM toTerm g |* msp
toTerm (E.LIST g msp) = localMSPBy msp $ LIST |$> mapM toTerm g |* msp
toTerm (E.TPL g msp) = localMSPBy msp $ TPL |$> mapM toTerm g |* msp
toTerm (E.TAG g msp) = localMSPBy msp $ TAG |$> mapM toTerm g |* msp
--
toTerm e = case e of
    E.NULL                -> (*:) NULL
    E.BOOL bool msp       -> (*:) $ BOOL bool msp
    E.INT n msp           -> (*:) $ INT n msp
    E.CHAR c msp          -> (*:) $ CHAR c msp
    
    E.LAM (pm, ty) e msp  -> localMSPBy msp $ LAM (pm, ty) |$> localLAMPushPM pm (toTerm e) |* msp
    E.LAMM (pm, ty) e msp -> localMSPBy msp $ LAMM (pm, ty) |$> localLAMPushPM pm (toTerm e) |* msp
    _ -> throwCompileError $ strMsg $ "toTerm: invalid pattern detected: " ++ show (convert e :: Expr_)

----------------------------------------------------------------------
-- restore
----------------------------------------------------------------------

restore :: Term -> Lambda Expr
restore NULL       = (*:) E.NULL
restore (UNIT msp) = (*:) $ E.UNIT msp
restore (VAR index msp) = localMSPBy msp $ do
    name <- restoreIndex index
    (*:) $ E.VAR name msp
restore (FIX lam msp)   = localMSPBy msp $ E.FIX |$> restore lam |* msp
restore (APP t1 t2 msp) = localMSPBy msp $ E.APP |$> restore t1 |*> restore t2 |* msp
restore (CONST name _)  = (*:) $ E.var name 
restore (COMND name _)  = (*:) $ E.var name 
restore (AFUNC name _)  = restoreFuncKind "AFUNC" name
restore (WAFUNC name _) = restoreFuncKind "WAFUNC" name
restore (PROC name _)   = restoreFuncKind "PROC" name
restore (META name _)   = restoreFuncKind "META" name
restore (BOOL bool msp)   = (*:) $ E.BOOL bool msp
restore (INT n msp)       = (*:) $ E.INT n msp
restore (CHAR c msp)      = (*:) $ E.CHAR c msp
--
restore (LAM (pm,ty) t msp) = localMSPBy msp $ do
        (pm', binds) <- restorePM pm
        E.LAM (pm', ty) |$> (localBindVars (const binds) $ restore t) |* msp
restore (LAMM (pm,ty) t msp) = localMSPBy msp $ do
        (pm', binds) <- restorePM pm
        E.LAMM (pm', ty) |$> (localBindVars (const binds) $ restore t) |* msp
restore (THUNK t)        = E.THUNK |$> restore t
-- gadget
restore (SYN g msp) = localMSPBy msp $ E.SYN |$> mapM restore g |* msp
restore (SEN g msp) = localMSPBy msp $ E.SEN |$> mapM restore g |* msp
restore (QUT g msp) = localMSPBy msp $ E.QUT |$> mapM restore g |* msp
restore (LIST g msp) = localMSPBy msp $ E.LIST |$> mapM restore g |* msp
restore (TPL g msp) = localMSPBy msp $ E.TPL |$> mapM restore g |* msp
restore (TAG g msp) = localMSPBy msp $ E.TAG |$> mapM restore g |* msp

restoreFuncKind :: String -> String -> Lambda Expr
restoreFuncKind label string = 
    case readSLexeme string of
        Left err        -> throwRestoreError $ strMsg $ label ++": failed to restore: '"++ string ++"'\n"++ show err
        Right (se,[])   -> se >- desugarSExpr
        Right (se,rest) -> throwRestoreError $ strMsg $ label ++": failed to restore: "++ string ++"\nparsed: "++ show se ++"\nrest: "++ show rest

----------------------------------------------------------------------
-- for debug
----------------------------------------------------------------------

-- restore & traceM
rtM :: String -> Term -> Lambda ()
rtM str t = do
    e <- restore t 
    traceM $ str ++ ": " ++ show e

