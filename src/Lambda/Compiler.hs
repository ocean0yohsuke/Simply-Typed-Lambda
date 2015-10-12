module Lambda.Compiler (
    desugarSExpr, 
    desugarExpr, 
    typeofTerm, fromBNFtoLAM, 
    toTerm, restore, 
    
    rtM,
) where

import DeepControl.Applicative
import DeepControl.Monad hiding (mapM, forM, forM_)

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

import Prelude hiding (foldl, foldr, mapM, forM, concat)
import Data.List hiding (foldl, foldr, concat)
import Data.Function (on)
import qualified Data.Map as M
import Data.Foldable
import Data.Traversable

-- for debug
import Debug.Trace 

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
                e <- makeLAMFold (pm, tyPM, x) route >>= desugar
                (*:) (pm, e)
            ) |* msp
    desugar (SE.DEF name defs) = case defs of
        (pms,s,msp):[] -> localMSPBy msp $ do
            pairs <- defToLambdaPairs pms Ty.UNIT
            s <- desugar s
            (*:) $ SE.DEF name [([], SE.lam pairs s, msp)]
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
        s <- desugar s
        arg <- desugar arg 
        case pm of
          PM.CONS _ _ msp -> localMSPBy msp $ makeLAMFold (pm, ty, arg) s
          PM.TAG _ _ msp  -> localMSPBy msp $ makeLAMFold (pm, ty, arg) s
          PM.TUPLE _ msp  -> localMSPBy msp $ makeLAMFold (pm, ty, arg) s
          _ -> (*:) $ SE.app (SE.lam [(pm,ty)] s) [arg]
    desugar (SE.APP s [arg] msp) = do
        s <- desugar s 
        arg <- desugar arg
        (*:) $ SE.APP s [arg] msp
    desugar (SE.APP s args msp) = localMSPBy msp $ desugar $ foldl (\acc arg -> SE.APP acc [arg] msp) s args
    desugar (SE.LAM [(pm, ty)] s msp) = localMSPBy msp $ do
        s <- desugar s
        case pm of
          PM.CONS _ _ _ -> wrapfold s
          PM.TAG _ _ _  -> wrapfold s
          PM.TUPLE _ _  -> wrapfold s
          _             -> (*:) $ SE.LAM [(pm, ty)] s msp
      where
        wrapfold s = do
            lamfold <- makeLAMFold (pm, ty, SE.var "@x") s
            tyPM <- typeofPM pm
            (*:) $ SE.lam [(PM.var "@x", tyPM)] lamfold
    desugar (SE.LAM params s msp) = desugar $ foldr (\param acc -> SE.LAM [param] acc msp) s params
    desugar x = pdmapM desugar x

desugarExpr :: Expr -> Lambda Expr
desugarExpr (E.APPSeq es msp) = localMSPBy msp $ E.APPSeq (reverse es) msp >- (dsAPPSeq >=> desugarExpr)
  where
    dsAPPSeq :: Expr -> Lambda Expr
    dsAPPSeq (E.APPSeq (opr@(E.OPR name msp):l:[]) _) = localMSPBy msp $ do
        let opr = E.VAR name msp
        (*:) $ E.app opr l
    dsAPPSeq (E.APPSeq (r:opr@(E.OPR name msp):[]) _) = localMSPBy msp $ do
        let l = E.var "l"
            opr = E.VAR name msp
        (*:) $ E.lam (PM.var "l", Ty.UNIT) $ E.app (E.app opr l) r
    dsAPPSeq (E.APPSeq (r:(E.OPR name msp):l:[]) _) = localMSPBy msp $ do
        l <- desugarExpr l
        r <- desugarExpr r
        let opr = E.VAR name msp
        (*:) $ E.app (E.app opr l) r
    dsAPPSeq (E.APPSeq (opr@(E.OPR name msp):es) _)  = localMSPBy msp $ do
        l <- dsAPPSeq (E.APPSeq es Nothing)
        case l of
          E.LAM pm l msp -> localMSPBy msp $ do
            let r = E.var "r"
                opr = E.VAR name msp
            (*:) $ E.lam (PM.var "r", Ty.UNIT) $ E.lam pm $ E.app (E.app opr l) r
          _          -> do
            let r = E.var "r"
                opr = E.VAR name msp
            (*:) $ E.lam (PM.var "r", Ty.UNIT) $ E.app (E.app opr l) r
    dsAPPSeq (E.APPSeq (r:opr@(E.OPR name msp):es) _) = localMSPBy msp $ do 
        l <- dsAPPSeq (E.appseq es)
        let opr = E.VAR name msp
        case l of
          E.LAM pm l _ -> (*:) $ E.lam pm $ E.app (E.app opr l) r
          _            -> (*:) $ E.app (E.app opr l) r
desugarExpr (E.LAM (pm, ty) e msp) = localMSPBy msp $ do
    ty' <- compensateTypeUNIT ty
    E.LAM (pm, ty') |$> desugarExpr e |* msp
desugarExpr (E.LAMM (pm, ty) e msp) = localMSPBy msp $ do
    ty' <- compensateTypeUNIT ty
    E.LAMM (pm, ty') |$> desugarExpr e |* msp
desugarExpr x = pdmapM desugarExpr x

----------------------------------------------------------------------
-- toTerm: to de Bruijn Index term
----------------------------------------------------------------------

toTerm :: Expr -> Lambda Term
toTerm (E.VAR name msp) = localMSPBy msp $ do
    mv <- nameToIndex name
    case mv of
      Just index -> (*:) $ VAR index msp
      Nothing    -> throwCompileError $ strMsg $ "toTerm: not found: "++ show name
{-
      Nothing    -> do
        insertDef name (Ty.UNIT, unit)
        mv <- nameToIndex name
        case mv of Just index -> (*:) $ VAR index msp
-}
toTerm (E.OPR sym msp) = toTerm $ E.VAR sym msp
--
toTerm (E.FIX lam msp)     = localMSPBy msp $ FIX |$> toTerm lam |* msp
toTerm (E.APP opr opd msp) = localMSPBy msp $ APP |$> toTerm opr |*> toTerm opd |* msp
-- gadget
toTerm (E.SYN g msp) = localMSPBy msp $ SYN |$> mapM toTerm g |* msp
toTerm (E.SEN g msp) = SEN |$> (localMSPBy msp $ case g of
    TYPESig (name, ty) -> do
        mv <- lookupDef name
        case mv of
          Just _  -> throwCompileError $ strMsg $ "DEF: multiple type-signature declarations: "++ name
          Nothing -> do
            insertDef name (ty, unit)
            (*:) $ TYPESig (name, ty)
    DEF name e -> do
        t <- toTerm e
        mv <- lookupDef name 
        case mv of
          Nothing     -> insertDef name (Ty.UNIT, t)
          Just (ty,_) -> insertDef name (ty, t)
        (*:) $ DEF name t
    BNF typename pairs -> do
        pushTypeDef typename pairs
        forM_ pairs $ \(tagname, tys) -> do
            (lamtag, ty) <- fromBNFtoLAM typename (tagname, tys)
            insertDef tagname (ty, lamtag)
        (*:) $ BNF typename pairs
    ) |* msp
toTerm (E.QUT g msp) = localMSPBy msp $ QUT |$> mapM toTerm g |* msp
toTerm (E.LIST g msp) = localMSPBy msp $ LIST |$> mapM toTerm g |* msp
toTerm (E.TPL g msp) = localMSPBy msp $ TPL |$> mapM toTerm g |* msp
toTerm (E.TAG g msp) = localMSPBy msp $ TAG |$> mapM toTerm g |* msp
--
toTerm e = case e of
    E.NULL           -> (*:) NULL
    E.BOOL bool msp  -> (*:) $ BOOL bool msp
    E.INT n msp      -> (*:) $ INT n msp
    E.CHAR c msp     -> (*:) $ CHAR c msp
    E.LAM p e msp    -> localMSPBy msp $ LAM p |$> localLAMPush p (toTerm e) |* msp
    E.LAMM p e msp   -> localMSPBy msp $ LAMM p |$> localLAMPush p (toTerm e) |* msp
    _ -> throwCompileError $ strMsg $ "toTerm: invalid pattern detected: " ++ show (convert e :: Expr_)

fromBNFtoLAM :: Name -> (Name, [Type]) -> Lambda (Term, Type)
fromBNFtoLAM typename (tagname, tys) = do
    lamtag <- makeLambdaTag (tagname, tys)
    let ty = makeType tys
    (*:) (lamtag, ty)
  where 
    makeLambdaTag :: (Name, [Type]) -> Lambda Term
    makeLambdaTag (tagname, tys) = do
        let vars = VAR |$> take (length tys) newIndexes |* Nothing
            tagas = TAG (TAGAs tagname vars) Nothing
            pms = PM.var |$> take (length tys) newVars
        (*:) $ foldr (\(pm,ty) acc -> lam (pm, ty) acc) tagas (zip pms tys)
      where
        newIndexes = [0..]
        newVars :: [Name]
        newVars = ("x"++) |$> semiInfinitePostfixes
    makeType :: [Type] -> Type
    makeType tys = foldr (:->) (Ty.DATA typename) tys

----------------------------------------------------------------------
-- typeofTerm
----------------------------------------------------------------------

typeofTerm :: Term -> Lambda Type
typeofTerm e = do
    (ty, _) <- rec e []
    (*:) ty
  where
    rec :: Term -> TyUnify -> Lambda (Type, TyUnify)
    rec (VAR index msp) unify = localMSPBy msp $ do
        mv <- indexToType index
        case mv of
          Just ty -> do
            ctx <- askContext
            if length ctx > index
            then (*:) (ty, unify)
            else refreshTypeVars ty <$|(,)|* unify
          Nothing -> do
            newtypevar <- newTypeVar
            mv <- indexToName index
            case mv of
              Just name -> insertDef name (newtypevar, unit)
              Nothing   -> throwCompileError $ strMsg $ "typeofTerm: index is too large: "++ show index
            (*:) (newtypevar, unify)
    rec (FIX lam msp) unify = localMSPBy msp $ do
        (a :-> _, unify') <- rec lam unify
        (*:) (a, unify')
    rec (APP t1 t2 msp) unify = localMSPBy msp $ do
        (ty1, unify1) <- rec t1 unify
        (ty2, unify2) <- rec t2 unify1
        let ty1' = substTyUnifyforType unify2 ty1
        tyx <- newTypeVar
        unify3 <- typeunify [(ty1', ty2 :-> tyx)] `catchError` \e -> do
            e1 <- restore t1
            e2 <- restore t2
            throwTypeofError $ "APP: type mismatch: '"++ show e2 ++" :: "++ show ty2 ++"' is applied for '"++ show e1 ++" :: "++ show ty1' ++"'"
        (*:) (substTyUnifyforType unify3 tyx, unify3 `composeTyUnify` unify2)
    -- gadget
    rec (SYN syn msp) unify = localMSPBy msp $ case syn of
        IF e1 e2 e3 -> do
            (ty1, unify1) <- rec e1 unify
            (ty2, unify2) <- rec e2 unify1
            (ty3, unify3) <- rec e3 unify2
            let ty1' = substTyUnifyforType unify3 ty1
                ty2' = substTyUnifyforType unify3 ty2
            unify4 <- (typeunify [(ty1',Ty.BOOL)]) `catchError` \e -> do
                throwTypeofError $ "IF: guard of conditional is not a boolean: '"++ show e1 ++"::"++ show ty1' ++"'"
            let ty2'' = substTyUnifyforType unify4 ty2'
                ty3' = substTyUnifyforType unify4 ty3
            unify5 <- (typeunify [(ty2'',ty3')]) `catchError` \e -> do
                throwTypeofError $ "IF: arms of conditional have different types: '"++ show e2 ++"::"++ show ty2'' ++ "' and '"++ show e3 ++"::"++ show ty3' ++"'"
            (*:) (substTyUnifyforType unify5 ty2'', unify5 `composeTyUnify` unify4 `composeTyUnify` unify3)
        CASE e pairs -> do
            (ty, unify1) <- rec e unify
            let (pms, es) = (fst |$> pairs, snd |$> pairs)
            (tys, unify2) <- recseq es unify1
            let ty' = substTyUnifyforType unify2 ty
                tys' = substTyUnifyforType unify2 |$> tys
            unify3 <- checkPM ty' pms unify2
            let ty'' = substTyUnifyforType unify3 ty
                tys'' = substTyUnifyforType unify3 |$> tys
            let headtype = head tys''
            unify4 <- checkExpr headtype (tail tys'') unify3
            (*:) (headtype, unify4)
          where
            checkPM :: Type -> [PM] -> TyUnify -> Lambda TyUnify
            checkPM ty []       unify = (*:) unify
            checkPM ty (pm:pms) unify = do
                tyPM <- typeofPM pm
                unify1 <- typeunify [(ty, tyPM)]
                checkPM ty pms (composeTyUnify unify unify1)
            checkExpr :: Type -> [Type] -> TyUnify -> Lambda TyUnify
            checkExpr hty []       unify = (*:) unify
            checkExpr hty (ty:tys) unify = do
                unify1 <- typeunify [(hty,ty)]
                let hty' = substTyUnifyforType unify1 hty
                    ty' = substTyUnifyforType unify1 ty
                    tys' = substTyUnifyforType unify1 |$> tys
                if hty' /= ty'
                then throwTypeofError $ "CASE: Couldn't match expected type '"++ show hty ++"', but detected: '"++ show ty ++"'\n on: "++ show e
                else checkExpr hty' tys' (composeTyUnify unify unify1)
    rec (QUT _ _)    unify = (*:) (Ty.QUT, unify)
    rec (LIST g msp) unify = localMSPBy msp $ case g of
        NIL        -> do
            newtyvar <- newTypeVar
            (*:) (Ty.CONS newtyvar, unify)
        CONS e1 e2 -> do
            (ty1, unify1) <- rec e1 unify
            (ty2, unify2) <- rec e2 unify1
            let ty1' = substTyUnifyforType unify2 ty1
            tyx <- newTypeVar
            unify3 <- typeunify [(ty2, Ty.CONS tyx)] `catchError` \e -> do
                throwTypeofError $ "typeofExpr: CONS: wrong type detected: "++ show g ++" :: ("++ show ty1' ++":"++ show ty2 ++")"
            let tyx' = substTyUnifyforType unify3 tyx
            unify4 <- typeunify [(ty1', tyx')] `catchError` \e -> do
                throwTypeofError $ "typeofExpr: CONS: type mismatch: "++ show g ++" :: ("++ show ty1' ++":"++ show ty2 ++")"
            let ty1'' = substTyUnifyforType unify4 ty1'
            (*:) (Ty.CONS ty1'', unify4 `composeTyUnify` unify3 `composeTyUnify` unify2)
        HEAD e -> do
            (ty, unify1) <- rec e unify
            tyx <- newTypeVar
            unify2 <- typeunify [(ty, Ty.CONS tyx)] `catchError` \e -> do
                throwTypeofError $ "HEAD: not a list: " ++ show e ++" :: "++ show ty
            let tyx' = substTyUnifyforType unify2 tyx
            (*:) (tyx', unify2 `composeTyUnify` unify1)
        TAIL e -> do
            (ty, unify1) <- rec e unify
            tyx <- newTypeVar
            unify2 <- typeunify [(ty, Ty.CONS tyx)] `catchError` \e -> do
                throwTypeofError $ "TAIL: not a list: " ++ show e ++" :: "++ show ty
            let tyx' = substTyUnifyforType unify2 tyx
            (*:) (Ty.CONS tyx', unify2 `composeTyUnify` unify1)
    rec (SEN sen msp) unify = localMSPBy msp $ case sen of
        TYPESig (_, ty) -> (*:) (ty, unify)
        BNF name _      -> (*:) (Ty.DATA name, unify)
        DEF name e      -> rec e unify
    rec (TPL g msp) unify = localMSPBy msp $ case g of
        TUPLE xs   -> do
            (tys, unify1) <- recseq xs unify
            let tys' = substTyUnifyforType unify1 |$> tys
            (*:) (Ty.TUPLE tys', unify1)
        TPLPrj e n -> do
            (ty, unify1) <- rec e unify
            case ty of
              Ty.TUPLE tys ->
                if length tys < n
                then throwTypeofError $ "TPLPrj: tuple's index number exceeded: " ++ show n
                else (*:) (substTyUnifyforType unify1 (tys !! (n-1)), unify1)
              --Ty.VAR _ -> do
              _ -> throwTypeofError $ "TPLPrj: not a tuple: " ++ show e
    rec (TAG g msp) unify = localMSPBy msp $ case g of
        TAGAs name es -> do
            mv <- lookupDef name
            case mv of
              Nothing     -> throwTypeofError $ "TAG: not found:" ++ name
              Just (ty,_) -> do
                (tys, unify1) <- recseq es unify
                let tys' = substTyUnifyforType unify1 |$> tys  
                ty <- foldM app ty (zip tys' es)
                (*:) (ty, unify1)
        TAGPrj e (tagname, n) -> do
            (ty, unify1) <- rec e unify
            case ty of  
              Ty.DATA typename -> do
                mtys <- lookupTypeDef typename tagname
                case mtys of
                  Nothing  -> throwTypeofError $ "TAGPrj: Nothing returned on 'lookupTypeDef "++ show typename ++" "++ show tagname ++"'"
                  Just tys -> if length tys < n
                              then throwTypeofError $ "TAGPrj: tag's index number exceeded: " ++ show n
                              else (*:) (substTyUnifyforType unify1 (tys !! (n-1)), unify1)
              _ -> throwTypeofError $ "TAGPrj: not a tag: " ++ show e ++"::"++ show ty
      where
        app :: Type -> (Type, Term) -> Lambda Type
        app ty1 (ty2, e) = do
            tyx <- newTypeVar
            unify <- typeunify [(ty1, ty2 :-> tyx)] `catchError` \e -> do
                throwTypeofError $ "TAG: type mismatch: `"++ show e ++"::"++ show ty2 ++"` was applied for "++ show ty1
            (*:) $ substTyUnifyforType unify tyx
    rec e unify = case e of
        UNIT _    -> newTypeVar <$|(,)|* unify
        NULL      -> (*:) (Ty.NULL, unify)
        BOOL _ _  -> (*:) (Ty.BOOL, unify)
        INT _ _   -> (*:) (Ty.INT,  unify)
        CHAR _ _  -> (*:) (Ty.CHAR, unify)
        LAM (pm, ty) e msp -> localMSPBy msp $ do
            (tyx, unify1) <- localLAMPush (pm, ty) $ rec e unify
            let ty' = substTyUnifyforType unify1 ty 
            (*:) (ty' :-> tyx, unify)
        LAMM (pm, ty) e msp -> localMSPBy msp $ do
            (tyx, unify1) <- localLAMPush (pm, ty) $ rec e unify
            let ty' = substTyUnifyforType unify1 ty 
            (*:) (ty' :-> tyx, unify)
        _ -> throwTypeofError $ "invalid form: " ++ show e
    recseq :: [Term] -> TyUnify -> Lambda ([Type], TyUnify)
    recseq []     unify = (*:) ([], unify)
    recseq (x:xs) unify = do
        (ty, unify1) <- rec x unify
        (tys, unify2) <- recseq xs unify1
        (*:) (ty:tys, unify2)

----------------------------------------------------------------------
-- typeofPM: type of pattern match
----------------------------------------------------------------------

--typeofPM :: PM -> Lambda Type
--typeofPM pm = typeofExpr (convert pm)

typeofPM :: PM -> Lambda Type
typeofPM (PM.UNIT _)       = newTypeVar
typeofPM (PM.VAR name _)   = newTypeVar
typeofPM (PM.NIL _)        = Ty.CONS |$> newTypeVar
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
typeofPM (PM.BOOL _ _)     = (*:) Ty.BOOL
typeofPM (PM.INT _ _)      = (*:) Ty.INT
typeofPM (PM.CHAR _ _)     = (*:) Ty.CHAR
typeofPM (PM.TUPLE xs msp) = localMSPBy msp $ Ty.TUPLE |$> mapM typeofPM xs
typeofPM (PM.TAG name xs msp) = localMSPBy msp $ do
    mv <- lookupDef name
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
-- restore
----------------------------------------------------------------------

restore :: Term -> Lambda Expr
restore NULL       = (*:) E.NULL
restore (UNIT msp) = (*:) $ E.UNIT msp
restore (VAR index msp) = localMSPBy msp $ do
    mv <- indexToName index
    case mv of  
      Just name -> (*:) $ E.VAR name msp
      Nothing   -> throwCompileError $ strMsg $ "restore: index is too large: "++ show index
restore (FIX lam msp)   = localMSPBy msp $ E.FIX |$> restore lam |* msp
restore (APP t1 t2 msp) = localMSPBy msp $ E.APP |$> restore t1 |*> restore t2 |* msp
restore (CONST name _)  = (*:) $ E.var name 
restore (COMND name _)  = (*:) $ E.var name 
restore (AFUNC name _)  = restoreFuncKind "AFUNC" name
restore (WAFUNC name _) = restoreFuncKind "WAFUNC" name
restore (PROC name _)   = restoreFuncKind "PROC" name
restore (META name _)   = restoreFuncKind "META" name
restore (BOOL bool msp) = (*:) $ E.BOOL bool msp
restore (INT n msp)     = (*:) $ E.INT n msp
restore (CHAR c msp)    = (*:) $ E.CHAR c msp
--
restore (LAM (pm,ty) t msp) = localMSPBy msp $ do
        pm' <- restorePM pm
        E.LAM (pm', ty) |$> (localLAMPush (pm', ty) $ restore t) |* msp
restore (LAMM (pm,ty) t msp) = localMSPBy msp $ do
        pm' <- restorePM pm
        E.LAMM (pm', ty) |$> (localLAMPush (pm', ty) $ restore t) |* msp
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
        Left err        -> throwRestoreError $ label ++": failed to restore: '"++ string ++"'\n"++ show err
        Right (se,[])   -> se >- desugarSExpr
        Right (se,rest) -> throwRestoreError $ label ++": failed to restore: "++ string ++"\nparsed: "++ show se ++"\nrest: "++ show rest

----------------------------------------------------------------------
-- for debug
----------------------------------------------------------------------

-- restore & traceM
rtM :: String -> Term -> Lambda ()
rtM str t = do
    e <- restore t 
    traceM $ str ++ ": " ++ show e

