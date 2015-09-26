{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
module Lambda.DataType.Expr where

import MonadX.Applicative
import MonadX.Monad

import Lambda.DataType.Common
import Lambda.DataType.PatternMatch (PM)
import qualified Lambda.DataType.PatternMatch as PM
import Lambda.DataType.Type (Type((:->)))
import qualified Lambda.DataType.Type as Ty
import Lambda.DataType.Gadget
import Util.Pseudo

import Data.List (intersperse)
import Data.Foldable (fold)

-------------------------------------
-- Expr
-------------------------------------

-- TODO: NIL
data Expr = NULL
          | BOOL Bool MSP
          | INT Integer MSP
          | CHAR Char MSP
          | UNIT MSP
          | VAR Name MSP
          | OPR String MSP
          | LAM (PM, Type) Expr MSP
          | LAMM (PM, Type) Expr MSP
          --
          | FIX Expr MSP
          | APP Expr Expr MSP
          | APPSeq [Expr] MSP  -- infix application
          -- gadget
          | SYN (Syntax Expr) MSP
          | SEN (Sentence Expr) MSP
          | QUT (Quote Expr) MSP
          | LIST (List Expr) MSP
          | TPL (Tuple Expr) MSP
          | TAG (Tag Expr) MSP
          --
          | THUNK Expr -- for restore

unit = UNIT Nothing
var name = VAR name Nothing
opr str = OPR str Nothing
bool b = BOOL b Nothing
int n = INT n Nothing
char c = CHAR c Nothing
lam p e = LAM p e Nothing
lamm p e = LAMM p e Nothing
fix e = FIX e Nothing
app e1 e2 = APP e1 e2 Nothing
appseq xs = APPSeq xs Nothing
list g = LIST g Nothing
tpl g = TPL g Nothing
tag g = TAG g Nothing

isList :: List Expr -> Bool
isList NIL                 = True
isList (CONS _ (LIST g _)) = isList g
isList _                   = False
toList :: List Expr -> [Expr]
toList NIL                            = []
toList (CONS a (LIST NIL _))          = [a]
toList (CONS a (LIST d@(CONS _ _) _)) = a : (toList d)
fromList :: [Expr] -> List Expr
fromList [] = NIL
fromList [a] = CONS a (LIST NIL Nothing)
fromList (a:as) = CONS a (LIST (fromList as) Nothing)

instance Show Expr where
    show NULL            = "()"
    show (BOOL bool _)   = show bool
    show (INT n _)       = show n
    show (CHAR c _)      = "'"++ [c] ++"'"
    show (UNIT _)         = "_"
    show (VAR name _)     = if isSymbolic name
                                then "("++ name ++")"
                                else name
    show (OPR sym _)      = sym
    show (LAM (pm, ty@(_ :-> _)) e _)  = "\\"++ show pm ++"::("++ show ty ++")."++ show e
    show (LAM (pm, Ty.UNIT) e _)       = "\\"++ show pm ++"."++ show e
    --show (LAM (pm, Ty.VAR _) e _)      = "\\"++ show pm ++"."++ show e
    show (LAM (pm, ty)      e _)       = "\\"++ show pm ++"::"++ show ty ++"."++ show e
    show (LAMM (pm, ty@(_ :-> _)) e _) = "#"++ show pm ++"::("++ show ty ++")."++ show e
    show (LAMM (pm, Ty.UNIT) e _)      = "#"++ show pm ++"."++ show e
    --show (LAMM (pm, Ty.VAR _) e _)     = "#"++ show pm ++"."++ show e
    show (LAMM (pm, ty)      e _)      = "#"++ show pm ++"::"++ show ty ++"."++ show e
    -- 
    show (FIX lambda _)      = "fix " ++ show lambda 
    show (APP e1 e2 _)       = showOperator e1 ++" "++ showOperand e2
      where
        showOperator e@(OPR sym _) = "("++ sym ++")"
        showOperator e@(LAM _ _ _) = "("++ show e ++")"
        showOperator e@(FIX _ _)   = "("++ show e ++")"
        showOperator e@(SYN _ _)   = "("++ show e ++")"
        showOperator e             = show e
        showOperand e@(OPR sym _)  = "("++ show e ++")"
        showOperand e@(APP _ _ _)  = "("++ show e ++")"
        showOperand e              = showOperator e
    show (APPSeq [] _)     = error "show SExpr: APPSeq: empty seq"
    show (APPSeq (x:[]) _) = show x
    show (APPSeq seq _)    = "("++ (unwords $ seq <$| show) ++")"   
    -- list
    show (LIST g@(CONS a d) _) = 
        if isList g
        then case a of
            CHAR _ _ -> show $ toString $ toList g
            _        -> show $ toList g
        else showWithParens a ++ ":"++ showWithParens d
      where
        toString []                = [] 
        toString ((CHAR c _):cs)   = c : toString cs
        showWithParens x@(LAM _ _ _) = "("++ show x ++")"
        showWithParens x@(FIX _ _)   = "("++ show x ++")"
        showWithParens x@(APP _ _ _) = "("++ show x ++")"
        showWithParens x@(SYN _ _)   = "("++ show x ++")"
        showWithParens x             = show x
    -- gadget
    show (SYN g _)    = show g
    show (SEN g _)    = show g
    show (QUT g _)    = show g
    show (LIST g _)   = show g
    show (TPL g _)   = show g
    show (TAG g _)   = show g
    --
    show (THUNK x)      = "_["++ show x ++"]_"

instance Eq Expr where
    NULL     == NULL       = True
    BOOL x _ == BOOL y _   = x == y
    INT x _   == INT y _   = x == y
    CHAR x _  == CHAR y _  = x == y
    UNIT _ == _     = True
    _     == UNIT _ = True
    VAR x _ == VAR y _ = x == y
    OPR x _ == OPR y _ = x == y
    LAM x1 x2 _ == LAM y1 y2 _   = (x1,x2) == (y1,y2)
    LAMM x1 x2 _ == LAMM y1 y2 _ = (x1,x2) == (y1,y2)
    --
    FIX x _ == FIX y _ = x == y
    APP x1 x2 _ == APP y1 y2 _ = (x1,x2) == (y1,y2)
    APPSeq x _ == APPSeq y _ = x == y
    -- gadget
    SYN x _     == SYN y _  = x == y
    SEN x _     == SEN y _  = x == y
    QUT x _     == QUT y _  = x == y
    LIST x _    == LIST y _ = x == y
    TPL x _     == TPL y _  = x == y
    TAG x _     == TAG y _  = x == y
    --
    THUNK x     == THUNK y = x == y
    _     == _     = False


