{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
module Lambda.Convertor (
    module Lambda.Convertor.PatternMatch,
    module Lambda.Convertor.SExpr,
    module Lambda.Convertor.Expr,
    module Lambda.Convertor.Term,
    module Lambda.Convertor.Util,

) where

import MonadX.Applicative
import MonadX.Monad

import Lambda.Convertor.PatternMatch
import Lambda.Convertor.SExpr
import Lambda.Convertor.Expr
import Lambda.Convertor.Term
import Lambda.Convertor.Util

import Lambda.DataType 
import qualified Lambda.DataType.SExpr as SE
import qualified Lambda.DataType.Expr as E
import qualified Lambda.DataType.PatternMatch as PM

import Debug.Trace

instance Convert SExpr Expr where
    convert (SE.BOOL bool msp)    = E.BOOL bool msp
    convert (SE.INT n msp)        = E.INT n msp
    convert (SE.CHAR c msp)       = E.CHAR c msp
    convert (SE.UNIT msp)         = E.UNIT msp
    convert (SE.VAR name msp)     = E.VAR name msp
    convert (SE.OPR name msp)     = E.OPR name msp
    --
    convert (SE.FIX x msp)        = E.FIX (convert x) msp
    -- syntax
    convert (SE.IF x1 x2 x3 msp)  = E.SYN (IF (convert x1) (convert x2) (convert x3)) msp
    convert (SE.CASE x pairs msp) = E.SYN (CASE (convert x) (pairs <$| \(pm, x) -> (pm, convert x))) msp
    -- sentence
    convert (SE.TYPESig p msp)         = E.SEN (TYPESig p) msp
    convert (SE.DEF name [([],x,msp)]) = E.SEN (DEF name (convert x)) msp
    convert (SE.BNF name ps msp)       = E.SEN (BNF name ps) msp
    -- quote
    convert (SE.QUT x msp)         = E.QUT (QUOTE (convert x)) msp
    convert (SE.QQUT x msp)        = E.QUT (QQUOTE (convert x)) msp
    convert (SE.UNQUT x msp)       = E.QUT (UNQUOTE (convert x)) msp
    -- list
    convert (SE.NIL msp)           = E.LIST NIL msp
    convert (SE.CONS a d msp)      = E.LIST (CONS (convert a) (convert d)) msp
    convert (SE.HEAD x)            = E.LIST (HEAD (convert x)) Nothing
    convert (SE.TAIL x)            = E.LIST (TAIL (convert x)) Nothing
    -- tuple
    convert (SE.TUPLE xs msp)     = E.TPL (TUPLE (convert |$> xs)) msp
    convert (SE.TPLPrj x i)       = E.TPL (TPLPrj (convert x) i) Nothing
    -- syntactic-sugar
    convert (SE.APP x1 [x2] msp)   = E.APP (convert x1) (convert x2) msp
    convert (SE.APP x xs msp)      = foldl (\acc x -> E.APP acc x msp) (convert x) (convert |$> xs)
    convert (SE.APPSeq xs msp)     = E.APPSeq (convert |$> xs) msp
    convert (SE.LAM params x msp)  = foldr (\param acc -> E.LAM param acc msp) (convert x) params
    convert (SE.LAMM params x msp) = foldr (\param acc -> E.LAMM param acc msp) (convert x) params
    convert se = error $ "Convert SExpr Expr: Non-exhaustive patterns: " ++ show (convert se :: SExpr_)

instance Convert Expr Term where
    convert E.NULL               = NULL
    convert (E.BOOL bool msp)    = BOOL bool msp
    convert (E.INT n msp)        = INT n msp
    convert (E.CHAR c msp)       = CHAR c msp
    convert (E.UNIT msp)         = UNIT msp
    convert (E.LAM param x msp)  = LAM param (convert x) msp
    convert (E.LAMM param x msp) = LAMM param (convert x) msp
    -- 
    convert (E.FIX x msp)        = FIX (convert x) msp
    convert (E.APP x1 x2 msp)    = APP (convert x1) (convert x2) msp
    -- gadget
    convert (E.SYN g msp)      = SYN (convert |$> g) msp
    convert (E.SEN g msp)      = SEN (convert |$> g) msp
    convert (E.QUT g msp)      = QUT (convert |$> g) msp
    convert (E.LIST g msp)     = LIST (convert |$> g) msp
    convert (E.TPL g msp)      = TPL (convert |$> g) msp
    convert (E.TAG g msp)      = TAG (convert |$> g) msp

    convert e = error $ "Convert Expr Term: Non-exhaustive patterns: " ++ show (convert e :: Expr_)

instance Convert Term Expr where
    convert NULL               = E.NULL
    convert (BOOL bool msp)    = E.BOOL bool msp
    convert (INT n msp)        = E.INT n msp
    convert (CHAR c msp)       = E.CHAR c msp
    convert (UNIT msp)         = E.UNIT msp
    convert (LAM param x msp)  = E.LAM param (convert x) msp
    convert (LAMM param x msp) = E.LAMM param (convert x) msp
    --
    convert (FIX x msp)        = E.FIX (convert x) msp
    convert (APP x1 x2 msp)    = E.APP (convert x1) (convert x2) msp
    -- func
    convert (CONST name _)     = E.VAR name Nothing
    convert (COMND name _)     = E.VAR name Nothing
    convert (AFUNC name _)     = E.VAR name Nothing
    convert (WAFUNC name _)    = E.VAR name Nothing
    convert (PROC name _)      = E.VAR name Nothing
    convert (META name _)      = E.VAR name Nothing
    -- gadget
    convert (SYN g msp)      = E.SYN (convert |$> g) msp
    convert (SEN g msp)      = E.SEN (convert |$> g) msp
    convert (QUT g msp)      = E.QUT (convert |$> g) msp
    convert (LIST g msp)     = E.LIST (convert |$> g) msp
    convert (TPL g msp)      = E.TPL (convert |$> g) msp
    convert (TAG g msp)      = E.TAG (convert |$> g) msp
    --
    convert (THUNK x)          = E.THUNK (convert x)

    -- convert x = error $ "Convert Expr Term: Non-exhaustive patterns: " ++ show x

-------------------------------------
-- PM
-------------------------------------

{-
instance Convert Term PM where
    convert (UNIT msp)        = PM.UNIT msp
    convert (VAR _ msp)       = PM.UNIT msp
    convert (BOOL bool msp)   = PM.BOOL bool msp
    convert (INT n msp)       = PM.INT n msp
    convert (CHAR c msp)      = PM.CHAR c msp
    convert (CONS a d)        = PM.CONS (convert a) (convert d) Nothing
    convert (TUPLE xs msp)    = PM.TUPLE (xs <$| convert) msp
    convert (TAG name xs msp) = PM.TAG name (xs <$| convert) Nothing
    convert (CONST name _)    = PM.VAR name Nothing
    convert (COMND name _)    = PM.VAR name Nothing
    convert (AFUNC name _)    = PM.VAR name Nothing
    convert (WAFUNC name _)   = PM.VAR name Nothing
    convert (PROC name _)     = PM.VAR name Nothing
    convert (META name _)     = PM.VAR name Nothing
    convert v                 = error $ "convert error: from Term '"++ show v ++"' to PM"
-}

instance Convert PM Term where
    convert (PM.UNIT msp)        = UNIT msp
    convert (PM.VAR name msp)    = UNIT msp
    convert (PM.BOOL x msp)      = BOOL x msp
    convert (PM.INT x msp)       = INT x msp
    convert (PM.CHAR x msp)      = CHAR x msp
    convert (PM.NIL msp)         = LIST NIL msp
    convert (PM.CONS x y msp)    = LIST (CONS (convert x) (convert y)) msp
    convert (PM.TUPLE xs msp)    = TPL (TUPLE (convert |$> xs)) msp
    convert (PM.TAG name xs msp) = TAG (TAGAs name (convert |$> xs)) msp


