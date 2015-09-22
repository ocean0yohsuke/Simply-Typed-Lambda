{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
module Lambda.DataType.Shadow.SExpr_ where

import MonadX.Applicative
import MonadX.Monad

import Lambda.DataType.Common
import Lambda.DataType.Shadow.PatternMatch_ (PM_)
import Lambda.DataType.Type (Type)
import Lambda.DataType.SExpr (SExpr)
import qualified Lambda.DataType.SExpr as O -- original
import Util.Pseudo (Convert(..))

data SExpr_ = BOOL Bool
            | INT Integer
            | CHAR Char
            | UNIT   
            | VAR Name　　　　　　　　       -- variable
            | OPR String　　　　　　　　     -- symbolic operator
            -- tuple
            | TUPLE [SExpr_]
            | TPLPrj SExpr_ Index 
            -- 
            | FIX SExpr_ 
            -- syntax
            | IF SExpr_ SExpr_ SExpr_ 
            | CASE SExpr_ [(PM_, SExpr_)] 
            -- sentence
            | TYPESig (Name, Type) 
            | DEF Name [([PM_], SExpr_)]
            | BNF Name [(Name, [Type])] 
            -- quote
            | QUT SExpr_      -- quote
            | QQUT SExpr_     -- quasi-quote
            | UNQUT SExpr_    -- unquote
            -- syntactic-sugar
            | APP SExpr_ [SExpr_]         -- application
            | APPSeq [SExpr_]             -- infix application sequence
            | LAM [(PM_, Type)] SExpr_     -- lambda
            | LAMM [(PM_, Type)] SExpr_     -- lambda-macro
            | AS (SExpr_, Type)           -- ascription
            | LET (PM_, Type) SExpr_ SExpr_ 
            | LETREC (Name, Type) SExpr_ SExpr_ 
            -- list
            | NIL 
            | CONS SExpr_ SExpr_
            | HEAD SExpr_
            | TAIL SExpr_

  deriving (Show, Eq, Read)

instance Convert SExpr SExpr_ where
    convert (O.BOOL bool _)         = BOOL bool
    convert (O.INT n _)             = INT n
    convert (O.CHAR c _)            = CHAR c
    convert (O.UNIT _)              = UNIT
    convert (O.VAR name _)          = VAR name
    convert (O.OPR sym _)           = OPR sym
    -- tuple    
    convert (O.TUPLE xs _)          = TUPLE (xs <$| convert)
    convert (O.TPLPrj x i)          = TPLPrj (convert x) i
    --
    convert (O.FIX x _)             = FIX (convert x)
    -- syntax
    convert (O.IF x1 x2 x3 _)       = IF (convert x1) (convert x2) (convert x3)
    convert (O.CASE x ps _)         = CASE (convert x) $ ps <$| (\(pm, se) -> (convert pm, convert se))
    -- sentence
    convert (O.TYPESig p _)         = TYPESig p
    convert (O.DEF name xs)         = DEF name (xs <$| (\(pms, x, _) -> (convert |$> pms, convert x)))
    convert (O.BNF name ps _)       = BNF name ps
    -- syntactic-sugar
    convert (O.APP x xs _)          = APP (convert x) (xs <$| convert)
    convert (O.APPSeq seq _)        = APPSeq (convert |$> seq)
    convert (O.LAM ps x _)          = LAM (ps <$| \(pm,ty) -> (convert pm,ty)) (convert x)
    convert (O.LAMM ps x _)         = LAMM (ps <$| \(pm,ty) -> (convert pm,ty)) (convert x)
    convert (O.AS (se,ty) _)        = AS (convert se, ty)
    convert (O.LET (pm,ty) x1 x2 _) = LET (convert pm,ty) (convert x1) (convert x2)
    convert (O.LETREC p x1 x2 _)    = LETREC p (convert x1) (convert x2)
    -- quote
    convert (O.QUT x _)             = QUT (convert x) 
    convert (O.QQUT x _)            = QQUT (convert x) 
    convert (O.UNQUT x _)           = UNQUT (convert x) 
    -- list
    convert (O.NIL _)               = NIL
    convert (O.CONS a d _)          = CONS (convert a) (convert d) 
    convert (O.HEAD x)              = HEAD (convert x)
    convert (O.TAIL x)              = TAIL (convert x)

instance Convert SExpr_ SExpr where
    convert (BOOL bool)         = O.BOOL bool Nothing
    convert (INT n)             = O.INT n Nothing
    convert (CHAR c)            = O.CHAR c Nothing
    convert UNIT                = O.UNIT Nothing
    convert (VAR name)          = O.VAR name Nothing
    convert (OPR sym)           = O.OPR sym Nothing
    -- tuple
    convert (TUPLE xs)          = O.TUPLE (xs <$| convert) Nothing
    convert (TPLPrj x i)        = O.TPLPrj (convert x) i
    --
    convert (FIX x)             = O.FIX (convert x) Nothing
    -- syntax
    convert (IF x1 x2 x3)       = O.IF (convert x1) (convert x2) (convert x3) Nothing
    convert (CASE x ps)         = O.CASE (convert x) (ps <$| (\(pm, se) -> (convert pm, convert se))) Nothing
    -- sentece
    convert (TYPESig p)         = O.TYPESig p Nothing
    convert (DEF name xs)       = O.DEF name $ xs <$| (\(pms,x) -> (convert |$> pms, convert x, Nothing))
    convert (BNF name ps)       = O.BNF name ps Nothing
    -- syntactic-sugar
    convert (APP x xs)          = O.APP (convert x) (xs <$| convert) Nothing
    convert (APPSeq seq)        = O.APPSeq (convert |$> seq) Nothing
    convert (LAM ps x)          = O.LAM (ps <$| \(pm,ty) -> (convert pm,ty)) (convert x) Nothing
    convert (LAMM ps x)         = O.LAMM (ps <$| \(pm,ty) -> (convert pm,ty)) (convert x) Nothing
    convert (AS (se,ty))        = O.AS (convert se, ty) Nothing
    convert (LET (pm,ty) x1 x2) = O.LET (convert pm,ty) (convert x1) (convert x2) Nothing
    convert (LETREC p x1 x2)    = O.LETREC p (convert x1) (convert x2) Nothing
    -- quote
    convert (QUT x)             = O.QUT (convert x) Nothing
    convert (QQUT x)            = O.QQUT (convert x) Nothing
    convert (UNQUT x)           = O.UNQUT (convert x) Nothing
    -- list
    convert NIL                 = O.NIL Nothing
    convert (CONS a d)          = O.CONS (convert a) (convert d) Nothing
    convert (HEAD x)            = O.HEAD (convert x)
    convert (TAIL x)            = O.TAIL (convert x)



