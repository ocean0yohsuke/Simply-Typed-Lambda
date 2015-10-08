{-# LANGUAGE MultiParamTypeClasses #-}
module Lambda.DataType.Shadow.Expr_ where

import DeepControl.Applicative
import DeepControl.Monad

import Lambda.DataType.Common
import Lambda.DataType.PatternMatch (PM)
import Lambda.DataType.Shadow.PatternMatch_ (PM_)
import Lambda.DataType.Type (Type((:->)))
import Lambda.DataType.Gadget (Syntax, Sentence)
import Lambda.DataType.Shadow.Gadget_  (Syntax_, Sentence_, Quote_, List_, Tuple_, Tag_)
import Lambda.DataType.Expr (Expr)
import qualified Lambda.DataType.Expr as O -- original
import Util.Pseudo (Convert(..))

import Data.List (intersperse)
import qualified Data.Foldable as F

-------------------------------------
-- Expr_
-------------------------------------

data Expr_ = BOOL Bool
           | INT Integer
           | CHAR Char
           | UNIT
           | VAR Name
           | OPR String
           | LAM (PM_, Type) Expr_
           | LAMM (PM_, Type) Expr_
           -- 
           | FIX Expr_
           | APP Expr_ Expr_
           | APPSeq [Expr_]  -- infix application
           -- gadget
           | SYN (Syntax_ Expr_)
           | SEN (Sentence_ Expr_)
           | QUT (Quote_ Expr_)
           | LIST (List_ Expr_)
           | TPL (Tuple_ Expr_)
           | TAG (Tag_ Expr_)
           --
           | THUNK Expr_   -- for restore
    deriving (Show, Eq, Read)

instance Convert Expr Expr_ where
    convert (O.BOOL bool _)      = BOOL bool
    convert (O.INT n _)          = INT n
    convert (O.CHAR c _)         = CHAR c
    convert (O.UNIT _)           = UNIT
    convert (O.VAR name _)       = VAR name
    convert (O.OPR sym _)        = OPR sym
    convert (O.LAM (pm,ty) x _)  = LAM (convert pm, ty) (convert x)
    convert (O.LAMM (pm,ty) x _) = LAMM (convert pm, ty) (convert x)
    -- 
    convert (O.FIX x _)          = FIX (convert x)
    convert (O.APP x y _)        = APP (convert x) (convert y)
    convert (O.APPSeq xs _)      = APPSeq (xs <$| convert)
    -- gadget
    convert (O.SYN g _)        = SYN $ convert g
    convert (O.SEN g _)        = SEN $ convert g
    convert (O.QUT g _)        = QUT $ convert g
    convert (O.LIST g _)       = LIST $ convert g
    convert (O.TPL g _)        = TPL $ convert g
    convert (O.TAG g _)        = TAG $ convert g
    -- 
    convert (O.THUNK x)          = THUNK (convert x)

instance Convert Expr_ Expr where
    convert (BOOL bool)      = O.BOOL bool Nothing
    convert (INT n)          = O.INT n Nothing
    convert (CHAR c)         = O.CHAR c Nothing
    convert UNIT             = O.UNIT Nothing
    convert (VAR name)       = O.VAR name Nothing
    convert (OPR sym)        = O.OPR sym Nothing
    convert (LAM (pm,ty) x)  = O.LAM (convert pm, ty) (convert x) Nothing
    convert (LAMM (pm,ty) x) = O.LAMM (convert pm, ty) (convert x) Nothing
    -- 
    convert (FIX x)          = O.FIX (convert x) Nothing
    convert (APP x y)        = O.APP (convert x) (convert y) Nothing
    convert (APPSeq xs)      = O.APPSeq (xs <$| convert) Nothing
    -- gadget
    convert (SYN g)        = O.SYN (convert g) Nothing
    convert (SEN g)        = O.SEN (convert g) Nothing
    convert (QUT g)        = O.QUT (convert g) Nothing
    convert (LIST g)       = O.LIST (convert g) Nothing
    convert (TPL g)        = O.TPL (convert g) Nothing
    convert (TAG g)        = O.TAG (convert g) Nothing
    -- 
    convert (THUNK x)        = O.THUNK (convert x)

