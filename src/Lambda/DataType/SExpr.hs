{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
module Lambda.DataType.SExpr where

import DeepControl.Applicative
import DeepControl.Monad

import Lambda.DataType.Common
import Lambda.DataType.PatternMatch (PM)
import qualified Lambda.DataType.PatternMatch as PM
import Lambda.DataType.Type (Type((:->)))
import qualified Lambda.DataType.Type as Ty
import Util.Pseudo
import qualified Util.LISP as L

import Data.List (unwords, intersperse)
import Data.Foldable (fold)

-- | sugared expression
data SExpr = BOOL Bool MSP
           | INT Integer MSP
           | CHAR Char MSP
           | UNIT MSP  
           | VAR Name　MSP　　　　　　　       -- variable
           | OPR String　MSP　　　　　　　     -- symbolic operator
           -- tuple
           | TUPLE [SExpr] MSP
           | TPLPrj SExpr Index
           -- tag
           | TAGPrj SExpr (Name, Index)
           --
           | FIX SExpr MSP
           -- syntax
           | IF SExpr SExpr SExpr MSP
           | CASE SExpr [(PM, SExpr)] MSP
           -- sentence
           | TYPESig (Name, Type) MSP
           | DEF Name [([PM], SExpr, MSP)]
           | BNF Name [(Name, [Type])] MSP
           -- quote
           | QUT SExpr MSP     -- quote
           | QQUT SExpr MSP    -- quasi-quote
           | UNQUT SExpr MSP   -- unquote
           -- syntactic-sugar
           | APP SExpr [SExpr] MSP         -- application
           | APPSeq [SExpr] MSP            -- infix application sequence
           | LAM [(PM, Type)] SExpr MSP    -- lambda
           | LAMM [(PM, Type)] SExpr MSP -- lambda-macro
           | AS (SExpr, Type) MSP          -- ascription
           | LET (PM, Type) SExpr SExpr MSP
           | LETREC (Name, Type) SExpr SExpr MSP
           -- list
           | NIL MSP
           | CONS SExpr SExpr MSP
           | HEAD SExpr 
           | TAIL SExpr 
   deriving (Eq)

unit = UNIT Nothing
var name = VAR name Nothing
bool b = BOOL b Nothing
int n = INT n Nothing
char c = CHAR c Nothing
nil = NIL Nothing
cons a d = CONS a d Nothing
tuple xs = TUPLE xs Nothing
fix x = FIX x Nothing
app x xs = APP x xs Nothing
appseq xs = APPSeq xs Nothing
lam ps x = LAM ps x Nothing
lamm ps x = LAMM ps x Nothing
let_ p x1 x2 = LET p x1 x2 Nothing
letrec p x1 x2 = LETREC p x1 x2 Nothing

instance L.LISP SExpr where
    car (CONS a _ _) = a
    car _            = error "car: empty structure"
    cdr (CONS _ d _) = d
    cdr _            = error "cdr: empty structure"
    cons a d = CONS a d Nothing
    nil = NIL Nothing
    isCell (CONS _ _ _) = True
    isCell _            = False

instance Show SExpr where
    -- value
    show (BOOL bool _)  = show bool
    show (INT n _)      = show n
    show (CHAR c _)     = "'"++ [c] ++"'"
    -- variable
    show (UNIT _)       = "_"
    show (VAR name _)   = if isSymbolic name
                              then "("++ name ++")"
                              else name
    show (OPR sym _)    = sym
    -- tuple
    show (TUPLE xs _) = show |$> xs 
                          >- (intersperse ", " >-> fold)
                          >- \s -> "("++ s ++ ")"
    -- apply
    show (FIX lambda _)      = "(fix " ++ show lambda ++")"
    show (TPLPrj x n)        = show x ++"."++ show n
    show (TAGPrj x (name,n)) = "("++ show x ++")."++ name ++"["++ show n ++"]"
    -- syntax
    show (IF e1 e2 e3 _)  = "if "++ show e1 ++" then "++ show e2 ++" else "++ show e3
    show (CASE e pairs _) = "case "++ show e ++" of "++ (pairs <$| (\(pm,e) -> show pm ++" -> "++ show e)
                                                                 >- (intersperse " | " >-> fold))
    -- sentence
    show (TYPESig (name, ty) _) = if isSymbolic name
                                  then "("++ name ++") :: "++ show ty
                                  else name ++" :: "++ show ty
    show (DEF name defs)      = fold $ intersperse "\n" $ defs <$| showdef
      where
        showdef ([],s,_)  = showname ++" = "++ show s
        showdef (pms,s,_) = showname ++" "++ (unwords $ pms <$| show) ++" = "++ show s
        showname = if isSymbolic name
                   then "("++ name ++")"
                   else name
    show (BNF name tags _)  = "data "++ name ++" = "++ ((showTag |$> tags) >- (intersperse " | " >-> fold))
      where
        showTag (name, [])  = name
        showTag (name, tys) = name ++" "++  ((show |$> tys) >- (intersperse " " >-> fold))
    -- quote
    show (QUT t _)      = "{"++ show t ++ "}"
    show (QQUT t _)     = "`"++ show t
    show (UNQUT t _)    = ","++ show t
    -- syntactic-sugar
    show (APP e [] _)      = error "APP: empty args"
    show (APP e args _)    = "("++ show e ++" "++ (unwords $ args <$| show) ++")"
    show (APPSeq [] _)     = error "APPSeq: empty seq"
    show (APPSeq (x:[]) _) = show x
    show (APPSeq seq _)    = "("++ (unwords $ seq <$| show) ++")" 
    show (LAM []     e _) = error "LAM: empty params"
    show (LAM params e _) = "(\\"++ showParams ++"."++ show e ++")"
      where
        showParams = fold $ params <$| (\(pm, ty) -> show pm ++ showType ty)
                            >- intersperse " "                    
          where
            showType ty@(_ :-> _) = "::"++ "("++ show ty ++")"
            showType Ty.UNIT      = ""
            showType ty           = "::"++ show ty
    show (LAMM []     e _) = error "LAMM: empty params"
    show (LAMM params e _) = "("++ showParams ++ show e ++")"
      where
        showParams = fold $ params <$| (\(pm, ty) -> "#"++ show pm ++ showType ty ++ ".")                            
          where
            showType ty@(_ :-> _) = "::"++ "("++ show ty ++")"
            showType Ty.UNIT      = ""
            showType ty           = "::"++ show ty
    show (AS (e, ty@(_ :-> _)) _) = show e ++"::("++ show ty ++")"
    show (AS (e, ty) _)           = show e ++"::"++ show ty
    show (LET (pm,Ty.UNIT) e1 e2 _) = "let "++ show pm ++" = "++ show e1 ++" in "++ show e2
    show (LET (pm,ty)      e1 e2 _) = "let "++ show pm ++"::"++ show ty ++" = "++ show e1 ++" in "++ show e2
    show (LETREC (var,ty) e1 e2 _)  = "letrec "++ var ++"::"++ show ty ++" = "++ show e1 ++" in "++ show e2
    -- list
    show (NIL _)        = "[]"
    show x@(CONS a d _) = 
        if L.isList x
        then case a of
            CHAR _ _ -> show $ toString $ L.toList x
            _        -> show $ L.toList x
        else showParens a ++ ":"++ showParens d
      where
        toString []              = [] 
        toString ((CHAR c _):cs) = c : toString cs
        showParens x@(LAM _ _ _)  = "("++ show x ++")"
        showParens x@(FIX _ _)    = "("++ show x ++")"
        showParens x@(APP _ _ _)  = "("++ show x ++")"
        showParens x@(IF _ _ _ _) = "("++ show x ++")"
        showParens x@(CASE _ _ _) = "("++ show x ++")"
        showParens x              = show x
    show (HEAD x)        = "(head " ++ show x ++")"
    show (TAIL x)        = "(tail " ++ show x ++")"

