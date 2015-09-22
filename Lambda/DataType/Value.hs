module Lambda.DataType.Value where

import MonadX.Applicative
import MonadX.Monad

import Lambda.DataType.Common
import Lambda.DataType.PatternMatch (PM)
import qualified Lambda.DataType.PatternMatch as PM
import Lambda.DataType.Type (Type)
import qualified Lambda.DataType.Type as Ty

import Data.List (intersperse)
import qualified Data.Foldable as F

-------------------------------------
-- Value
-------------------------------------

data Value = BOOL Bool
           | INT Integer
           | CHAR Char
           | LIST [Value]
           | TUPLE [Value]
           | LAM (PM, Type) Value
           | LAMM (PM, Type) Value
           | TAG Name [Value]

instance Eq Value where
    BOOL x  == BOOL y  = x == y
    INT x   == INT y   = x == y
    CHAR x  == CHAR y  = x == y
    LIST x  == LIST y  = x == y
    TUPLE x == TUPLE y = x == y
    LAM x1 x2 == LAM y1 y2 = (x1,x2) == (y1,y2)
    TAG x1 x2 == TAG y1 y2 = (x1,x2) == (y1,y2)
    _       == _       = False

instance Show Value where
    show (BOOL bool)   = show bool
    show (INT n)       = show n
    show (CHAR c)      = "'"++ [c] ++"'"
    show (LIST xs)     = show |$> xs 
                              >- (intersperse ", " >-> F.fold)
                              >- \s -> "["++ s ++ "]"
    show (TUPLE es)   = show |$> es 
                              >- (intersperse ", " >-> F.fold)
                              >- \s -> "("++ s ++ ")"
    show (LAM _ t) = "\\ "++ show t
    show (TAG name xs) = name ++" "++ ((show |$> xs) >- (intersperse " " >-> F.fold))


