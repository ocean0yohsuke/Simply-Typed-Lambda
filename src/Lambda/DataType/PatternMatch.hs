module Lambda.DataType.PatternMatch where

import DeepControl.Applicative
import DeepControl.Monad

import Lambda.DataType.Common
import qualified Lambda.DataType.Type as Ty
import qualified Util.LISP as L

import Prelude hiding (concat)
import Data.List (intersperse)
import Data.Foldable

--------------------------------------------------
-- PatternMatch
--------------------------------------------------

data PatternMatch = UNIT MSP
                  | VAR Name MSP
                  | BOOL Bool MSP
                  | INT Integer MSP
                  | CHAR Char MSP
                  | NIL MSP
                  | CONS PM PM MSP
                  | TUPLE [PM] MSP
                  | TAG Name [PM] MSP
type PM = PatternMatch

unit = UNIT Nothing
var name = VAR name Nothing
bool b = BOOL b Nothing
int n = INT n Nothing
char c = CHAR c Nothing
nil = NIL Nothing
cons a d = CONS a d Nothing
tuple pms = TUPLE pms Nothing
tag name pms = TAG name pms Nothing

instance L.LISP PatternMatch where
    car (CONS a _ _) = a
    car _            = error "car: empty structure"
    cdr (CONS _ d _) = d
    cdr _            = error "cdr: empty structure"
    cons a d = CONS a d Nothing
    nil = NIL Nothing
    isCell (CONS _ _ _) = True
    isCell _            = False

instance Show PatternMatch where
    show (UNIT _)       = "_"
    show (VAR name _)   = name
    show (BOOL bool _)  = show bool
    show (INT n _)      = show n
    show (CHAR c _)     = "'"++ [c] ++"'"
    show (NIL _)        = "[]"
    show (CONS x y _)   = "("++ show x ++":"++ show y ++")"
    show (TUPLE xs _)   = xs <$| show
                            >- (intersperse ", " >-> fold)
                            >- \s -> "("++ s ++ ")"
    show (TAG name [] _) = name
    show (TAG name xs _) = name ++" "++ ((show |$> xs) >- (intersperse " " >-> fold))

instance Eq PatternMatch where
    UNIT _       == _            = True
    _            == UNIT _       = True
    VAR _ _      == _            = True
    _            == VAR _ _      = True
    BOOL x _     == BOOL y _     = x == y
    INT x _      == INT y _      = x == y
    CHAR x _     == CHAR y _     = x == y
    NIL _        == NIL _        = True
    CONS x1 x2 _ == CONS y1 y2 _ = (x1,x2) == (y1,y2)
    TUPLE x _    == TUPLE y _    = x == y
    TAG x1 x2 _  == TAG y1 y2 _  = (x1,x2) == (y1,y2)
    _            == _            = False


