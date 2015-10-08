module Lambda.DataType.Type where

import DeepControl.Applicative
import DeepControl.Monad
import Lambda.DataType.Common

import Data.List (intersperse)
import Data.Foldable (fold)

--------------------------------------------------
-- Type
--------------------------------------------------

infixr 9 :->
data Type = NULL -- ()
          | UNIT -- for sequence: (λ_.t2) t1
          | BOOL
          | INT
          | CHAR 
          | VAR Name
          | Type :-> Type
          | TUPLE [Type]
          | CONS Type
          | DATA Name   -- TODO: data Foo a =  -- TODO: BNF
          | QUT         -- quote
    deriving (Read)

instance Eq Type where
    NULL  == NULL    = True
    UNIT  == _       = True
    _     == UNIT    = True
    BOOL  == BOOL    = True
    INT   == INT     = True
    CHAR  == CHAR    = True
    VAR s == VAR s'  = s == s'
    a1 :-> a2 == b1 :-> b2 = (a1,a2) == (b1,b2)
    TUPLE x   == TUPLE y   = x == y
    CONS x    == CONS y    = x == y
    DATA x == DATA y = x == y
    QUT    == QUT    = True
    _      == _      = False
 
instance Show Type where
    show NULL = "()"
    show UNIT = "_"
    show BOOL = "Bool"
    show INT  = "Int"
    show CHAR = "Char"
    show (VAR s) = s
    show (t1@(_ :-> _) :-> t2) = "("++ show t1 ++") -> "++ show t2
    show (t1 :-> t2)           = show t1 ++" -> "++ show t2
    show (TUPLE xs) = show |$> xs
                          >- (intersperse ", " >-> fold)
                          >- \s -> "("++ s ++ ")"
    show (CONS ty) = "["++ show ty ++"]"
    show (DATA name) = name
    show QUT = "Quote"



