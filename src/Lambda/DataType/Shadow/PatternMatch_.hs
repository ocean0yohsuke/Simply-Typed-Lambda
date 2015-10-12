{-# LANGUAGE MultiParamTypeClasses #-}
module Lambda.DataType.Shadow.PatternMatch_ where

import DeepControl.Applicative
import DeepControl.Monad

import Lambda.DataType.Common
import Lambda.DataType.PatternMatch (PatternMatch)
import qualified Lambda.DataType.PatternMatch as O -- original
import Util.Pseudo (Convert(..))

--------------------------------------------------
-- PatternMatch
--------------------------------------------------

data PatternMatch_ = UNIT 
                   | VAR Name 
                   | BOOL Bool
                   | INT Integer
                   | CHAR Char
                   | NIL
                   | CONS PM_ PM_
                   | TUPLE [PM_]
                   | TAG Name [PM_]
    deriving (Show, Eq, Read)
type PM_ = PatternMatch_

instance Convert PatternMatch PatternMatch_ where
    convert (O.UNIT _)        = UNIT
    convert (O.VAR name _)    = VAR name
    convert (O.BOOL bool _)   = BOOL bool
    convert (O.INT n _)       = INT n
    convert (O.CHAR c _)      = CHAR c
    convert (O.NIL _)         = NIL
    convert (O.CONS x y _)    = CONS (convert x) (convert y)
    convert (O.TUPLE xs _)    = TUPLE (xs <$| convert)
    convert (O.TAG name xs _) = TAG name (xs <$| convert)

instance Convert PatternMatch_ PatternMatch where
    convert UNIT          = O.UNIT Nothing
    convert (VAR name)    = O.VAR name Nothing
    convert (BOOL bool)   = O.BOOL bool Nothing
    convert (INT n)       = O.INT n Nothing
    convert (CHAR c)      = O.CHAR c Nothing
    convert NIL           = O.NIL Nothing
    convert (CONS x y)    = O.CONS (convert x) (convert y) Nothing
    convert (TUPLE xs)    = O.TUPLE (xs <$| convert) Nothing
    convert (TAG name xs) = O.TAG name (xs <$| convert) Nothing

