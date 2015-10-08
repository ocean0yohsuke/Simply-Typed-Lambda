{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
module Lambda.DataType.Shadow.Gadget_ where

import DeepControl.Applicative
import DeepControl.Monad
import Lambda.DataType.Common
import Lambda.DataType.Shadow.PatternMatch_ (PM_)
import Lambda.DataType.Type (Type)
import Util.Pseudo (Convert(..))

import Data.List (intersperse)
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Traversable (Traversable, fmapDefault, foldMapDefault)
import qualified Data.Traversable as T
import Lambda.DataType.Gadget (Syntax, Sentence, Quote, List, Tuple, Tag)
import qualified Lambda.DataType.Gadget as O -- original

---------------------------------------------------------------------------
-- Syntax
---------------------------------------------------------------------------

data Syntax_ a = IF a a a
               | CASE a [(PM_, a)]
  deriving (Show, Eq, Read)

instance (Convert a b) => Convert (Syntax a) (Syntax_ b) where
    convert (O.IF x1 x2 x3) = IF (convert x1) (convert x2) (convert x3)
    convert (O.CASE x xs)   = CASE (convert x) $ xs <$| \(pm,x) -> (convert pm, convert x)

-- TODO: type-family
instance (Convert a b) => Convert (Syntax_ a) (Syntax b) where
    convert (IF x1 x2 x3) = O.IF (convert x1) (convert x2) (convert x3)
    convert (CASE x xs)   = O.CASE (convert x) $ xs <$| \(pm,x) -> (convert pm, convert x)

---------------------------------------------------------------------------
-- Sentence
---------------------------------------------------------------------------

data Sentence_ a = TYPESig (Name, Type)     -- type signature
                 | DEF Name a               -- definition
                 | BNF Name [(Name, [Type])]
  deriving (Show, Eq, Read)

instance (Convert a b) => Convert (Sentence a) (Sentence_ b) where
    convert (O.TYPESig p)  = TYPESig p
    convert (O.DEF name x) = DEF name (convert x)
    convert (O.BNF p ps)   = BNF p ps
instance (Convert a b) => Convert (Sentence_ a) (Sentence b) where
    convert (TYPESig p)  = O.TYPESig p
    convert (DEF name x) = O.DEF name (convert x)
    convert (BNF p ps)   = O.BNF p ps

---------------------------------------------------------------------------
-- Quote
---------------------------------------------------------------------------

data Quote_ a = QUOTE a      -- quote
              | QQUOTE a     -- quasi-quote
              | UNQUOTE a    -- unquote
  deriving (Show, Eq, Read)

instance (Convert a b) => Convert (Quote a) (Quote_ b) where
    convert (O.QUOTE x)   = QUOTE (convert x)
    convert (O.QQUOTE x)  = QQUOTE (convert x)
    convert (O.UNQUOTE x) = UNQUOTE (convert x)
instance (Convert a b) => Convert (Quote_ a) (Quote b) where
    convert (QUOTE x)   = O.QUOTE (convert x)
    convert (QQUOTE x)  = O.QQUOTE (convert x)
    convert (UNQUOTE x) = O.UNQUOTE (convert x)

---------------------------------------------------------------------------
-- List
---------------------------------------------------------------------------

data List_ a = NIL
             | CONS a a
             | HEAD a 
             | TAIL a 
  deriving (Show, Eq, Read)

instance (Convert a b) => Convert (List a) (List_ b) where
    convert O.NIL        = NIL 
    convert (O.CONS a d) = CONS (convert a) (convert d)
    convert (O.HEAD x)   = HEAD (convert x)
    convert (O.TAIL x)   = TAIL (convert x)
instance (Convert a b) => Convert (List_ a) (List b) where
    convert NIL        = O.NIL 
    convert (CONS a d) = O.CONS (convert a) (convert d)
    convert (HEAD x)   = O.HEAD (convert x)
    convert (TAIL x)   = O.TAIL (convert x)

---------------------------------------------------------------------------
-- Tuple
---------------------------------------------------------------------------

data Tuple_ a = TUPLE [a]
              | TPLPrj a Index         -- tuple projection
  deriving (Show, Eq, Read)

instance (Convert a b) => Convert (Tuple a) (Tuple_ b) where
    convert (O.TUPLE xs)   = TUPLE (convert |$> xs) 
    convert (O.TPLPrj x i) = TPLPrj (convert x) i
instance (Convert a b) => Convert (Tuple_ a) (Tuple b) where
    convert (TUPLE xs)   = O.TUPLE (convert |$> xs) 
    convert (TPLPrj x i) = O.TPLPrj (convert x) i

---------------------------------------------------------------------------
-- Tag
---------------------------------------------------------------------------

data Tag_ a = TAGAs Name [a]          -- tag as
            | TAGPrj a (Name, Index)  -- tag projection
  deriving (Show, Eq, Read)

instance (Convert a b) => Convert (Tag a) (Tag_ b) where
    convert (O.TAGAs name xs) = TAGAs name (convert |$> xs) 
    convert (O.TAGPrj x p)    = TAGPrj (convert x) p
instance (Convert a b) => Convert (Tag_ a) (Tag b) where
    convert (TAGAs name xs) = O.TAGAs name (convert |$> xs) 
    convert (TAGPrj x p)    = O.TAGPrj (convert x) p



