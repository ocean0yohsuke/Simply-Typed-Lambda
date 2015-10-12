module Lambda.DataType.Gadget where

import DeepControl.Applicative
import DeepControl.Monad hiding (mapM)

import Lambda.DataType.Common
import Lambda.DataType.PatternMatch (PM)
import Lambda.DataType.Type (Type)

import Prelude hiding (mapM)
import Data.List (intersperse)
import Data.Foldable (Foldable(..), fold)
import Data.Traversable (Traversable(..), fmapDefault, foldMapDefault, for)

---------------------------------------------------------------------------
-- Syntax
---------------------------------------------------------------------------

data Syntax a = IF a a a
              | CASE a [(PM, a)]
  deriving (Eq)

instance (Show a) => Show (Syntax a) where
    show (IF e1 e2 e3)  = "if "++ show e1 ++" then "++ show e2 ++" else "++ show e3
    show (CASE e pairs) = "case "++ show e ++" of\n"++ (pairs <$| (\(pm,e) -> "    "++ show pm ++" -> "++ show e)
                                                                >- (intersperse "\n" >-> fold))

instance Traversable Syntax where
    traverse f (IF a b c) = IF |$> f a |*> f b |*> f c
    traverse f (CASE a xs) = CASE |$> f a |*> for xs (\(pm, x) -> pm <|(,)|$> f x)
instance Functor Syntax where
    fmap = fmapDefault
instance Foldable Syntax where
    foldMap = foldMapDefault

---------------------------------------------------------------------------
-- Sentence
---------------------------------------------------------------------------

data Sentence a = TYPESig (Name, Type)      -- type signature   -- TODO: おそらく不要。 type casting が本質的
                | DEF Name a                -- function's definition
                | BNF Name [(Name, [Type])] -- BNF
  deriving (Eq)

instance Show a => Show (Sentence a) where
    show (TYPESig (name, ty)) = if isSymbolic name
                                then "("++ name ++") :: "++ show ty
                                else name ++" :: "++ show ty
    show (DEF name x)         = if isSymbolic name
                                then "("++ name ++") = "++ show x
                                else name ++" = "++ show x
    show (BNF name tags)      = "data "++ name ++" = "++ ((showTag |$> tags) >- (intersperse " | " >-> fold))
      where
        showTag (name, [])  = name
        showTag (name, tys) = name ++" "++  ((show |$> tys) >- (intersperse " " >-> fold))

instance Traversable Sentence where
    traverse f (TYPESig (name, ty)) = (*:) $ TYPESig (name, ty)
    traverse f (DEF name x)         = DEF name |$> (f x)
    traverse f (BNF name xs)        = (*:) $ BNF name xs
instance Functor Sentence where
    fmap = fmapDefault
instance Foldable Sentence where
    foldMap = foldMapDefault

---------------------------------------------------------------------------
-- Quote
---------------------------------------------------------------------------

data Quote a = QUOTE a      -- quote
             | QQUOTE a     -- quasi-quote
             | UNQUOTE a    -- unquote
  deriving (Eq)

instance Show a => Show (Quote a) where
    show (QUOTE x)      = "{"++ show x ++ "}"
    show (QQUOTE x)     = "`("++ show x ++")" -- TODO: showWithParens
    show (UNQUOTE x)    = ","++ show x        -- TODO: showWithParens

instance Traversable Quote where
    traverse f (QUOTE x)   = QUOTE |$> f x
    traverse f (QQUOTE x)  = QQUOTE |$> f x
    traverse f (UNQUOTE x) = UNQUOTE |$> f x
instance Functor Quote where
    fmap = fmapDefault
instance Foldable Quote where
    foldMap = foldMapDefault

---------------------------------------------------------------------------
-- LIST
---------------------------------------------------------------------------

data List a = NIL
            | CONS a a
            | HEAD a 
            | TAIL a 
  deriving (Eq)

instance Show a => Show (List a) where
    show NIL        = "[]"
    show (CONS a d) = show a ++":"++ show d    -- TODO: showWithParens
    show (HEAD x)   = "(head "++ show x ++")" 
    show (TAIL x)   = "(tail "++ show x ++")" 

instance Traversable List where
    traverse f NIL        = (*:) NIL
    traverse f (CONS a d) = CONS |$> f a |*> f d
    traverse f (HEAD x)   = HEAD |$> f x
    traverse f (TAIL x)   = TAIL |$> f x
instance Functor List where
    fmap = fmapDefault
instance Foldable List where
    foldMap = foldMapDefault


---------------------------------------------------------------------------
-- Tuple
---------------------------------------------------------------------------

data Tuple a = TUPLE [a]
             | TPLPrj a Index         -- tuple projection
  deriving (Eq)

instance Show a => Show (Tuple a) where
    show (TUPLE es)   = show |$> es 
                              >- (intersperse ", " >-> fold)
                              >- \s -> "("++ s ++ ")"
    show (TPLPrj x n) = show x ++"."++ show n

instance Traversable Tuple where
    traverse f (TUPLE xs)   = TUPLE |$> traverse f xs
    traverse f (TPLPrj x i) = TPLPrj |$> f x |* i
instance Functor Tuple where
    fmap = fmapDefault
instance Foldable Tuple where
    foldMap = foldMapDefault

---------------------------------------------------------------------------
-- Tag
---------------------------------------------------------------------------

data Tag a = TAGAs Name [a]         -- tag as
           | TAGPrj a (Name, Index) -- tag projection
  deriving (Eq)

instance Show a => Show (Tag a) where
    show (TAGAs name xs)       = name ++" "++ ((show |$> xs) >- (intersperse " " >-> fold))
    show (TAGPrj x (name,n)) = "("++ show x ++")."++ name ++"["++ show n ++"]"

instance Traversable Tag where
    traverse f (TAGAs name xs) = (TAGAs name) |$> traverse f xs
    traverse f (TAGPrj x p)  = TAGPrj |$> f x |* p
instance Functor Tag where
    fmap = fmapDefault
instance Foldable Tag where
    foldMap = foldMapDefault




