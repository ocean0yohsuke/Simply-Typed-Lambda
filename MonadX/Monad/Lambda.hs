module MonadX.Monad.Lambda where 

import MonadX.Applicative
import MonadX.Monad
import MonadX.MonadTrans

{-
*Main> :t \a -> 1
\a -> 1 :: Num a => t -> a
*Main> :t ((->) a) 1
<interactive>:1:3: parse error on input `->'
*Main> :t (->)
<interactive>:1:2: parse error on input `->'
*Main> :t ((->) r) |$> [1]
<interactive>:1:3: parse error on input `->'
-}


data Lambda a b = LAMDA a b

--instance Show Lambda where
--  show (LAMDA x y) = "\\" ++ show 

instance Functor (Lambda a) where
    fmap f (LAMDA a b) = LAMDA a $ f b 


----------------------------------------------------------------------------
-- (|->|) r m) : ((->) r) の Monad Transformer 版
-- これが作れれば (ReaderT r m) と (StateT s m) は不要になる？
----------------------------------------------------------------------------




















