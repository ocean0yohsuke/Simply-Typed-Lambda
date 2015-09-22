{-# LANGUAGE 
        FunctionalDependencies,
        FlexibleInstances,
        UndecidableInstances
 #-}
module MonadX.Functor where 

import MonadX.Applicative

import Control.Monad
import Control.Category ((>>>), (<<<))
import Control.Monad.Error (Error(..))


class CommutativeFunctor f1 where
  commute :: (Applicative f2) => f1 (f2 a) -> f2 (f1 a)

instance CommutativeFunctor Maybe where
  commute (Just fa) = Just |$> fa
  commute Nothing = (*|) Nothing
-- TODO : traverse について
{-
*Main> :t \a -> 1
\a -> 1 :: Num a => t -> a
*Main> :t ((->) a) 1
<interactive>:1:3: parse error on input `->'
*Main> :t (->)
<interactive>:1:2: parse error on input `->'
*Main> :t ((->) r) |$> [1]
<interactive>:1:3: parse error on input `->'

Haskell が ((->) r) をデータ構築子として認識してくれない（パースエラー）。
そのため、 ((->) r) を含むデータタイプは Monad2 にできない。
その結果、 ((->) r) を含むデータタイプに関しては Monad Transformer を使わざるを得ない。

(((->) r)|$>) が使えないため、
Haskell　においては、
(\r -> M f(r))
を
M (\r -> f(r))
にする関数は存在しないとも言える
すなわち ((->) r)　は commute　が定義できない。
以下はエラーとなる。

instance CommutativeFunctor ((->) r) where
    commute (((->) r) fa) = ((->) r) |$> fa 
instance MonadExchange (Reader r) where
    commute (Reader ma) = (Reader . ((->) r)) |$> ma

よって、 ((->) r) は Monad2 のインスタンスを作成することはおそらくできない
以下のようにしたいが、((->) r) の commute が定義できていないためエラーになる

-- (((->) r)|$>) が使えないためこれはエラーとなる
instance Monad2 ((->) r) where
    mrv >>== f = 
        mrv >>= \rv
        let ((->) r) v = rv 
            mr'b = f v
            mb = (r >-) |$> mr'b
        in  ((->) r) |$> mb 

-- commute が定義できていないためエラーとなる
instance Monad2 ((->) r) where
    mrv >>== f = 
        mrv >>= \rv -> 
        commute $ \r ->
            let mrb = f $ rv r in
            let mb = (r >-) |$> mrb in
            mb

(Reader r) も同じ。

instance Monad2 (Reader r) where
    mmv >>== f = 
        mmv >>= \(Reader v) -> 
        commute $ 
            Reader $ \r ->
                v r >- \a -> 
                ((r >-) . runReader) |$> (f a)

Monad2　にできない、つまり >>== を定義できないという事は >>= を使わざるを得ない、つまり Monad Transformer　を使わざるを得ないという事だ。
Monad Transformer　を使わざるを得ないという事は Applicative な式が書けないという事だ。
それはすなわち do記法 に縛られる事を意味し、
それはすわなち 命令型プログラミング に縛られる事を意味し、
それはそなわち時間概念に縛られる事を意味する。

逆に言えば、 ((|->|) r m)), (ReaderT r m), (StateT s m) は (((->) r)|$>) の代用とも言える。 

-}      



{-
FunctionalDependencies を使っても駄目

{-# LANGUAGE 
        FunctionalDependencies,
        FlexibleInstances,
        UndecidableInstances
 #-}

class CommutativeFunctor a f1 | f1 -> a where
  commute :: (Applicative f2) => f1 (f2 a) -> f2 (f1 a)
instance CommutativeFunctor a Maybe where
  commute (Just fa) = Just |$> fa
  commute Nothing = (*|) Nothing

以下がエラーとなる。
instance CommutativeFunctor r ((->) r) where
    --commute (((->) r) fv) = ((->) r) |$> fv
    commute mfv = 
        let \r -> fv = mfv    -- ここがエラー
        in　　((->) r) |$> fv
-}


