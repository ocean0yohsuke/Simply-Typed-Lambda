{-# LANGUAGE MultiParamTypeClasses, 
             FlexibleInstances #-}

module MonadX.Monad.Writer (
    module MonadX.MonadTrans,
    MonadIO(..),

    MonadWriter(..),
    listens, censor,

    Writer(..), execWriter, mapWriter,
    WriterT(..), execWriterT, mapWriterT, liftCatch,
    WriterT2(..), execWriterT2, 

    ) where 

import MonadX.Applicative
import MonadX.Monad
import MonadX.MonadTrans

import Control.Monad.Writer (MonadWriter(..))
import Data.Monoid (Monoid(..))

listens :: MonadWriter w m => (w -> b) -> m a -> m (a, b)
listens f m = do
    (a, w) <- listen m
    return (a, f w)

censor :: MonadWriter w m => (w -> w) -> m a -> m a
censor f m = pass $ do
    a <- m
    return (a, f)

----------------------------------------------------------------------
-- Writer
----------------------------------------------------------------------

newtype Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where
    fmap f v = Writer $ (\(a, w) -> (f a, w)) $ (runWriter v)
instance (Monoid w) => Applicative (Writer w) where
    pure a = Writer $ (a, mempty)
--  (<*>) = ap
    (<*>) = \(Writer (f, w)) (Writer (a, w')) ->
        Writer (f a, w' `mappend` w)
instance (Monoid w) => Monad (Writer w) where
    return   = (*|)
--  mv >>= f = 
--      mv >- \(Writer (a, w)) -> 
--      (\(Writer (b, w')) -> Writer (b, w `mappend` w')) $ f a
    mv >>= f = (Writer (fst.runWriter |$> f, snd.runWriter $ f (fst.runWriter $ mv))) |*> mv
instance (Monoid w) => Monad2 (Writer w) where
    mmv >>== f = 
        mmv >>= \(Writer (a, w)) -> 
        (\(Writer (b, w')) -> Writer (b, w `mappend` w')) |$> f a
instance (Monoid w) => Monad3 (Writer w) where
    mmv >>>== f = 
        mmv >>== \(Writer (a, w)) -> 
        (\(Writer (b, w')) -> Writer (b, w `mappend` w')) |$>> f a

instance (Monoid w) => MonadWriter w (Writer w) where
    writer   = Writer
    tell w   = writer ((), w)
    listen m = Writer $ 
        runWriter m >- \(a, w) ->
        ((a, w), w) 
    pass m   = Writer $ 
        runWriter m >- \((a, f), w) ->
        (a, f w)

execWriter :: Writer w a -> w
execWriter m =
    runWriter m >- \(_, w) ->
    w

mapWriter :: ((a, w) -> (b, w')) -> Writer w a -> Writer w' b
mapWriter f m = Writer $ f (runWriter m)

----------------------------------------------------------------------
-- WriterT
----------------------------------------------------------------------

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

instance (Applicative m, Monad m) => Functor (WriterT w m) where
    fmap f v = WriterT $ (\(a, w) -> (f a, w)) |$> (runWriterT v)
instance (Monoid w, Applicative m, Monad m) => Applicative (WriterT w m) where
    pure a = WriterT $ (*|) (a, mempty)
    (<*>)  = ap
instance (Monoid w, Applicative m, Monad m) => Monad (WriterT w m) where  
    return            = (*|)
    (WriterT v) >>= f = WriterT $
        v >>= \(a, w) ->
        runWriterT (f a) >>= \(a', w') ->
        (*|) (a', w `mappend` w')
instance (Monoid w, Applicative m, Monad m) => MonadWriter w (WriterT w m) where
    writer   = WriterT . (*|)
    tell w   = writer $ ((), w)
    listen m = WriterT $
        runWriterT m >>= \(a, w) ->
        (*|) ((a, w), w) 
    pass m   = WriterT $
        runWriterT m >>= \((a, f), w) ->
        (*|) (a, f w)

instance (Monoid w) => MonadTrans (WriterT w) where
    trans m = WriterT $ 
        m >>= \a ->
        (*|) (a, mempty)
instance (Monoid w, MonadIO m, Applicative m, Monad m) => MonadIO (WriterT w m) where
    liftIO = (|-) . liftIO

execWriterT :: (Applicative m, Monad m) => WriterT w m a -> m w
execWriterT m =
    runWriterT m >>= \(_, w) ->
    (*|) w

-- | Map both the return value and output of a computation using the given function.
-- runWriterT (mapWriterT f m) = f (runWriterT m)
mapWriterT :: (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f m = WriterT $ f (runWriterT m)

-- | Lift a catchE operation to the new monad.
liftCatch :: Catch e m (a,w) -> Catch e (WriterT w m) a
liftCatch catchE m h =
    WriterT $ runWriterT m `catchE` \ e -> runWriterT (h e)

----------------------------------------------------------------------
-- WriterT2
----------------------------------------------------------------------

newtype WriterT2 w m1 m2 a = WriterT2 { runWriterT2 :: m1 (m2 (a, w)) }

instance (Applicative m1, Monad m1, Monad2 m2) => Functor (WriterT2 w m1 m2) where
    fmap f v = WriterT2 $ (\(a, w) -> (f a, w)) |$>> (runWriterT2 v)
instance (Monoid w, Applicative m1, Monad m1, Monad2 m2) => Applicative (WriterT2 w m1 m2) where
    pure a = WriterT2 $ (**|) (a, mempty)
    (<*>)  = ap
instance (Monoid w, Applicative m1, Monad m1, Monad2 m2) => Monad (WriterT2 w m1 m2) where  
    return             = (*|)
    (WriterT2 v) >>= f = WriterT2 $
        v >>== \(a, w) ->
        runWriterT2 (f a) >>== \(a', w') ->
        (**|) (a', w `mappend` w')
instance (Monoid w, Applicative m1, Monad m1, Monad2 m2) => MonadWriter w (WriterT2 w m1 m2) where
    writer   = WriterT2 . (**|)
    tell w   = writer $ ((), w)
    listen m = WriterT2 $
        runWriterT2 m >>== \(a, w) ->
        (**|) ((a, w), w) 
    pass m   = WriterT2 $
        runWriterT2 m >>== \((a, f), w) ->
        (**|) (a, f w)

instance (Monoid w) => MonadTrans2 (WriterT2 w) where
    trans2 m = WriterT2 $ 
        m >>== \a ->
        (**|) (a, mempty)
instance (Monoid w, MonadIO m1, Applicative m1, Monad m1, Monad2 m2) => MonadIO (WriterT2 w m1 m2) where
    liftIO = (|-*|) . liftIO

execWriterT2 :: (Applicative m1, Monad m1, Monad2 m2) => WriterT2 w m1 m2 a -> m1 (m2 w)
execWriterT2 m =
    runWriterT2 m >>== \(_, w) ->
    (**|) w

