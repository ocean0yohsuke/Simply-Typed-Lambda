{-# LANGUAGE MultiParamTypeClasses, 
             FlexibleInstances #-}

module MonadX.Monad.Reader (
    module MonadX.MonadTrans,
    module DeepControl.Monad.Reader,
    MonadIO(..),

    MonadReader(..),
    asks,

    ReaderT(..),
    ReaderT2(..),

    mapReaderT, liftCatch,

    ) where 

import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.Monad.Reader
import MonadX.MonadTrans

import Control.Monad.Reader (MonadReader(..))
--import Control.Monad.IO.Class (MonadIO(..))

----------------------------------------------------------------------
-- ReaderT
----------------------------------------------------------------------

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
    fmap f m = ReaderT $ \r ->
        f |$> runReaderT m r
instance (Applicative m, Monad m) => Applicative (ReaderT s m) where
    pure a = ReaderT $ \_ -> (*:) a
    (<*>) = ap
instance (Applicative m, Monad m) => Monad (ReaderT r m) where
    return = (*:)
    (ReaderT v) >>= f = ReaderT $ \r ->
        v r >>= \a -> 
        runReaderT (f a) r
instance (Applicative m, Monad m) => MonadReader r (ReaderT r m) where
    ask       = ReaderT $ (*:)
    local f m = ReaderT $ runReaderT m . f

instance MonadTrans (ReaderT r) where
    trans m = ReaderT $ \r -> 
        m >>= \a ->
        (*:) a
instance (MonadIO m, Applicative m, Monad m) => MonadIO (ReaderT r m) where
    liftIO = trans . liftIO

-- | Transform the computation inside a ReaderT.
-- runReaderT (mapReaderT f m) = f . runReaderT m
mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
mapReaderT f m = ReaderT $ f . runReaderT m

-- | Lift a catchE operation to the new monad.
liftCatch :: Catch e m a -> Catch e (ReaderT r m) a
liftCatch f m h =
    ReaderT $ \ r -> f (runReaderT m r) (\ e -> runReaderT (h e) r)

----------------------------------------------------------------------
-- ReaderT2
----------------------------------------------------------------------

newtype ReaderT2 r m1 m2 a = ReaderT2 { runReaderT2 :: r -> m1 (m2 a) }

instance (Functor m1, Functor m2) => Functor (ReaderT2 r m1 m2) where
    fmap f m = ReaderT2 $ \r ->
        f |$>> runReaderT2 m r
instance (Applicative m1, Monad m1, Monad2 m2) => Applicative (ReaderT2 s m1 m2) where
    pure a = ReaderT2 $ \_ -> (**:) a
    (<*>)  = ap
instance (Applicative m1, Monad m1, Monad2 m2) => Monad (ReaderT2 r m1 m2) where
    return = (*:)
    (ReaderT2 v) >>= f = ReaderT2 $ \r ->
        v r >>== \a -> 
        runReaderT2 (f a) r
instance (Applicative m1, Monad m1, Monad2 m2) => MonadReader r (ReaderT2 r m1 m2) where
    ask       = ReaderT2 $ (**:)
    local f m = ReaderT2 $ runReaderT2 m . f

instance MonadTrans2 (ReaderT2 r) where
    trans2 m = ReaderT2 $ \r -> 
        m >>== \a ->
        (**:) a
instance (MonadIO m1, Applicative m1, Monad m1, Monad2 m2) => MonadIO (ReaderT2 r m1 m2) where
    liftIO = trans2 . (-*) . liftIO


