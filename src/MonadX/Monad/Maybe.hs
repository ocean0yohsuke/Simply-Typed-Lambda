{-# LANGUAGE FlexibleInstances #-}

module MonadX.Monad.Maybe (
    MonadIO(..),
    MonadPlus(..),

    MaybeT(..), 
    mapMaybeT, liftCatch,

    ) where 

import DeepControl.Applicative
import DeepControl.Monad
import MonadX.MonadTrans

----------------------------------------------------------------------
-- MaybeT
----------------------------------------------------------------------

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
    fmap f = \(MaybeT mmv) -> MaybeT $ f |$>> mmv
instance (Applicative m, Monad m) => Applicative (MaybeT m) where
    pure a = MaybeT $ (**:) a
    (<*>) = ap
--  (<*>) = \(MaybeT mmf) (MaybeT mma) ->
--      MaybeT $ mmf |*>> mma
instance (Applicative m, Monad m) => Monad (MaybeT m) where
    return = pure
    (MaybeT mmv) >>= f = MaybeT $ 
        mmv >>== \v -> 
        runMaybeT (f v)

instance MonadTrans MaybeT where
    trans = MaybeT . (-*)

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO = trans . liftIO

-- | Transform the computation inside a MaybeT.
-- runMaybeT (mapMaybeT f m) = f (runMaybeT m)
mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f = MaybeT . f . runMaybeT

-- | Lift a catchE operation to the new monad.
liftCatch :: Catch e m (Maybe a) -> Catch e (MaybeT m) a
liftCatch f m h = MaybeT $ f (runMaybeT m) (runMaybeT . h)

{-
Prelude Control.Monad.Error Control.Monad.Trans.Maybe> :t liftCatch
liftCatch
  :: (m (Maybe a) -> (e -> m (Maybe a)) -> m (Maybe a))
     -> MaybeT m a -> (e -> MaybeT m a) -> MaybeT m a
Prelude Control.Monad.Error Control.Monad.Trans.Maybe> :t catchError
catchError :: MonadError e m => m a -> (e -> m a) -> m a
Prelude Control.Monad.Error Control.Monad.Trans.Maybe> :t liftCatch catchError
liftCatch catchError
  :: MonadError e m => MaybeT m a -> (e -> MaybeT m a) -> MaybeT m a
-}

