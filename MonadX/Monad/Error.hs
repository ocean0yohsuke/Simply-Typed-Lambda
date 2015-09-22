{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
 #-}

module MonadX.Monad.Error (
      Error(..)
    , ErrorT(..)
	, MonadError(..)

    , mapErrorT, liftCatch,
	) where 

import MonadX.Applicative
import MonadX.Monad
import MonadX.MonadTrans
import Control.Monad.Error (Error(..), MonadError(..))

----------------------------------------------------------------------
-- ErrorT
----------------------------------------------------------------------

newtype ErrorT e m a = ErrorT { runErrorT :: m (Either e a) } 

instance (Error e, Functor m) => Functor (ErrorT e m) where
    fmap f = \(ErrorT mmv) -> ErrorT $ f |$>> mmv
instance (Error e, Applicative m, Monad m) => Applicative (ErrorT e m) where
    pure a = ErrorT $ (*:) (Right a)
    (<*>)  = ap
instance (Error e, Applicative m, Monad m) => Monad (ErrorT e m) where
    return             = pure
    (ErrorT mmv) >>= f = ErrorT $ 
        mmv >>== \v ->
        runErrorT (f v)
    fail msg           = ErrorT $ (*:) (Left (strMsg msg))

instance (Error e, Applicative m, Monad m) => MonadError e (ErrorT e m) where
    throwError e = ErrorT $ (*:) (Left e)
    m `catchError` h = ErrorT $ do
        a <- runErrorT m
        case a of
            Left  l -> runErrorT (h l)
            Right r -> (*:) (Right r)

instance (Error e) => MonadTrans (ErrorT e) where
    trans = ErrorT . (-*)
instance (Error e, MonadIO m, Applicative m, Monad m) => MonadIO (ErrorT e m) where
	liftIO = trans . liftIO

-- | Map the unwrapped computation using the given function.
-- runErrorT (mapErrorT f m) = f (runErrorT m)
mapErrorT :: (m (Either e a) -> n (Either e' b))
          -> ErrorT e m a
          -> ErrorT e' n b
mapErrorT f = ErrorT . f . runErrorT

-- | Lift a catchE operation to the new monad.
liftCatch :: Catch e m (Either e' a) -> Catch e (ErrorT e' m) a
liftCatch f m h = ErrorT $ f (runErrorT m) (runErrorT . h)

