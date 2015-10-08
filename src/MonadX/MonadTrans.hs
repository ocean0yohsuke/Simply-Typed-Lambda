module MonadX.MonadTrans (
    Catch,

    MonadTrans(..), MonadTrans2(..),
    MonadIO(..),
) where

import DeepControl.Applicative
import DeepControl.Monad

----------------------------------------------------------------------
-- Level-1
----------------------------------------------------------------------

type Catch e m a = m a -> (e -> m a) -> m a

class  MonadTrans t  where
    trans :: (Applicative m, Monad m) => m a -> t m a

class (Applicative m, Monad m) => MonadIO m where
    -- | lift a computation from the IO monad.
    liftIO :: IO a -> m a
instance MonadIO IO where
    liftIO = id

----------------------------------------------------------------------
-- Level-2
----------------------------------------------------------------------

class  MonadTrans2 t  where
    trans2 :: (Applicative m1, Monad m1, Monad2 m2) => m1 (m2 a) -> t m1 m2 a


