{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             UndecidableInstances #-}

module MonadX.Monad.State (
    module MonadX.MonadTrans,
    MonadIO(..),

    MonadState(..),
    modify, gets,

    StateT(..), evalStateT, execStateT, mapStateT, liftCatch,
    StateT2(..), evalStateT2, execStateT2

    ) where 

import DeepControl.Applicative
import DeepControl.Monad
import MonadX.MonadTrans

import Control.Monad.State (MonadState(..))

modify :: MonadState s m => (s -> s) -> m ()
modify f = state $ \s -> ((), f s)

gets :: MonadState s m => (s -> a) -> m a
gets f = state $ \s -> (f s, s)

----------------------------------------------------------------------
-- State
----------------------------------------------------------------------

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f v = State $ \s ->
        (\(a, s') -> (f a, s')) $ runState v s
instance Applicative (State s) where
    pure a = State $ \s -> (a,s) 
    (<*>) = ap
instance Monad (State s) where  
    return          = (*:)
    (State v) >>= f = 
        State $ \s -> 
            v s >- \(a, s') ->
            runState (f a) s'

instance MonadState s (State s) where
    get   = State $ \s -> (s, s)
    put s = State $ \_ -> ((), s)

evalState :: State s a -> s -> a
evalState m s = 
    let (a, _) = runState m s
    in a
execState :: State s a -> s -> s
execState m s = 
    let (_, s') = runState m s
    in s'
mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f m = State $ f . runState m
withState :: (s -> s) -> State s a -> State s a
withState f m = State $ runState m . f

----------------------------------------------------------------------
-- StateT
----------------------------------------------------------------------

newtype StateT s m a = StateT { runStateT :: (s -> m (a,s)) }

instance (Functor m) => Functor (StateT s m) where
    fmap f v = StateT $ \s ->
        (\(a, s') -> (f a, s')) |$> runStateT v s
instance (Applicative m, Monad m) => Applicative (StateT s m) where
    pure a = StateT $ \s -> (*:) (a,s)
    (<*>)  = ap
instance (Applicative m, Monad m) => Monad (StateT s m) where
    return           = (*:)
    (StateT v) >>= f = 
        StateT $ \s -> 
            v s >>= \(a, s') ->
            runStateT (f a) s'
instance (Applicative m, Monad m) => MonadState s (StateT s m) where
    get   = StateT $ \s -> (*:) (s, s)
    put s = StateT $ \_ -> (*:) ((), s)

instance MonadTrans (StateT s) where
    trans m = StateT $ \s -> 
        m >>= \a ->
        (*:) (a, s)
instance (MonadIO m, Applicative m, Monad m) => MonadIO (StateT s m) where
    liftIO = trans . liftIO

evalStateT :: (Applicative m, Monad m) => StateT s m a -> s -> m a
evalStateT m s = 
    runStateT m s >>= \(a, _) ->
    (*:) a
execStateT :: (Applicative m, Monad m) => StateT s m a -> s -> m s
execStateT m s = 
    runStateT m s >>= \(_, s') ->
    (*:) s'

-- | Map both the return value and final state of a computation using the given function.
-- runStateT (mapStateT f m) = f . runStateT m
mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f m = StateT $ f . runStateT m

-- | Lift a catchE operation to the new monad.
liftCatch :: Catch e m (a,s) -> Catch e (StateT s m) a
liftCatch catchE m h =
    StateT $ \ s -> runStateT m s `catchE` \ e -> runStateT (h e) s

----------------------------------------------------------------------
-- StateT2
----------------------------------------------------------------------

newtype StateT2 s m1 m2 a = StateT2 { runStateT2 :: (s -> m1 (m2 (a,s))) }

instance (Functor m1, Functor m2) => Functor (StateT2 s m1 m2) where
    fmap f v = StateT2 $ \s ->
        (\(a, s') -> (f a, s')) |$>> runStateT2 v s
instance (Applicative m1, Monad m1, Monad2 m2) => Applicative (StateT2 s m1 m2) where
    pure a = StateT2 $ \s -> (**:) (a,s)
    (<*>) = ap
instance (Applicative m1, Monad m1, Monad2 m2) => Monad (StateT2 s m1 m2) where
    return            = (*:)
    (StateT2 v) >>= f = 
        StateT2 $ \s -> 
            v s >>== \(a, s') ->
            runStateT2 (f a) s'
instance (Applicative m1, Monad m1, Monad2 m2) => MonadState s (StateT2 s m1 m2) where
    get   = StateT2 $ \s -> (**:) (s, s)
    put s = StateT2 $ \_ -> (**:) ((), s)

instance MonadTrans2 (StateT2 s) where
    trans2 m = StateT2 $ \s -> 
        m >>== \a ->
        (**:) (a, s)
instance (MonadIO m1, Applicative m1, Monad m1, Monad2 m2) => MonadIO (StateT2 s m1 m2) where
    liftIO = trans2 . (-*) . liftIO

evalStateT2 :: (Applicative m1, Monad m1, Monad2 m2) => StateT2 s m1 m2 a -> s -> m1 (m2 a)
evalStateT2 m s = 
    runStateT2 m s >>== \(a, _) ->
    (**:) a
execStateT2 :: (Applicative m1, Monad m1, Monad2 m2) => StateT2 s m1 m2 a -> s -> m1 (m2 s)
execStateT2 m s = 
    runStateT2 m s >>== \(_, s') ->
    (**:) s'


