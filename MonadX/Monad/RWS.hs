{-# LANGUAGE MultiParamTypeClasses, 
             FlexibleInstances #-}

module MonadX.Monad.RWS (
	module MonadX.MonadTrans,
	MonadIO(..),

	MonadReader(..), MonadWriter(..), MonadState(..),

	RWS(..), rws, evalRWS, execRWS, mapRWS, withRWS,
	RWST(..), rwsT, evalRWST, execRWST, mapRWST, liftCatch,
	RWST2(..), rwsT2, evalRWST2, execRWST2, 

	) where 

import MonadX.Applicative
import MonadX.Monad
import MonadX.MonadTrans

import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Writer (MonadWriter(..))
import Control.Monad.State (MonadState(..))
import Data.Monoid

----------------------------------------------------------------------
-- Level-0
----------------------------------------------------------------------

newtype RWS r w s a = RWS { runRWS :: r -> s -> (a, s, w) }

instance Functor (RWS r w s) where
	fmap f m = RWS $ \r s ->
		(\(a, s', w) -> (f a, s', w)) $ runRWS m r s
instance (Monoid w) => Applicative (RWS r w s) where
	pure a	= RWS $ \_ s -> (a, s, mempty)
	(<*>)	= ap
instance (Monoid w) => Monad (RWS r w s) where
	return	= pure
	m >>= k	= RWS $ \r s -> 
		runRWS m r s >- \(a, s', w) ->
		runRWS (k a) r s' >- \(b, s'',w') ->
		(b, s'', w `mappend` w')
instance (Monoid w) => MonadReader r (RWS r w s) where
	ask       = RWS $ \r s -> (r, s, mempty)
	local f m = RWS $ \r s -> runRWS m (f r) s
instance (Monoid w) => MonadWriter w (RWS r w s) where
	writer (a, w) 	= RWS $ \_ s -> (a, s, w)
	tell w 			= RWS $ \_ s -> ((),s,w)
	listen m 		= RWS $ \r s -> 
		runRWS m r s >- \(a, s', w) ->
		((a, w), s', w)
	pass m 			= RWS $ \r s ->
		runRWS m r s >- \((a, f), s', w) ->
		(a, s', f w)
instance (Monoid w) => MonadState s (RWS r w s) where
	get 	= RWS $ \_ s -> (s, s, mempty)
	put s 	= RWS $ \_ _ -> ((), s, mempty)

rws :: (r -> s -> (a, s, w)) -> RWS r w s a
rws = RWS
evalRWS :: RWS r w s a -> r -> s -> (a, w)
evalRWS m r s =
	runRWS m r s >- \(a, _, w) ->
	(a, w)
execRWS :: RWS r w s a -> r -> s -> (s, w)
execRWS m r s =
	runRWS m r s >- \(_, s', w) ->
	(s', w)
mapRWS :: ((a, s, w) -> (b, s, w')) -> RWS r w s a -> RWS r w' s b
mapRWS f m = RWS $ \r s -> f (runRWS m r s)
withRWS :: (r' -> s -> (r, s)) -> RWS r w s a -> RWS r' w s a
withRWS f m = RWS $ \r s -> uncurry (runRWS m) (f r s)

----------------------------------------------------------------------
-- Level-1
----------------------------------------------------------------------

newtype RWST r w s m a = RWST { runRWST :: r -> s -> m (a, s, w) }

instance (Functor m) => Functor (RWST r w s m) where
	fmap f m = RWST $ \r s ->
		(\(a, s', w) -> (f a, s', w)) |$> runRWST m r s
instance (Monoid w, Applicative m, Monad m) => Applicative (RWST r w s m) where
    pure a = RWST $ \_ s -> (*:) (a, s, mempty)
    (<*>)  = ap
instance (Monoid w, Applicative m, Monad m) => Monad (RWST r w s m) where
	return  = pure
	m >>= k = RWST $ \r s -> 
		runRWST m r s >>= \(a, s', w) ->
		runRWST (k a) r s' >>= \(b, s'',w') ->
		(*:) (b, s'', w `mappend` w')
instance (Monoid w, Applicative m, Monad m) => MonadReader r (RWST r w s m) where
	ask       = RWST $ \r s -> (*:) (r, s, mempty)
	local f m = RWST $ \r s -> runRWST m (f r) s
instance (Monoid w, Applicative m, Monad m) => MonadWriter w (RWST r w s m) where
	writer (a, w) 	= RWST $ \_ s -> (*:) (a, s, w)
	tell w 			= RWST $ \_ s -> (*:) ((),s,w)
	listen m 		= RWST $ \r s -> 
		runRWST m r s >>= \(a, s', w) ->
		(*:) ((a, w), s', w)
	pass m 			= RWST $ \r s ->
		runRWST m r s >>= \((a, f), s', w) ->
		(*:) (a, s', f w)
instance (Monoid w, Applicative m, Monad m) => MonadState s (RWST r w s m) where
	get 	= RWST $ \_ s -> (*:) (s, s, mempty)
	put s 	= RWST $ \_ _ -> (*:) ((), s, mempty)

instance (Monoid w) => MonadTrans (RWST r w s) where
	trans m = RWST $ \r s -> 
		m >>= \a ->
		(*:) (a, s, mempty)
instance (Monoid w, MonadIO m, Applicative m, Monad m) => MonadIO (RWST r w s m) where
	liftIO = trans . liftIO

rwsT :: (Applicative m, Monad m) => (r -> s -> (a, s, w)) -> RWST r w s m a
rwsT = RWST . ((*:)|$>>)
evalRWST :: (Applicative m, Monad m) => RWST r w s m a -> r -> s -> m (a, w)
evalRWST m r s =
	runRWST m r s >>= \(a, _, w) ->
	(*:) (a, w)
execRWST :: (Applicative m, Monad m) => RWST r w s m a -> r -> s -> m (s, w)
execRWST m r s =
	runRWST m r s >>= \(_, s', w) ->
	(*:) (s', w)

-- | Map the inner computation using the given function.
-- runRWST (mapRWST f m) r s = f (runRWST m r s)
mapRWST :: (m (a, s, w) -> n (b, s, w')) -> RWST r w s m a -> RWST r w' s n b
mapRWST f m = RWST $ \ r s -> f (runRWST m r s)

-- | Lift a catchE operation to the new monad.
liftCatch :: Catch e m (a,s,w) -> Catch e (RWST r w s m) a
liftCatch catchE m h =
    RWST $ \ r s -> runRWST m r s `catchE` \ e -> runRWST (h e) r s

----------------------------------------------------------------------
-- Level-2
----------------------------------------------------------------------

newtype RWST2 r w s m1 m2 a = RWST2 { runRWST2 :: r -> s -> m1 (m2 (a, s, w)) }

instance (Functor m1, Functor m2) => Functor (RWST2 r w s m1 m2) where
	fmap f m = RWST2 $ \r s ->
		(\(a, s', w) -> (f a, s', w)) |$>> runRWST2 m r s
instance (Monoid w, Applicative m1, Monad m1, Monad2 m2) => Applicative (RWST2 r w s m1 m2) where
    pure a	= RWST2 $ \_ s -> (**:) (a, s, mempty)
    (<*>)	= ap
instance (Monoid w, Applicative m1, Monad m1, Monad2 m2) => Monad (RWST2 r w s m1 m2) where
	return	= pure
	m >>= k	= RWST2 $ \r s -> 
		runRWST2 m r s >>== \(a, s', w) ->
		runRWST2 (k a) r s' >>== \(b, s'',w') ->
		(**:) (b, s'', w `mappend` w')
instance (Monoid w, Applicative m1, Monad m1, Monad2 m2) => MonadReader r (RWST2 r w s m1 m2) where
	ask       = RWST2 $ \r s -> (**:) (r, s, mempty)
	local f m = RWST2 $ \r s -> runRWST2 m (f r) s
instance (Monoid w, Applicative m1, Monad m1, Monad2 m2) => MonadWriter w (RWST2 r w s m1 m2) where
	writer (a, w) 	= RWST2 $ \_ s -> (**:) (a, s, w)
	tell w 			= RWST2 $ \_ s -> (**:) ((),s,w)
	listen m 		= RWST2 $ \r s -> 
		runRWST2 m r s >>== \(a, s', w) ->
		(**:) ((a, w), s', w)
	pass m 			= RWST2 $ \r s ->
		runRWST2 m r s >>== \((a, f), s', w) ->
		(**:) (a, s', f w)
instance (Monoid w, Applicative m1, Monad m1, Monad2 m2) => MonadState s (RWST2 r w s m1 m2) where
	get 	= RWST2 $ \_ s -> (**:) (s, s, mempty)
	put s 	= RWST2 $ \_ _ -> (**:) ((), s, mempty)


instance (Monoid w) => MonadTrans2 (RWST2 r w s) where
	trans2 m = RWST2 $ \r s -> 
		m >>== \a ->
		(**:) (a, s, mempty)
instance (Monoid w, MonadIO m1, Applicative m1, Monad m1, Monad2 m2) => MonadIO (RWST2 r w s m1 m2) where
	liftIO = trans2 . (-*) . liftIO

rwsT2 :: (Applicative m1, Monad m1, Monad2 m2) => (r -> s -> (a, s, w)) -> RWST2 r w s m1 m2 a
rwsT2 = RWST2 . ((**:)|$>>)
evalRWST2 :: (Applicative m1, Monad m1, Monad2 m2) => RWST2 r w s m1 m2 a -> r -> s -> m1 (m2 (a, w))
evalRWST2 m r s =
	runRWST2 m r s >>== \(a, _, w) ->
	(**:) (a, w)
execRWST2 :: (Applicative m1, Monad m1, Monad2 m2) => RWST2 r w s m1 m2 a -> r -> s -> m1 (m2 (s, w))
execRWST2 m r s =
	runRWST2 m r s >>== \(_, s', w) ->
	(**:) (s', w)

