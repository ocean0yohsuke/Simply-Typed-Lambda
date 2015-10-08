{-# LANGUAGE 
        DeriveFunctor,
        GeneralizedNewtypeDeriving,

        FlexibleInstances,
        FunctionalDependencies
 #-}
module MonadX.Monad.Reference (
    Ref, RefMap, RefEnv,

    initReference,
    MonadReference(..),  

    Reference(..), unReference, evalReference, mapReference,
    ReferenceT(..), unReferenceT, evalReferenceT, mapReferenceT, liftCatch,
    ReferenceT2(..), unReferenceT2, evalReferenceT2,

    ) where 

import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.Monad.State
import MonadX.Monad.State hiding (liftCatch)
import MonadX.MonadTrans

import qualified Data.Map as M

--------------------------------------------------------------------------
-- 
--------------------------------------------------------------------------

-- Ref
data Ref v = Ref Name Counter  
  deriving (Show, Eq, Ord)

type Name = String
type Counter = Int

-- RefEnv
type RefEnv v = (Name, Counter, RefMap v) 
type RefMap v = M.Map (Ref v) v

initReference :: Name -> RefEnv v
initReference name = (name, 0, M.empty)

--------------------------------------------------------------------------
-- Ref
--------------------------------------------------------------------------

newtype Reference v a = Reference (State (RefEnv v) a)
  deriving (Functor, Applicative, Monad, MonadState (RefEnv v))

--
-- MonadReference
--
class (Applicative m, Monad m) => MonadReference v m | m -> v where
    newRef :: v -> m (Ref v)
    readRef :: Ref v -> m v
    writeRef :: Ref v -> v -> m ()
    modifyRef :: Ref v -> (v -> v) -> m ()
    modifyRef ref f = do
       x <- readRef ref
       let x' = f x
       x' `seq` writeRef ref x'

instance MonadReference v (Reference v) where
    newRef v = do
        (name, counter, map) <- get
        let ref = Ref name counter
        put (name, counter+1, M.insert ref v map)
        (*:) ref
    readRef ref@(Ref name _) = do
        (name', counter, map) <- get
        if name /= name'
        then error $ "readRef: wrong type of reference '" ++ name ++ "' was detected."
        else case M.lookup ref map of
                Just v  -> (*:) v
                Nothing -> error $ "readRef: failed to find any actual from reference " ++ show ref
    writeRef ref@(Ref name _) v = do
        (name', counter, map) <- get
        if name /= name'
        then error $ "readRef: wrong type of reference '" ++ name ++ "' was detected."
        else put (name, counter, M.insert ref v map)

unReference :: Reference v a -> RefEnv v -> (a, RefEnv v)
unReference (Reference state) = runState state
evalReference :: Reference v a -> RefEnv v -> a
evalReference (Reference state) = evalState state

mapReference :: ((a, RefEnv v) -> (b, RefEnv v)) -> Reference v a -> Reference v b
mapReference f m = Reference $ State $ f . unReference m

--------------------------------------------------------------------------
-- ReferenceT
--------------------------------------------------------------------------

newtype ReferenceT v m a = ReferenceT (StateT (RefEnv v) m a)
  deriving (Functor, Applicative, Monad, (MonadState (RefEnv v)), MonadTrans, MonadIO)

instance (Applicative m, Monad m) => MonadReference v (ReferenceT v m) where
    newRef v = do
        (name, counter, map) <- get
        let ref = Ref name counter
        put (name, counter+1, M.insert ref v map)
        (*:) ref
    readRef ref@(Ref name _) = do
        (name', counter, map) <- get
        if name /= name'
        then error $ "readRef: wrong type of reference '" ++ name ++ "' was detected."
        else case M.lookup ref map of
                Just v  -> (*:) v
                Nothing -> error $ "readRef: failed to find any actual from reference " ++ show ref
    writeRef ref@(Ref name _) v = do
        (name', counter, map) <- get
        if name /= name'
        then error $ "readRef: wrong type of reference '" ++ name ++ "' was detected."
        else put (name, counter, M.insert ref v map)

unReferenceT :: ReferenceT v m a -> RefEnv v -> m (a, RefEnv v)
unReferenceT (ReferenceT s) = runStateT s
evalReferenceT :: (Applicative m, Monad m) => ReferenceT v m a -> RefEnv v -> m a
evalReferenceT (ReferenceT s) = evalStateT s

mapReferenceT :: (m (a, RefEnv v) -> n (b, RefEnv v)) -> ReferenceT v m a -> ReferenceT v n b
mapReferenceT f m = ReferenceT $ StateT $ f . unReferenceT m

liftCatch :: Catch e m (a, RefEnv v) -> Catch e (ReferenceT v m) a
liftCatch catchE m h =
    ReferenceT $ StateT $ \ s -> unReferenceT m s `catchE` \ e -> unReferenceT (h e) s

--------------------------------------------------------------------------
-- ReferenceT2
--------------------------------------------------------------------------

newtype ReferenceT2 v m1 m2 a = ReferenceT2 (StateT2 (RefEnv v) m1 m2 a)
  deriving (Functor, Applicative, Monad, (MonadState (RefEnv v)), MonadTrans2, MonadIO)

unReferenceT2 :: (Applicative m1, Monad m1, Monad2 m2) => ReferenceT2 v m1 m2 a -> RefEnv v -> m1 (m2 (a, RefEnv v))
unReferenceT2 (ReferenceT2 s) = runStateT2 s
evalReferenceT2 :: (Applicative m1, Monad m1, Monad2 m2) => ReferenceT2 v m1 m2 a -> RefEnv v -> m1 (m2 a)
evalReferenceT2 (ReferenceT2 s) = evalStateT2 s



