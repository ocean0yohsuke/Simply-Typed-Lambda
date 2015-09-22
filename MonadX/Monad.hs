module MonadX.Monad (
    module Control.Monad,

    (-<), (>-), (>->), (<-<),
    (<<), 
    Monad2(..),
    Monad3(..),
    Monad4(..),
    Monad5(..),
    
    ) where 

import MonadX.Applicative

import Control.Monad
import Control.Category ((>>>), (<<<))

----------------------------------------------------------------------
-- Level-0
----------------------------------------------------------------------

infixl 1 -<
(-<) :: (a -> b) -> a -> b
(-<) = ($)

infixl 1 >-
(>-) :: a -> (a -> b) -> b
(>-) = flip (-<)

infixr 1  <-<
(<-<) :: (b -> c) -> (a -> b) -> a -> c
(<-<) = (<<<)

infixr 1  >->
(>->) :: (a -> b) -> (b -> c) -> a -> c
(>->) = (>>>)

----------------------------------------------------------------------
-- Level-1
----------------------------------------------------------------------

infixr 1  <<
(<<) :: Monad m => m b -> m a -> m b 
(<<) = flip (>>)

----------------------------------------------------------------------
-- Level-2
----------------------------------------------------------------------

infixr 1  >==>
infixl 1  >>--, >>== 
infixl 1  ->==, >-==

class (Applicative m2, Monad m2) => Monad2 m2 where
  (>>==) :: (Applicative m1, Monad m1) => m1 (m2 a) -> (a -> m1 (m2 b)) -> m1 (m2 b)

  (>==>) :: (Applicative m1, Monad m1) => (a -> m1 (m2 b)) -> (b -> m1 (m2 c)) -> a -> m1 (m2 c)
  f >==> g = \x -> f x >>== g
  (>>--) :: (Applicative m1, Monad m1) => m1 (m2 a) -> m1 (m2 b) -> m1 (m2 b)
  m >>-- k = m >>== \_ -> k

  (>-==) :: (Applicative m1, Monad m1) => m1 a -> (a -> m1 (m2 b)) -> m1 (m2 b)
  m >-== k = (-*) m >>== k
  (->==) :: (Applicative m1, Monad m1) => m2 a -> (a -> m1 (m2 b)) -> m1 (m2 b)
  m ->== k = (*-) m >>== k

instance Monad2 Maybe where
    mmv >>== f = 
        mmv >>= \mv ->
        case mv of 
            Nothing -> (*:) Nothing
            Just a  -> f a

instance Monad2 [] where
    mmv >>== f = 
        mmv >>= \xs -> 
        foldr (\x acc -> f x <$|(++)|*> acc) ((*:) []) xs

-- map f xs = foldr (\x acc -> (f x) : acc) [] xs

instance Monad2 (Either e) where
	mmv >>== f = 
		mmv >>= \mv -> 
		case mv of
			Left l  -> (*:) (Left l)
			Right r -> f r

----------------------------------------------------------------------
-- Level-3
----------------------------------------------------------------------

infixr 1  >===>
infixl 1  >>>--, >>>==
infixl 1  >--==, ->-==, -->==, >>-==, >->==, ->>==

class (Applicative m3, Monad m3) => Monad3 m3 where
  (>>>==) :: (Applicative m1, Monad m1, Monad2 m2) => m1 (m2 (m3 a)) -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))

  (>===>) :: (Applicative m1, Monad m1, Monad2 m2) => (a -> m1 (m2 (m3 b))) -> (b -> m1 (m2 (m3 c))) -> a -> m1 (m2 (m3 c))
  f >===> g = \x -> f x >>>== g
  (>>>--) :: (Applicative m1, Monad m1, Monad2 m2) => m1 (m2 (m3 a)) -> m1 (m2 (m3 b)) -> m1 (m2 (m3 b))
  m >>>-- k = m >>>== \_ -> k

  (>--==) :: (Applicative m1, Monad m1, Monad2 m2) => m1 a -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
  m >--== k = (-**) m >>>== k
  (->-==) :: (Applicative m1, Monad m1, Monad2 m2) => m2 a -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
  m ->-== k = (*-*) m >>>== k
  (-->==) :: (Applicative m1, Monad m1, Monad2 m2) => m3 a -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
  m -->== k = (**-) m >>>== k
  (>>-==) :: (Applicative m1, Monad m1, Monad2 m2) => m1 (m2 a) -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
  m >>-== k = (--*) m >>>== k
  (->>==) :: (Applicative m1, Monad m1, Monad2 m2) => m2 (m3 a) -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
  m ->>== k = (*--) m >>>== k
  (>->==) :: (Applicative m1, Monad m1, Monad2 m2) => m1 (m3 a) -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
  m >->== k = (-*-) m >>>== k

instance Monad3 Maybe where
    mmmv >>>== f = 
        mmmv >>== \mv ->
        case mv of 
            Nothing -> (**:) Nothing
            Just a  -> f a

instance Monad3 [] where
    mmmv >>>== f = 
        mmmv >>== \xs -> 
        foldr (\x acc -> f x <<$|(++)|*>> acc) ((**:) []) xs 

instance Monad3 (Either e) where
	mmmv >>>== f = 
		mmmv >>== \mv -> 
		case mv of
			Left l  -> (**:) (Left l)
			Right r -> f r

----------------------------------------------------------------------
-- Level-4
----------------------------------------------------------------------

infixr 1  >====>
infixl 1  >>>>--, >>>>==

class (Applicative m4, Monad m4) => Monad4 m4 where
    (>>>>==) :: (Applicative m1, Monad m1, Monad2 m2, Monad3 m3) => m1 (m2 (m3 (m4 a))) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))

    (>====>) :: (Applicative m1, Monad m1, Monad2 m2, Monad3 m3) => (a -> m1 (m2 (m3 (m4 b)))) -> (b -> m1 (m2 (m3 (m4 c)))) -> a -> m1 (m2 (m3 (m4 c)))
    f >====> g = \x -> f x >>>>== g
    (>>>>--) :: (Applicative m1, Monad m1, Monad2 m2, Monad3 m3) => m1 (m2 (m3 (m4 a))) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
    m >>>>-- k = m >>>>== \_ -> k

instance Monad4 Maybe where
    mmmmv >>>>== f = 
        mmmmv >>>== \mv ->
        case mv of 
            Nothing -> (***:) Nothing
            Just a  -> f a

instance Monad4 [] where
    mmmmv >>>>== f = 
        mmmmv >>>== \xs -> 
        foldr (\x acc -> f x <<<$|(++)|*>>> acc) ((***:) []) xs 

instance Monad4 (Either e) where
	mmmmv >>>>== f = 
		mmmmv >>>== \mv -> 
		case mv of
			Left l  -> (***:) (Left l)
			Right r -> f r

----------------------------------------------------------------------
-- Level-5
----------------------------------------------------------------------

infixr 1  >=====>
infixl 1  >>>>>--, >>>>>== 

class (Applicative m5, Monad m5) => Monad5 m5 where
    (>>>>>==) :: (Applicative m1, Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 (m2 (m3 (m4 (m5 a)))) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))

    (>=====>) :: (Applicative m1, Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => (a -> m1 (m2 (m3 (m4 (m5 b))))) -> (b -> m1 (m2 (m3 (m4 (m5 c))))) -> a -> m1 (m2 (m3 (m4 (m5 c))))
    f >=====> g = \x -> f x >>>>>== g
    (>>>>>--) :: (Applicative m1, Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 (m2 (m3 (m4 (m5 a)))) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
    m >>>>>-- k = m >>>>>== \_ -> k

instance Monad5 Maybe where
    mmmmmv >>>>>== f = 
        mmmmmv >>>>== \mv ->
        case mv of 
            Nothing -> (****:) Nothing
            Just a  -> f a

instance Monad5 [] where
    mmmmmv >>>>>== f = 
        mmmmmv >>>>== \xs -> 
        foldr (\x acc -> (f x) <<<<$|(++)|*>>>> acc) ((****:) []) xs 

instance Monad5 (Either e) where
	mmmmmv >>>>>== f = 
		mmmmmv >>>>== \mv -> 
		case mv of
			Left l  -> (****:) (Left l)
			Right r -> f r

