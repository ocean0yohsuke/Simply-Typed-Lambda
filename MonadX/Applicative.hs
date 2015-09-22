{-# LANGUAGE BangPatterns #-}
module MonadX.Applicative (
    module Control.Applicative,

    (|>), (<|),

    (*:),
    (<$|), (|$>), (|*>), (<*|),
    (|!$>),

    (**:), (*-), (-*),
    (<<$|), (|$>>), (|*>>), (<<*|),
    (*>>), (<<*),

    (***:), (**-), (*-*), (-**), (--*), (-*-), (*--),
    (<<<$|), (|$>>>), (|*>>>), (<<<*|),
    (*>>>), (<<<*),

    (****:), 
    (<<<<$|), (|$>>>>), (|*>>>>), (<<<<*|),
    (*>>>>), (<<<<*),

    (*****:), 
    (<<<<<$|), (|$>>>>>), (|*>>>>>), (<<<<<*|),
    (*>>>>>), (<<<<<*),


    (|*), (*|),
    (|**), (**|), (|-*), (|*-), (-*|), (*-|)

    ) where 

import Control.Applicative

----------------------------------------------------------------------
-- Level-0
----------------------------------------------------------------------

infixl 4  |>, <|
(|>) :: (a -> b) -> a -> b
(|>) = ($)
(<|) :: a -> (a -> b) -> b
(<|) = flip (|>)

----------------------------------------------------------------------
-- Level-1
----------------------------------------------------------------------

infixl 5  *:
(*:) :: (Applicative f) => a -> f a
(*:) = pure

infixl 4  |$>, <$|, |*>, <*|
(|$>) :: Functor f => (a -> b) -> f a -> f b
(|$>) = (<$>)
(|*>) :: Applicative f => f (a -> b) -> f a -> f b
(|*>) = (<*>)
(<$|) :: Functor f => f a -> (a -> b) -> f b
(<$|) = flip (|$>)
(<*|) :: Applicative f => f a -> f (a -> b) -> f b
(<*|) = flip (|*>)

infixl 4  |!$>
(|!$>) :: Functor f => (a -> b) -> f a -> f b
(|!$>) !l !r = l <$> r

infixl 4  |*, *|
(|*) :: Applicative f => f (a -> b) -> a -> f b
f |* x = f |*> ((*:) x)
(*|) :: Applicative f => a -> f (a -> b) -> f b
(*|) = flip (|*)

----------------------------------------------------------------------
-- Level-2
----------------------------------------------------------------------

infixl 5  **:
(**:) :: (Applicative f1, Applicative f2) => a -> f1 (f2 a)
(**:) = (*:) . (*:)

infixl 5  -*, *-
(-*) :: (Applicative f1, Applicative f2) => f1 a -> f1 (f2 a)
(-*) = ((*:)|$>) 
(*-) :: (Applicative f1, Applicative f2) => f2 a -> f1 (f2 a)
(*-) = (*:)

infixl 4  |$>>, <<$|, |*>>, <<*|
(|$>>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(|$>>) = (|$>) . (|$>)
(|*>>) :: (Applicative f1, Applicative f2) => f1 (f2 (a -> b)) -> f1 (f2 a) -> f1 (f2 b)
(|*>>) = liftA2 (|*>)
(<<$|) :: (Functor f1, Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
(<<$|) = flip (|$>>)
(<<*|) :: (Applicative f1, Applicative f2) => f1 (f2 a) -> f1 (f2 (a -> b)) -> f1 (f2 b)
(<<*|) = flip (|*>>)

infixl 4  <<*, *>>
(*>>) :: (Applicative f1, Applicative f2) => f1 (f2 a) -> f1 (f2 b) -> f1 (f2 b)
(*>>) = liftA2 (*>)
(<<*) :: (Applicative f1, Applicative f2) => f1 (f2 a) -> f1 (f2 b) -> f1 (f2 a)
(<<*) = liftA2 (<*)

infixl 4  |**, **|
(|**) :: (Applicative f1, Applicative f2) => f1 (f2 (a -> b)) -> a -> f1 (f2 b)
f |** x = f |*>> ((**:) x)
(**|) :: (Applicative f1, Applicative f2) => a -> f1 (f2 (a -> b)) -> f1 (f2 b)
(**|)  = flip (|**)

infixl 4  |-*, |*-, -*|, *-|
(|-*) :: (Applicative f1, Applicative f2) => f1 (f2 (a -> b)) -> f1 a -> f1 (f2 b)
f |-* x = f |*>> ((-*) x)
(|*-) :: (Applicative f1, Applicative f2) => f1 (f2 (a -> b)) -> f2 a -> f1 (f2 b)
f |*- x = f |*>> ((*-) x)
(-*|) :: (Applicative f1, Applicative f2) => f1 a -> f1 (f2 (a -> b)) -> f1 (f2 b)
(-*|) = flip (|-*)
(*-|) :: (Applicative f1, Applicative f2) => f2 a -> f1 (f2 (a -> b)) -> f1 (f2 b)
(*-|) = flip (|*-)

----------------------------------------------------------------------
-- Level-3
----------------------------------------------------------------------

infixl 5  ***:
(***:) :: (Applicative f1, Applicative f2, Applicative f3) => a -> f1 (f2 (f3 a))
(***:) = (*:) . (**:)

infixl 5  -**, *-*, **-, --*, -*-, *--
(-**) :: (Applicative f1, Applicative f2, Applicative f3) => f1 a -> f1 (f2 (f3 a))
(-**) = ((**:)|$>) 
(*-*) :: (Applicative f1, Applicative f2, Applicative f3) => f2 a -> f1 (f2 (f3 a))
(*-*) = (*:) . ((*:)|$>) 
(**-) :: (Applicative f1, Applicative f2, Applicative f3) => f3 a -> f1 (f2 (f3 a))
(**-) = (**:)
(--*) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 a) -> f1 (f2 (f3 a))
(--*) = ((*:)|$>>)
(-*-) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f3 a) -> f1 (f2 (f3 a))
(-*-) = ((*:)|$>)
(*--) :: (Applicative f1, Applicative f2, Applicative f3) => f2 (f3 a) -> f1 (f2 (f3 a))
(*--) = (*:)

infixl 4 |$>>>, <<<$|, |*>>>, <<<*|
(|$>>>) :: (Functor f1, Functor f2, Functor f3) => (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
(|$>>>) = (|$>) . (|$>>)
(|*>>>) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 (a -> b))) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
(|*>>>) = liftA2 (|*>>)
(<<<$|) :: (Functor f1, Functor f2, Functor f3) => f1 (f2 (f3 a)) -> (a -> b) -> f1 (f2 (f3 b))
(<<<$|) = flip (|$>>>)
(<<<*|) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 a)) -> f1 (f2 (f3 (a -> b))) -> f1 (f2 (f3 b))
(<<<*|) = flip (|*>>>)

infixl 4 <<<*, *>>>
(*>>>) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 a)) -> f1 (f2 (f3 b)) -> f1 (f2 (f3 b))
(*>>>) = liftA2 (*>>)
(<<<*) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 a)) -> f1 (f2 (f3 b)) -> f1 (f2 (f3 a))
(<<<*) = liftA2 (<<*)

----------------------------------------------------------------------
-- Level-4
----------------------------------------------------------------------

infixl 5  ****:
(****:) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => a -> f1 (f2 (f3 (f4 a)))
(****:) = (***:) . (*:)

infixl 4 |$>>>>, <<<<$|, |*>>>>, <<<<*|
(|$>>>>) :: (Functor f1, Functor f2, Functor f3, Functor f4) => (a -> b) -> f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 b)))
(|$>>>>) = (|$>) . (|$>>>)
(|*>>>>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 b)))
(|*>>>>) = liftA2 (|*>>>)
(<<<<$|) :: (Functor f1, Functor f2, Functor f3, Functor f4) => f1 (f2 (f3 (f4 a))) -> (a -> b) -> f1 (f2 (f3 (f4 b)))
(<<<<$|) = flip (|$>>>>)
(<<<<*|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f2 (f3 (f4 b)))
(<<<<*|) = flip (|*>>>>)

infixl 4 <<<<*, *>>>>
(*>>>>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 b))) -> f1 (f2 (f3 (f4 b)))
(*>>>>) = liftA2 (*>>>)
(<<<<*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 b))) -> f1 (f2 (f3 (f4 a)))
(<<<<*) = liftA2 (<<<*)

----------------------------------------------------------------------
-- Level-5
----------------------------------------------------------------------

infixl 5  *****:
(*****:) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => a -> f1 (f2 (f3 (f4 (f5 a))))
(*****:) = (*:) . (****:)

infixl 4 |$>>>>>, <<<<<$|, |*>>>>>, <<<<<*|
(|$>>>>>) :: (Functor f1, Functor f2, Functor f3, Functor f4, Functor f5) => (a -> b) -> f1 (f2 (f3 (f4 (f5 a)))) -> f1 (f2 (f3 (f4 (f5 b))))
(|$>>>>>) = (|$>) . (|$>>>>)
(|*>>>>>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 a)))) -> f1 (f2 (f3 (f4 (f5 b))))
(|*>>>>>) = liftA2 (|*>>>>)
(<<<<<$|) :: (Functor f1, Functor f2, Functor f3, Functor f4, Functor f5) => f1 (f2 (f3 (f4 (f5 a)))) -> (a -> b) -> f1 (f2 (f3 (f4 (f5 b))))
(<<<<<$|) = flip (|$>>>>>)
(<<<<<*|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 a)))) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
(<<<<<*|) = flip (|*>>>>>)

infixl 4 <<<<<*, *>>>>>
(*>>>>>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 a)))) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
(*>>>>>) = liftA2 (*>>>>)
(<<<<<*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 a)))) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 a))))
(<<<<<*) = liftA2 (<<<<*)

