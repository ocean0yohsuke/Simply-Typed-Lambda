
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

($) :: (a -> b) -> a -> b
($) f x = f x

(>>) :: a -> b -> b
(>>) x y = (\_ -> y) x   -- TODO: not work well in lazy-evaluation.

(>-) :: a -> (a -> b) -> b
(>-) = flip ($)

-- (.)
(<-<) :: (b -> c) -> (a -> b) -> a -> c
(<-<) f g = \x -> f (g x)
(>->) :: (a -> b) -> (b -> c) -> a -> c
(>->) f g = flip (<-<)

