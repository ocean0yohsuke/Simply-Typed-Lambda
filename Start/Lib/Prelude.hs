(import "Lib/Micro.hs")

not :: Bool -> Bool
not True = False
not False = True 

--
-- arithmetic
--
even :: Int -> Bool
even x = iszero (mod x 2)

odd :: Int -> Bool
odd x = not (even x)

abs :: Int -> Int
abs x = if x < 0
        then x * (-1)
        else x

max :: Int -> Int -> Int
max a b = if a > b then a else b
min :: Int -> Int -> Int
min a b = if a > b then b else a

-- great common diviser
gcd :: Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b (mod a b)

-- least common deviser
lcm :: Int -> Int -> Int
lcm a b = (a * b) / (gcd a b)

--
-- fold
--
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f a []     = a
foldl f a (x:xs) = foldl f (f a x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f a []     = a
foldr f a (x:xs) = f x (foldr f a xs)

--
-- list
--
append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs
(++) :: [a] -> [a] -> [a]
(++) = append

length :: [a] -> Int
length xs = foldl (\a x -> a + 1) 0 xs

reverse :: [a] -> [a]
reverse xs = foldl (\a x -> x : a) [] xs

(!!) :: [a] -> Int -> a
(!!) (x:xs) 0 = x
(!!) (x:xs) k = xs !! (k - 1)

elem :: a -> [a] -> Bool
elem t []     = False
elem t (x:xs) = if t == x
                then True
                else elem t xs

--
-- higher order 
--
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs
(|$>) :: (a -> b) -> [a] -> [b]
(|$>) = map

filter :: (a -> Bool) -> [a] -> [a]
filter f []     = []
filter f (x:xs) = if f x
                  then x : filter f xs
                  else filter f xs
--
-- boolean
--
(||) :: Bool -> Bool -> Bool
(||) False False = False
(||) _     _     = True

(&&) :: Bool -> Bool -> Bool
(&&) True True = True
(&&) _     _   = False

or :: [Bool] -> Bool
or xs = foldl (||) False xs

and :: [Bool] -> Bool
and xs = foldl (&&) False xs

any :: (a -> Bool) -> [a] -> Bool
any f xs = or (map f xs)

all :: (a -> Bool) -> [a] -> Bool
all f xs = and (map f xs)

