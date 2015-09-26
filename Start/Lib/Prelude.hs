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
foldl :: (b12 -> a12 -> b12) -> b12 -> [a12] -> b12
foldl f a []     = a
foldl f a (x:xs) = foldl f (f a x) xs

foldr :: (a13 -> b13 -> b13) -> b13 -> [a13] -> b13
foldr f a []     = a
foldr f a (x:xs) = f x (foldr f a xs)

--
-- list
--
append :: [a14] -> [a14] -> [a14]
append xs ys = foldr (:) ys xs
(++) :: [a15] -> [a15] -> [a15]
(++) = append

length :: [a16] -> Int
length xs = foldl (\a x -> a + 1) 0 xs

reverse :: [a17] -> [a17]
reverse xs = foldl (\a x -> x : a) [] xs

(!!) :: [a6] -> Int -> a6
(!!) (x:xs) 0 = x
(!!) (x:xs) k = xs !! (k - 1)

elem :: a7 -> [a7] -> Bool
elem t []     = False
elem t (x:xs) = if t == x
                then True
                else elem t xs

--
-- higher order 
--
map :: (a8 -> b8) -> [a8] -> [b8]
map f []     = []
map f (x:xs) = f x : map f xs
(|$>) :: (a8 -> b8) -> [a8] -> [b8]
(|$>) = map

filter :: (a9 -> Bool) -> [a9] -> [a9]
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

any :: (a10 -> Bool) -> [a10] -> Bool
any f xs = or (map f xs)

all :: (a11 -> Bool) -> [a11] -> Bool
all f xs = and (map f xs)

