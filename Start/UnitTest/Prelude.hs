import "Lib/Prelude.hs"

unittest "not, null, iszero, even, odd" [
    (not True,  False),
    (not False, True), 
    (null [],   True),
    (null [1],  False), 
    (iszero 0,  True), 
    (iszero 1,  False), 
    (even 1,    False), 
    (even 2,    True), 
    (even 0,    True), 
    (odd 0,     False),
    (odd 101,   True), 
    ]

unittest "abs, max, min, gcd, lcm" [
    (abs -10,    10), 
    (abs (-10),  10), 
    (abs 10,     10),
    (max 1 2,  2),
    (min 1 2,  1),

    (gcd 7 5,   1),
    (gcd 24 12, 12),
    (gcd 12 128, 4),
    (gcd 24 (gcd 12 128), 4),
    (lcm 7 5, 35),
    (lcm (lcm 7 5) 9, 315),

    ]

unittest "fold-left, fold-right" [
    (show (foldl (+) 0 [1,2,3,4,5,6,7,8,9,10]),              "55"),
    (show (foldl (\a x -> x : a) [] [1,2,3,4,5,6,7,8,9,10]), "[10,9,8,7,6,5,4,3,2,1]"),

    (show (foldr (+) 0 [1,2,3,4,5,6,7,8,9,10]),              "55"),
    (show (foldr (:) [] [1,2,3,4,5,6,7,8,9,10]),             "[1,2,3,4,5,6,7,8,9,10]"),
    ]

unittest "append(++), length, reverse" [
    (append "abc" "def",            "abcdef"),
    (show (append [1,2,3] [4,5,6]), "[1,2,3,4,5,6]"),

    (show (length []),                    "0"),
    (show (length ['a','b','c','d','e']), "5"),

    (show (reverse []),             "[]"),
    (show (reverse [1,2,3,4,5]),    "[5,4,3,2,1]"),

    ]

unittest "!!, elem" [
    (show ([0,1,2,3,4,5] !! 0), "0"),
    (show ([0,1,2,3,4,5] !! 1), "1"),
    (show ([0,1,2,3,4,5] !! 5), "5"),
    (show ("abcdef" !! 0),      "'a'"),

    (show (elem 3 [0,1,2,3,4,5]), "True"),
    (show (elem 9 [0,1,2,3,4,5]), "False"),
    (show (elem 'a' "Hello"), "False"),
    (show (elem 'H' "Hello"), "True"),
    ]

unittest "map, filter" [
    (show (map (\x -> x * x) [1,2,3,4,5]), "[1,4,9,16,25]"),
    (show (map ((*) 2) [1,2,3,4,5]),       "[2,4,6,8,10]"),
    (show (map (2 *) [1,2,3,4,5]),         "[2,4,6,8,10]"),
    (show (map (++"!") ["a","b","c"]),    "[\"a!\",\"b!\",\"c!\"]"),
    (show (map ("!"++) ["a","b","c"]),    "[\"!a\",\"!b\",\"!c\"]"),

    (show (filter even [1,2,3,4,5,6,7,8]), "[2,4,6,8]"),
    (show (filter odd [1,2,3,4,5,6,7,8]),  "[1,3,5,7]"),
    ]

unittest "(||), (&&), or, and, any, all" [
    (True || False,     True),
    (True && False,     False),

    (or [True, False, True],   True),
    (and [True, False, True],  False),

    (any even [1,2,3],   True),
    (all even [1,2,3],   False),
    ]

unittest "let" [
    (let a = 10 in let b = 20 in 
     let a = 0  in let b = 1  in [a,b]    , [0,1]),

    (let a = 1 in
     let b = a + 1 in [a,b]               , [1,2]),
    (let a = 1 in
     let b = a + 1 in               
     let c = b * 2 in [a,b,c]             , [1,2,4]),

    ]

unittest "fix" [
    (let factorial = fix (\f m -> if iszero m then 1 else m * (f (m - 1)))
     in  factorial 10,  3628800)
    ]

unittest "letrec" [
    (letrec factorial::Int->Int = \m -> if iszero m then 1 else m * (factorial (m - 1)) 
     in factorial 10,   3628800)
    ]
{--}


