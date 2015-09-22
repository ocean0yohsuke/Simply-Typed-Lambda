import "Lib/Prelude.hs"

unittest "evaluation order" [
    (let a = 1
     in let f = \x -> 
            let b = a + x
            in let a = 5
               in a + b
        in f 10
    , 16),

    (let a = 1
     in let f = \x -> 
            let a = 5
            in let b = a + x
            in a + b
        in f 10
     , 20)
    ]

unless :: Bool -> a -> a -> a
unless cond x y = if cond
                  then y
                  else x
factorial :: Int -> Int
factorial n = unless (n == 1) (n * (factorial (n - 1))) 1

unittest "lazy-evaluation: unless" [
    -- run forever in applicative-order
    -- stop in lazy-evaluation
    (factorial 5, 120),
    ]

-- infinite list
inflist :: a -> [a]
inflist n = n : (inflist (n + 1))

-- natural number
nn :: [Int]
nn = inflist 1

unittest "lazy-evaluation: infinite list" [
    (head nn,        1),
    (nn !! 0,        1),
    (nn !! 1,        2),
    (nn !! 2,        3),
    (nn !! 10,      11),
    
    (head (filter even nn),  2),
    ((filter even nn) !! 10, 22),  -- TODO: memoize が未実装なのでとても計算が遅い
    ]







