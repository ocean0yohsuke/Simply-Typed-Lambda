
equal :: Int -> Int -> Bool
equal = fix (λeq::Int -> Int -> Bool.
      λm::Int. λn::Int.
        if iszero m then iszero n
        else if iszero n then False
        else eq (pred m) (pred n))
unittest "equal" [
    (equal 3 3, True),
    (equal 3 4, False),
    ]

{-
fix' :: (a -> a) -> a
fix' f = let x = f x in x

equal' :: Int -> Int -> Bool
equal' = fix' (λeq::Int -> Int -> Bool.
      λm::Int. λn::Int.
        if iszero m then iszero n
        else if iszero n then False
        else eq (pred m) (pred n))

(unittest "equal'" [
    (equal' 3 3, True),
    (equal' 3 4, False),
    ])
-}

"---------------------------------"

plus :: Int -> Int -> Int
plus = fix (λp::Int -> Int -> Int.
  λm::Int. λn::Int.
    if iszero m then n else succ (p (pred m) n))
times :: Int -> Int -> Int
times = fix (λt::Int -> Int -> Int.
  λm::Int. λn::Int.
    if iszero m then 0 else plus n (t (pred m) n))
factorial :: Int -> Int
factorial = fix (λf::Int -> Int.
  λm::Int.
    if iszero m then 1 else times m (f (pred m)))

unittest "plus, times" [
    (plus 3 3, 6),
    (plus 3 4, 7),
    (times 3 3, 9),
    (times 3 4, 12),
    ]

-- TODO: memoize が未実装なので遅延評価だととても計算が遅い
unittest "factorial" [
    (factorial 0, 1),
    (factorial 1, 1),
    (factorial 2, 2),
    (factorial 3, 6),
    -- (factorial 4, 24),
    -- (factorial 5, 120),
    -- (factorial 6, 720),
    -- (factorial 10, 120),  -- 正格評価でも遅い

    ]

"-----------------------------"
evalN 0 ((fix (\rec n -> if n == 0 then 1 else n * rec (n-1))) 3)
evalN 1 ((fix (\rec n -> if n == 0 then 1 else n * rec (n-1))) 3)
evalN 2 ((fix (\rec n -> if n == 0 then 1 else n * rec (n-1))) 3)
evalN 3 ((fix (\rec n -> if n == 0 then 1 else n * rec (n-1))) 3)
evalN 4 ((fix (\rec n -> if n == 0 then 1 else n * rec (n-1))) 3)
evalN 5 ((fix (\rec n -> if n == 0 then 1 else n * rec (n-1))) 3)
-- (fix \rec.\n.if (==) n 0 then 1 else (*) n (rec ((-) n 1))) 3
-- (\n.if (==) n 0 then 1 else (*) n ((fix \rec.\n.if (==) n 0 then 1 else (*) n (rec ((-) n 1))) ((-) n 1))) 3
-- if (==) 3 0 then 1 else (*) 3 ((fix \rec.\n.if (==) n 0 then 1 else (*) n (rec ((-) n 1))) ((-) 3 1))
-- if (3 ==) 0 then 1 else (*) 3 ((fix \rec.\n.if (==) n 0 then 1 else (*) n (rec ((-) n 1))) ((-) 3 1))
-- if False then 1 else (*) 3 ((fix \rec.\n.if (==) n 0 then 1 else (*) n (rec ((-) n 1))) ((-) 3 1))
-- (3 *) ((fix \rec.\n.if (==) n 0 then 1 else (*) n (rec ((-) n 1))) ((-) 3 1))


