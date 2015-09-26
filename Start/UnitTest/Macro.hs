
squareM = #x -> `(,x * ,x)   -- `#` indicates macro-lambda

unittest "squareM" [
    (squareM 3,               9),
    (let a = 3 in squareM a,  9),
    ]

a = "a"
unittest "macro_expand" [
    (macro_expand {squareM 3},       (*) 3 3),
    (macro_expand {(squareM 3) + 1}, (+) ((*) 3 3) 1),  

    (macro_expand {squareM a},                   (*) a a),
    (macro_expand {let a = 1 in squareM a},      (\a.(*) a a) 1), 
    (macro_expand {let a = 1 in squareM (a+1)},  (\a.(*) ((+) a 1) ((+) a 1)) 1), 
    ]

evalN 0 ((squareM 3) + 1)
evalN 1 ((squareM 3) + 1)
evalN 2 ((squareM 3) + 1)
evalN 3 ((squareM 3) + 1)
evalN 4 ((squareM 3) + 1)
-- (+) ((*) 3 3) 1
-- (+) ((3 *) 3) 1
-- (+) 9 1
-- (9 +) 1
-- 10

lazyevalN 0 ((squareM 3) + 1)
lazyevalN 1 ((squareM 3) + 1)
lazyevalN 2 ((squareM 3) + 1)
lazyevalN 3 ((squareM 3) + 1)
lazyevalN 4 ((squareM 3) + 1)
-- (+) ((*) 3 3) 1
-- (+) ((*) 3 3) 1
-- (+) 9 1
-- (9 +) 1
-- 10

