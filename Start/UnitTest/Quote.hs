a :: Int
a = 123

unittest "quote" [
    (show {a},         "a"),
    (show {,a},        ",a"),  
    (show {pred ,a},   "pred ,a"),  
    ]

unittest "quasiquote" [
    (`a,                       a),  
    (`(,a),                    123),  
    (`(pred ,a),               pred 123),  
    (let x = `(pred ,a) in x , 122),
    ]

-- quine
q :: _ 
q = \x. `(,x {,x})
-- godel
g :: _
g = \x -> `(isunprovable (valueof (,x {,x})))

unittest "quine & godel" [
    (q {x},   x {x}),
    (q {q},   q {q}),
    (g {x},   isunprovable (valueof (x {x}))),
    (g {g},   isunprovable (valueof (g {g}))),
    ]
{--}


