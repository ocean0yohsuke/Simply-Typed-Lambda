# Simply-Typed-Lambda
My Simply-Typed-Lambda(Hakell-like) interpreter written in Haskell

いずれコンパイラにするつもり

    .../Simply-Typed-Lambda$ stack build

## Interpreter.hs

    .../Simply-Typed-Lambda$ stack runghc app/Interpreter.hs
    >
    1 + 1
    => 2
    (\x -> x + 1) 3
    => 4
    pred 3
    => 2
    let (x,y) = (1,2) in x
    => 1
    if True then 1 else 2
    => 1
    let factorial = fix (λ f m -> if m == 0 then 1 else m * (f (m - 1))) in factorial 5
    => 120
    letrec eq::Int->Int->Bool = (λm n. if m == 0 then n == 0 else if n == 0 then False else eq (m - 1) (n - 1)) in eq 3 3
    => True
    letrec makeInfiniteList::Int->[Int] = (\n -> n : (makeInfiniteList (n + 1))) in head (makeInfiniteList 1)
    => 1

    typeof (+)
    Int -> Int -> Int

    typeof (\x -> x + 1)
    Int -> Int

    typeof (λx::a->a. x 3)
    (Int -> Int) -> Int

    typeof (λx. x 3)
    (Int -> a23) -> a23

## FileInterpreter.hs

    .../Simply-Typed-Lambda$ stack runghc app/FileInterpreter.hs
    Files in 'Start'.
    Input number or command; 'list', 'up' or 're-eval' (abbr. ':l', ':u', ':r', respectively).
    - 0. ../
    - 1. [Lib]
    - 2. [UnitTest]
    [Start]: 2
    Files in 'Start/UnitTest'.
    Input number or command; 'list', 'up' or 're-eval' (abbr. ':l', ':u', ':r', respectively).
    - 0. ../
    - 1. EvaluationStrategy.hs
    - 2. FixedPoint.hs
    - 3. Macro.hs
    - 4. Prelude.hs
    - 5. Quote.hs
    [Start/UnitTest]: 1
    [ begin: 1. Start/UnitTest/EvaluationStrategy.hs ]
    unittest ["evaluation order"] - Cases: 2  Tried: 2  Errors: 0  Failures: 0
    unittest ["lazy-evaluation: unless"] - Cases: 1  Tried: 1  Errors: 0  Failures: 0
    unittest ["lazy-evaluation: infinite list"] - Cases: 7  Tried: 7  Errors: 0  Failures: 0
    [ end: 1. Start/UnitTest/EvaluationStrategy.hs ]
    [Start/UnitTest]: 


