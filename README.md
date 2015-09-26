# Simply-Typed-Lambda
My Simply-Typed-Lambda(Hakell like) interpreter written in Haskell

いずれコンパイラにするつもり

## Interpreter.hs
~/Simply-Typed-Lambda> ghci
GHCi, version 7.8.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> :l Interpreter.hs
*Main> run
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
Prelude> :l FileInterpreter.hs
*Main> run
Files in 'Start'.
Input number or command; 'list', 'up' or 're-eval' (abbr. ':l', ':u', ':r', respectively).
- 0. ../
- 1. [Lib]
- 2. [UnitTest]
- 3. test.hs
[Start]: 

