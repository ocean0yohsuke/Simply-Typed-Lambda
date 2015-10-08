module Main where 
import Test.HUnit

import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.Arrow
import Lambda.Parser
import Lambda.Compiler
import Lambda.Evaluator

import qualified Lambda.DataType.Type as Ty
import qualified Lambda.DataType.Shadow.Expr_ as E
import qualified Lambda.DataType.Shadow.SExpr_ as SE
import qualified Lambda.DataType.Shadow.PatternMatch_ as PM
import Lambda.DataType 

import Data.List (intersperse)
import Text.ParserCombinators.Parsec
import Debug.Trace

import System.IO.Unsafe (unsafePerformIO)  -- TODO: 

main :: IO ()
main = run >> return ()

-- | Run Unit-Test of this module. 
run = runTestTT $ TestList 
        [
          TestList testCase_Parser_Lexeme
        , TestList testCase_Parser_Expr

        , TestList testCase_desugar

        , TestList testCase_toTerm
        , TestList testCase_restore
        , TestList testCase_typeof

        , TestList testCase_eval

        ]

-- *UnitTest> run

----------------------------------------------------------------
-- template
----------------------------------------------------------------

-- read & show
rsLexeme :: (SExpr -> a) -> String -> a
rsLexeme func str = 
  case readSLexeme str of
    Left err        -> error $ str ++"\n"++ show err ++"\n"
    Right (s, [])   -> func s
    Right (s, rest) -> error $ str ++"\n" 
                                ++"*** parser failed to exhaust whole string:\n" 
                                ++"*** parsed: "++ show s ++"\n" 
                                ++"*** rest: " ++ rest ++"\n"
rsExpr :: (SExpr -> a) -> String -> a
rsExpr func str = 
  case readSExpr (str++"\n") of
    Left err        -> error $ str ++"\n"++ show err ++"\n"
    Right (s, [])   -> func s
    Right (s, rest) -> error $ str ++"\n" 
                                ++"*** parser failed to exhaust whole string:\n" 
                                ++"*** parsed: "++ show s ++"\n" 
                                ++"*** rest: " ++ rest ++"\n"

----------------------------------------------------------------
-- 
----------------------------------------------------------------

testCase_Parser_Lexeme = (\t -> "Parser" ~: "Lexeme" ~: t) |$>
    [ 
      t "x" ~?= "x"
    , t "x'" ~?= "x'"
    , t "x1" ~?= "x1"
    , t "3" ~?= "3"
    , t "'a'" ~?= "'a'"
    , t "(x)" ~?= "x"
    , t "[a,b,c]" ~?= "[a,b,c]"
    , t "(a,b,c)" ~?= "(a, b, c)"
    , t "[a,[b1,b2],c]" ~?= "[a,[b1,b2],c]"
    , t "(a,(b1,b2),c)" ~?= "(a, (b1, b2), c)"

    , t'' "[]" ~?= "NIL"

    , t' "(+)" ~?= SE.VAR "+"
    , t  "(+)" ~?= "(+)"
    , t "(+) 3 1" ~?= "((+) 3 1)"
    , t "(+) 1" ~?= "((+) 1)"
    , t' "(+) 1" ~?= SE.APP (SE.VAR "+") [SE.INT 1]

    , t "1 + 2" ~?= "(1 + 2)"
    , t' "1 + 2" ~?= SE.APPSeq [SE.INT 1,SE.OPR "+",SE.INT 2]
    , t "1 +" ~?= "(1 +)"
    , t "1+" ~?= "(1 +)"
    , t "+1" ~?= "(+ 1)"

    , t "x 1" ~?= "(x 1)"
    , t "x y" ~?= "(x y)"
    , t "(x y)" ~?= "(x y)"
    , t  "(x y) z" ~?= "((x y) z)"
    , t' "(x y) z" ~?= SE.APP (SE.APP (SE.VAR "x") [SE.VAR "y"]) [SE.VAR "z"]
    , t  "x (y z)" ~?= "(x (y z))"
    , t' "x (y z)" ~?= SE.APP (SE.VAR "x") [SE.APP (SE.VAR "y") [SE.VAR "z"]]
    , t  "x y z"   ~?= "(x y z)"
    , t' "x y z"   ~?= SE.APP (SE.VAR "x") [SE.VAR "y",SE.VAR "z"]
    , t  "x (y z) v" ~?= "(x (y z) v)"
    , t' "x (y z) v" ~?= SE.APP (SE.VAR "x") [ SE.APP (SE.VAR "y") [SE.VAR "z"]
                                             , SE.VAR "v"]

    , t "λx::A.x"      ~?= "(\\x::A.x)"
    , t "(λx::A.x)"    ~?= "(\\x::A.x)"
    , t "λx::A->A.x"   ~?= "(\\x::(A -> A).x)"
    , t "λx::(A->A).x" ~?= "(\\x::(A -> A).x)"
    , t "λx::(A->Bool->Int).x" ~?= "(\\x::(A -> Bool -> Int).x)"
    , t "λx::((A->Bool)->Int).x" ~?= "(\\x::((A -> Bool) -> Int).x)"
    , t "λx::(A->(Bool->Int)).x" ~?= "(\\x::(A -> Bool -> Int).x)"
    , t "λ_.x"    ~?= "(\\_.x)"
    , t "(λ_ -> y) x" ~?= "((\\_.y) x)"
    , t "(λm::Int->Int.m 3)" ~?= "(\\m::(Int -> Int).(m 3))"
    , t "(λm::Int->Int.m 3) (λ_.1)" ~?= "((\\m::(Int -> Int).(m 3)) (\\_.1))"

    , t "λx y. x"         ~?= "(\\x y.x)"
    , t'' "λx y. x"         ~?= "LAM [(VAR \"x\",_),(VAR \"y\",_)] (VAR \"x\")"
    , t "λx::A.λy::A.x"     ~?= "(\\x::A.(\\y::A.x))"
    , t "λx::A y::A. x"     ~?= "(\\x::A y::A.x)"
    , t "λx::A.(λy::A.x)"   ~?= "(\\x::A.(\\y::A.x))"
    , t "λx::A.λy::A.x y z" ~?= "(\\x::A.(\\y::A.(x y z)))"
    , t "(λx::A.λy::A.x) u v"       ~?= "((\\x::A.(\\y::A.x)) u v)"
    , t "λu::A.λv::A.(λx::A.λy::A.x) u v" ~?= "(\\u::A.(\\v::A.((\\x::A.(\\y::A.x)) u v)))"
    , t "(λx::A. λy::A. z x (λu::A. u x)) (λx::A. w x)" ~?= "((\\x::A.(\\y::A.(z x (\\u::A.(u x))))) (\\x::A.(w x)))"
    , t "λx::(A -> A -> A).x"  ~?= "(\\x::(A -> A -> A).x)"
    , t "λx::(A -> Int -> Bool).x"   ~?= "(\\x::(A -> Int -> Bool).x)"
    , t "λx::(A -> (Int -> Bool)).x" ~?= "(\\x::(A -> Int -> Bool).x)"
    , t "λx::((A -> Int) -> Bool).x" ~?= "(\\x::((A -> Int) -> Bool).x)"
    , t "λ(x::Int) -> x" ~?= "(\\x::Int.x)"
    , t "λx -> x"        ~?= "(\\x.x)"
    , t "λ(x, y) -> x"   ~?= "(\\(x, y).x)"
    , t "λ(x, y). x"     ~?= "(\\(x, y).x)"
    , t "λ(Tag 1). x"    ~?= "(\\Tag 1.x)"
    , t "λ(Tag _). x"    ~?= "(\\Tag _.x)"
    , t "λx::(Int, Int). x"   ~?= "(\\x::(Int, Int).x)"
    , t "λ(x::(Int, Int)). x" ~?= "(\\x::(Int, Int).x)"
    , t "λx::[Int]. x"        ~?= "(\\x::[Int].x)"
    , t "λm::a->a. m 3"       ~?= "(\\m::(a -> a).(m 3))"

    , t "True"  ~?= "True"
    , t "123"  ~?= "123"
    , t "\"hello\""  ~?= "\"hello\""
    , t "if True then 1 else 2"  ~?= "if True then 1 else 2"
    , t "if (iszero 3) then 1 else 2"  ~?= "if (iszero 3) then 1 else 2"
    , t "if iszero 3 then 1 else 2"  ~?= "if (iszero 3) then 1 else 2"
    , t "if (1 > 0) then 1 else 2"  ~?= "if (1 > 0) then 1 else 2"
    , t "if 1 > 0 then 1 else 2"  ~?= "if (1 > 0) then 1 else 2"

    , t "let x = 3 in x" ~?= "let x = 3 in x"
    , t "let x = a in x" ~?= "let x = a in x"

    , t "(1, a)" ~?= "(1, a)"
    , t "(1, a, True)" ~?= "(1, a, True)"
    , t "((1, a), True)" ~?= "((1, a), True)"
    , t "(1, (a, True))" ~?= "(1, (a, True))"

    , t "[]" ~?= "[]"
    , t "[a]" ~?= "[a]"
    , t "[1, 2, 3]" ~?= "[1,2,3]"


    , t "fix (λeq::(Int->Int->Bool).λm::Int.λn::Int. if iszero m then iszero n else if iszero n then False else eq (pred m) (pred n))"
    ~?= "(fix (\\eq::(Int -> Int -> Bool).(\\m::Int.(\\n::Int.if (iszero m) then (iszero n) else if (iszero n) then False else (eq (pred m) (pred n))))))"
    , t "letrec eq::Int->Int->Bool = (λm::Int.λn::Int. if iszero m then iszero n else if iszero n then False else eq (pred m) (pred n)) in eq 3 3"
    ~?= "letrec eq::Int -> Int -> Bool = (\\m::Int.(\\n::Int.if (iszero m) then (iszero n) else if (iszero n) then False else (eq (pred m) (pred n)))) in (eq 3 3)"

    , t "(x,y)"  ~?= "(x, y)"
    , t "let (x,y) = (1,2) in x"         ~?= "let (x, y) = (1, 2) in x"
    , t "let ((x,y),z) = ((1,2),3) in y" ~?= "let ((x, y), z) = ((1, 2), 3) in y"
    , t "let (x,_) = (1,2) in x"         ~?= "let (x, _) = (1, 2) in x"

    , t' "True"  ~?= SE.BOOL True

    , t "case 1 of 1 -> 2"   ~?= "case 1 of 1 -> 2"
    , t "case x of Tag1 1 -> 2"   ~?= "case x of Tag1 1 -> 2"
    , t "case x of Tag1 1 -> 2\n Tag2 2 -> 3"   ~?= "case x of Tag1 1 -> 2 | Tag2 2 -> 3"
    , t "case X 1 of X 1 -> 2"   ~?= "case (X 1) of X 1 -> 2"

    , t "let x = Apple in case x of Apple -> 1\n Orange -> 2"  ~?= "let x = Apple in case x of Apple -> 1 | Orange -> 2"
    , t "let x = Apple in case x of Apple -> pred 3\n Orange -> 3" ~?= "let x = Apple in case x of Apple -> (pred 3) | Orange -> 3"
    , t "let a = 1 in let b = a + 1 in let c = b * 2 in [a,b,c]" ~?= "let a = 1 in let b = (a + 1) in let c = (b * 2) in [a,b,c]"

    , t "#x -> x" ~?= "(#x.x)"

    , t "1 + 1"   ~?= "(1 + 1)"  
    , t "1+1"     ~?= "(1 + 1)"  
    , t "1+2*3/4" ~?= "(1 + 2 * 3 / 4)"  
    , t "1+"      ~?= "(1 +)"  
    , t'' "1+"      ~?= "APPSeq [INT 1,OPR \"+\"]"  
    , t "+1"      ~?= "(+ 1)"  
    , t "+1*"     ~?= "(+ 1 *)" 
    , t "-1"      ~?= "-1"
    , t "-1+"     ~?= "(-1 +)"
    , t "0-1+"    ~?= "(0 - 1 +)"
    , t "1+2*"    ~?= "(1 + 2 *)"  
    , t "+1*2"    ~?= "(+ 1 * 2)"  
    , t "+1*2-"   ~?= "(+ 1 * 2 -)"

    , t "λ(x:xs) -> x"     ~?= "(\\(x:xs).x)"
    , t "λ(x:[]) -> x"     ~?= "(\\(x:[]).x)"
    , t "λ(x1:x2:xs) -> x" ~?= "(\\(x1:(x2:xs)).x)"

    , t "x : y" ~?= "(x : y)"


    , t "a * b" ~?= "(a * b)"
    , t'' "a * b" ~?= "APPSeq [VAR \"a\",OPR \"*\",VAR \"b\"]"
    , t "(a * b) + 1" ~?= "((a * b) + 1)"
    , t'' "(a * b) + 1" ~?= "APPSeq [APPSeq [VAR \"a\",OPR \"*\",VAR \"b\"],OPR \"+\",INT 1]"

    , t "\"abc\"" ~?= "\"abc\""
    , t "\"_\\\"_\"" ~?= "\"_\\\"_\""

    , t "`(a b)" ~?= "`(a b)"
    , t'' "`(a b)" ~?= "QQUT (APP (VAR \"a\") [VAR \"b\"])"
    , t ",a" ~?= ",a"
    , t "`(,a b)" ~?= "`(,a b)"
    , t "`(a ,b)" ~?= "`(a ,b)"
    , t'' "`(a ,b)" ~?= "QQUT (APP (VAR \"a\") [UNQUT (VAR \"b\")])"
    , t "(a,b)" ~?= "(a, b)"
    , t'' "(a,b)" ~?= "TUPLE [VAR \"a\",VAR \"b\"]"
    , t "(a ,b)" ~?= "(a ,b)"
    , t'' "(a ,b)" ~?= "APP (VAR \"a\") [UNQUT (VAR \"b\")]"
    , t "(a (,b))" ~?= "(a ,b)"
    , t "{a b}" ~?= "{(a b)}"
    , t "`(,x {,x})" ~?= "`(,x {,x})"

    ]
  where
    t   = rsLexeme show
    t'  = rsLexeme (convert :: SExpr -> SExpr_)
    t'' = rsLexeme ((convert :: SExpr -> SExpr_) >-> show)

testCase_Parser_Expr = (\t -> "Parser" ~: "Expr" ~: t) |$>
    [ 
     t "func = pred"     ~?= "func = pred"
    , t "func = λx::Int.x" ~?= "func = (\\x::Int.x)"

    , t "f (x:xs) = 1" ~?= "f (x:xs) = 1"
    , t "f :: [a] -> Int -> a" ~?= "f :: [a] -> Int -> a"
    , t "(!!) :: [a] -> Int -> a" ~?= "(!!) :: [a] -> Int -> a"
    , t "(!!) (x:xs) 0 = x" ~?= "(!!) (x:xs) 0 = x"
    , t "map f (x:xs) = f x : map f xs" ~?= "map f (x:xs) = ((f x) : (map f xs))"
    , t "func :: Int -> Int -> Bool" ~?= "func :: Int -> Int -> Bool"

    , t "gcd a 0 = a\ngcd a b = gcd b (mod a b)"  ~?= "gcd a 0 = a\ngcd a b = (gcd b (mod a b))"

    , t "data Test = Tag1 Bool"   ~?= 
        "data Test = Tag1 Bool"
    , t "data Test = Tag1 Bool | Tag2 Int Char" ~?= 
        "data Test = Tag1 Bool | Tag2 Int Char"
    , t "data Test = Tag1 Bool | Tag2 Int Char | Tag3 (Int, Bool)" ~?= 
        "data Test = Tag1 Bool | Tag2 Int Char | Tag3 (Int, Bool)"
    , t'' "data Fruits = Apple | Orange" ~?= "BNF \"Fruits\" [(\"Apple\",[]),(\"Orange\",[])]"

    , t "(>>) x y = (λ_ -> y) x"  ~?= "(>>) x y = ((\\_.y) x)"
    ]
  where
    t   = rsExpr show
    t'  = rsExpr (convert :: SExpr -> SExpr_)
    t'' = rsExpr ((convert :: SExpr -> SExpr_) >-> show)

testCase_desugar = (\t -> "desugar" ~: "" ~: t) |$>
    [ t "x" ~?= "x"

    , t "x y z"    ~?= "x y z"
    , t "(x y) z" ~?= "x y z"
    , t "x (y z)" ~?= "x (y z)"
    , t "x (y z) v" ~?= "x (y z) v"


    , t "3 + 1" ~?= "(3 + 1)"
    , t "1 +" ~?= "(1 +)"
    , t "1+" ~?= "(1 +)"
    , t "+1" ~?= "(+ 1)"

    , t "λx::A.x"      ~?= "\\x::A.x"
    , t "(λx::A.x)"    ~?= "\\x::A.x"
    , t "λx::A->A.x"   ~?= "\\x::(A -> A).x"
    , t "λx::(A->A).x" ~?= "\\x::(A -> A).x"
    , t "λx::(A->Bool->Int).x" ~?= "\\x::(A -> Bool -> Int).x"
    , t "λx::((A->Bool)->Int).x" ~?= "\\x::((A -> Bool) -> Int).x"
    , t "λx::(A->(Bool->Int)).x" ~?= "\\x::(A -> Bool -> Int).x"
    , t "λ_.x"    ~?= "\\_.x"
    , t "(λm::Int->Int.m 3)" ~?= "\\m::(Int -> Int).m 3"

    , t "(λx y. x)"   ~?= "\\x.\\y.x"
    , t "λx::A.(λy::A.x)"   ~?= "\\x::A.\\y::A.x"
    , t "λx::A.λy::A.x"     ~?= "\\x::A.\\y::A.x"
    , t "λx::A.λy::A.x y z" ~?= "\\x::A.\\y::A.x y z"
    , t "(λx::A.λy::A.x) u v"       ~?= "(\\x::A.\\y::A.x) u v"
    , t "λu::A.λv::A.(λx::A.λy::A.x) u v" ~?= "\\u::A.\\v::A.(\\x::A.\\y::A.x) u v"

    , t "(λa -> (λ(x,y) -> x) a)"  ~?= "\\a.(\\x.(\\y.x) a.2) a.1"
    , t "(λ(x,y) -> x)"          ~?= "\\@x::(a1, a2).(\\x.(\\y.x) @x.2) @x.1"
    , t "(λ(x:xs). x)"     ~?= "\\@x::[a1].(\\x.(\\xs.x) (tail @x)) (head @x)"
    , t "(λ(x:xs). x) [1]" ~?= "(\\x.(\\xs.x) (tail [1])) (head [1])"
    , t "(λ(x:xs)::[a]. x) [1]"   ~?= "(\\x::a.(\\xs::[a].x) (tail [1])) (head [1])"

    , t "let x::Int = 3 in x"           ~?= "(\\x::Int.x) 3"

    , t "(1, a)" ~?= "(1, a)"
    , t "(1, a, True)" ~?= "(1, a, True)"
    , t "((1, a), True)" ~?= "((1, a), True)"
    , t "(1, (a, True))" ~?= "(1, (a, True))"

    , t "[]" ~?= "[]"
    , t "[a]" ~?= "[a]"
    , t "[1, 2, 3]" ~?= "[1,2,3]"

    , t "letrec eq::Int->Int->Bool = (λm::Int.λn::Int. if iszero m then iszero n else if iszero n then False else eq (pred m) (pred n)) in eq 3 3"
   ~?= "(\\eq::(Int -> Int -> Bool).eq 3 3) (fix \\eq::(Int -> Int -> Bool).\\m::Int.\\n::Int.if iszero m then iszero n else if iszero n then False else eq (pred m) (pred n))"

     , t "let (x,y) = (1,2) in x" ~?= "(\\x.(\\y.x) 2) 1"
    , t "let ((x,y),z) = ((1,2),3) in y"  ~?= "(\\x.(\\y.(\\z.y) 3) 2) 1"
    , t "let (x,_) = (1,2) in x"  ~?= "(\\x.x) 1"

    , t "let x = 1 in case x of 1 -> 2"   ~?= "(\\x.case x of\n    1 -> 2) 1"
    , t "case 1 of 1 -> 2"   ~?= "case 1 of\n    1 -> 2"
    , t "let x = 1 in case x of 1 -> 2" ~?= "(\\x.case x of\n    1 -> 2) 1"

    , t "#x -> x" ~?= "#x.x"

    ]
  where
    t = rsExpr $ \s -> 
        let (mv, _, _) = s >- desugarSExpr
                           >- \x -> unsafePerformIO $ runDefault x
        in  case mv of 
              Left err -> show err
              Right v  -> show v

testCase_toTerm = (\t -> "de Bruijn index" ~: "toTerm" ~: t) |$>
    [ t "pred 99" ~?= "'1 99"


    , t "λx::A. pred x" ~?= "\\ '2 '0"

    , t "1 +" ~?= "'8 1"

    , t "λx::A. λy::A. x" ~?= "\\ \\ '1"
    , t "λu::A.λv::A.(λx::A.λy::A.x) u v" ~?= "\\ \\ (\\ \\ '1) '1 '0"
    , t "λx::A. λy::A. λz::A. x z (y z)" ~?= "\\ \\ \\ '2 '0 ('1 '0)"
    , t "λz::A. (λy::A. y (λx::A. x)) (λx::A. z x)" ~?= "\\ (\\ '0 (\\ '0)) (\\ '1 '0)"

    , t "λs::A. λz::A. z"     ~?= "\\ \\ '0"
    , t "λs::A. λz::A. s (s z)"     ~?= "\\ \\ '1 ('1 '0)"
    , t "λm::A. λn::A. λs::A. λz::A. m s (n z s)"     ~?= "\\ \\ \\ \\ '3 '1 ('2 '0 '1)"
    , t "λf::A. (λx::A. f (λy::A. x x y)) (λx::A. f (λy::A. x x y))"     ~?= "\\ (\\ '1 (\\ '1 '1 '0)) (\\ '1 (\\ '1 '1 '0))"
    , t "(λx::A. (λx::A. x)) (λx::A. x)"    ~?= "(\\ \\ '0) (\\ '0)"
    , t "(λx::Bool. if x then 1 else 2) True" ~?= "(\\ if '0 then 1 else 2) True"
    , t "λ_.3"    ~?= "\\ 3"
    , t "(λm::Int->Int.m 3)" ~?= "\\ '0 3"

    , t "(λx -> x)"         ~?= "\\ '0"
    , t "(λ(x,y) -> x)"     ~?= "\\ (\\ (\\ '1) '1.2) '0.1"
    , t "(λa -> (λ(x,y) -> x) a)"     ~?= "\\ (\\ (\\ '1) '1.2) '0.1"
    , t "(λ(x,y) -> x) 99"     ~?= "(\\ (\\ '1) 99.2) 99.1"

    , t "(1, 2)" ~?= "(1, 2)"
    , t "(1, (λx::A.x), True)" ~?= "(1, \\ '0, True)"
    , t "((1, (λx::A.x)), True)" ~?= "((1, \\ '0), True)"
    , t "(1, ((λx::A.x), True))" ~?= "(1, (\\ '0, True))"

    , t "[]" ~?= "[]"
    , t "[1, 2, 3]" ~?= "[1,2,3]"

    , t "let (x,y) = (1,2) in x"  ~?= "(\\ (\\ '1) 2) 1"
    , t "let ((x,y),z) = ((1,2),3) in y"  ~?= "(\\ (\\ (\\ '1) 3) 2) 1"

    , t "let x = 1 in case x of 1 -> 2"   ~?= "(\\ case '0 of\n    1 -> 2) 1"
    , t "case 1 of 1 -> 2"   ~?= "case 1 of\n    1 -> 2"

    , t "#x -> x" ~?= "# '0"
{--}
    ]
  where
    t = rsExpr $ \s -> 
        let (mv, _, _) = s >- (desugarSExpr >=> desugarExpr >=> toTerm)
                           >- \x -> unsafePerformIO $ runDefault x
        in  case mv of 
              Left err -> show err
              Right v  -> show v

testCase_restore = (\t -> "de Bruijn index" ~: "restore" ~: t) |$>
    [ t "pred 99" ~?= "pred 99"


    , t "λx::A. pred x" ~?= "\\x::A.pred x"

    , t "λx::A. λy::A. x" ~?= "\\x::A.\\y::A.x"
    , t "λu::A.λv::A.(λx::A.λy::A.x) u v" ~?= "\\u::A.\\v::A.(\\x::A.\\y::A.x) u v"
    , t "λx::A. λy::A. λz::A. x z (y z)" ~?= "\\x::A.\\y::A.\\z::A.x z (y z)"
    , t "λz::A. (λy::A. y (λx::A. x)) (λx::A. z x)" ~?= "\\z::A.(\\y::A.y (\\x::A.x)) (\\x::A.z x)"

    , t "(+)" ~?= "(+)"
    , t "(+) 1" ~?= "(+) 1"
    , t "(+) 1 2" ~?= "(+) 1 2"
    , t "1 + 2" ~?= "(+) 1 2"
    , t "1+" ~?= "(+) 1"

    , t "+1" ~?= "\\l::a1.(+) l 1"

    , t "1 + 2 * 3" ~?= "(*) ((+) 1 2) 3"

    , t "λs::A. λz::A. z"     ~?= "\\s::A.\\z::A.z"
    , t "λs::A. λz::A. s (s z)"     ~?= "\\s::A.\\z::A.s (s z)"
    , t "λm::A. λn::A. λs::A. λz::A. m s (n z s)"     ~?= "\\m::A.\\n::A.\\s::A.\\z::A.m s (n z s)"
    , t "λf::A. (λx::A. f (λy::A. x x y)) (λx::A. f (λy::A. x x y))"     ~?= "\\f::A.(\\x::A.f (\\y::A.x x y)) (\\x::A.f (\\y::A.x x y))"
    , t "(λx::A. (λx::A. x)) (λx::A. x)"    ~?= "(\\x::A.\\x::A.x) (\\x::A.x)"
    , t "(λx::Bool. if x then 1 else 2) True" ~?= "(\\x::Bool.if x then 1 else 2) True"
    , t "λ_.3"    ~?= "\\_::a1.3"
    , t "(λm::Int->Int.m 3)" ~?= "\\m::(Int -> Int).m 3"

    , t "(1, 'a')" ~?= "(1, 'a')"
    , t "(1, 'a', True)" ~?= "(1, 'a', True)"
    , t "((1, 'a'), True)" ~?= "((1, 'a'), True)"
    , t "(1, ('a', True))" ~?= "(1, ('a', True))"

    , t "[]" ~?= "[]"
    , t "[1, 2, 3]" ~?= "[1,2,3]"

    , t "let (x,y) = (1,2) in x"  ~?= "(\\x::a1.(\\y::a2.x) 2) 1"
    , t "let ((x,y),z) = ((1,2),3) in y"  ~?= "(\\x::a1.(\\y::a2.(\\z::a3.y) 3) 2) 1"

    , t "let x = 1 in case x of 1 -> 2"   ~?= "(\\x::a1.case x of\n    1 -> 2) 1"
    , t "case 1 of 1 -> 2"   ~?= "case 1 of\n    1 -> 2"

    , t "let x = 1 in case x of 1 -> 2" ~?= "(\\x::a1.case x of\n    1 -> 2) 1"

    , t "#x -> x" ~?= "#x::a1.x"
{--}
    ]
  where
    t = rsExpr $ \s ->
        let (mv, _, _) = s >- (desugarSExpr >=> desugarExpr >=> toTerm >=> restore)
                           >- \x -> unsafePerformIO $ runDefault x
        in  case mv of 
              Left err -> show err
              Right v  -> show v
    t' = rsExpr $ \s ->
        let (mv, _, _) = s >- (desugarSExpr >=> desugarExpr >=> toTerm >=> restore)
                           >- \x -> unsafePerformIO $ runDefault x
        in  case mv of 
              Left err -> show err
              Right v  -> show v

testCase_typeof = fmap (\t -> "typeof" ~: "" ~: t)
    [ 

      t "λu::A.u" ~?= "A -> A"

    , t "(λx::Bool.x) True" ~?= "Bool"
    , t "(λx::Int.x) 3"     ~?= "Int"
    , t "(λx::[Char].x) \"abc\"" ~?= "[Char]"
    , t "if True then 1 else 2" ~?= "Int"

    , t "(λ_.3)"      ~?= "a1 -> Int"
    , t "(λ_.3) True" ~?= "Int"
    , t "(λm::Int->Int.m 3)" ~?= "(Int -> Int) -> Int"

    , t "(λx. x)"        ~?= "a1 -> a1"
    , t "(λx. x) 3"      ~?= "Int"
    , t "let x = 3 in x" ~?= "Int"

    , t "pred 3"        ~?= "Int"

    , t "[]"        ~?= "[a1]"
    , t "[1, 2, 3]" ~?= "[Int]"
    , t "head [1, 2, 3]" ~?= "Int"

    , t "(λeq::Int->Int->Bool.λm::Int.λn::Int. if iszero m then iszero n else if iszero n then False else eq (pred m) (pred n))"
             ~?= "(Int -> Int -> Bool) -> Int -> Int -> Bool"
    , t "fix (λeq::Int->Int->Bool.λm::Int.λn::Int. if iszero m then iszero n else if iszero n then False else eq (pred m) (pred n))"
             ~?= "Int -> Int -> Bool"

    , t "letrec eq::Int->Int->Bool = (λm::Int.λn::Int. if iszero m then iszero n else if iszero n then False else eq (pred m) (pred n)) in eq 3 3"
       ~?= "Bool"

    , t "(λx -> x)"               ~?= "a1 -> a1"
    , t "(λ(x,y) -> x)"           ~?= "(a6, a2) -> a6"
    , t "(λm::a->a. m 3)" ~?= "(Int -> Int) -> Int"

    , t "(\\x.(\\y.x) 2) 1"       ~?= "Int"
    , t "(λ(x,y) -> x) (1,2)"     ~?= "Int"
    , t "let (x,y) = (1,2) in x"  ~?= "Int"

    , t "let ((x,y),z) = ((1,2),3) in y" ~?= "Int"

    , t "let x = 1 in case 1 of 1 -> 2" ~?= "Int"
    , t "let x = 1 in case x of 1 -> 2" ~?= "Int"
  
    , t "#x -> x" ~?= "a1 -> a1"

   -- , t "if True then (if False then x else 10) else x + y + 20" ~?= "Int"

   ]
  where 
    t = rsExpr $ \s ->
        let (mv, _, _) = s >- (desugarSExpr >=> desugarExpr >=> toTerm >=> typeofTerm)
                           >- \x -> unsafePerformIO $ runDefault x
        in  case mv of 
              Left err -> show err
              Right v  -> show v

testCase_eval = fmap (\t -> "eval" ~: "" ~: t)
    [
      t "(λu::Int.u) 1"  ~?= "1" 

    , t "(λu::Int.λv::Int.(λx::A.λy::A.x)) 1 2"  ~?= "\\x::A.\\y::A.x"
    , t "if True then 1 else 2"  ~?= "1"


    , t "(+) 1 1" ~?= "2"
    , t "1 + 1"   ~?= "2"
    , t "2 + 3 + 4"   ~?= "9"
    , t "2 * 3 * 4"   ~?= "24"
    , t "(2 + 3 + 4) * 5" ~?= "45"
    , t "2 + 3 + 4 * 5" ~?= "45"

    , t "(λx::Bool. if x then 1 else 2) True" ~?= "1"
    , t "(λu::A.u)"        ~?= "\\u::A.u"
    , t "(λm::Int->Int.m 3) (λ_.1)" ~?= "1"

    , t "(λ_. 7) True" ~?= "7"
    , t "let x = 3 in x"        ~?= "3"

    , t "pred 1"         ~?= "0"
    , t "pred (pred 55)" ~?= "53"
    , t "succ 7"         ~?= "8"
    , t "succ (succ 7)"  ~?= "9"
    , t "head [1, 2, 3]" ~?= "1"
    , t "tail [1, 2, 3]" ~?= "[2,3]"
    , t "null []"  ~?= "True"
    , t "null [1]" ~?= "False"

    , t "iszero 0" ~?= "True"
    , t "iszero 3" ~?= "False"

    , t "((fix (λp::Int->Int->Int. λm::Int. λn::Int. if (iszero m) then n else (succ (p (pred m) n)))) 3) 3"  ~?= "6"

    , t "letrec eq::Int->Int->Bool = (λm::Int.λn::Int. if iszero m then iszero n else if iszero n then False else eq (pred m) (pred n)) in eq 3 3"
       ~?= "True"

    , t "(λx. x) 3" ~?= "3"
    , t "let x = 3 in x"          ~?= "3"
    , t "let (x,y) = (1,2) in x"          ~?= "1"
    , t "let ((x,y),z) = ((1,2),3) in y"  ~?= "2"
    , t "let (x,_) = (1,2) in x"          ~?= "1"
    , t "let (_,z) = ((1,2),3) in z"      ~?= "3"

    , t "case 1 of 1 -> 2"   ~?= "2"
    , t "let x = 1 in case 1 of 1 -> 2"   ~?= "2"
    , t "let x = 1 in case x of 1 -> 2"   ~?= "2"
    , t "let x = 1 in case x of 1 -> 2\n 2 -> 3"   ~?= "2"
    ]
  where 
    t = rsExpr $ \s -> 
        let (mv, _, _) = unsafePerformIO $ runDefault (interpretE s)
        in  case mv of 
              Left err -> show err
              Right (RETURN v) -> show v
              Right VOID       -> "- void -"


