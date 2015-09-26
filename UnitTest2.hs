module UnitTest2 where 
import Test.HUnit

import MonadX.Applicative
import MonadX.Monad
import Lambda.Parser
import Lambda.Compiler.Typeof
import Lambda.Evaluator
import Lambda.Convertor

import Lambda.DataType.Type (Type((:->)))
import qualified Lambda.DataType.PatternMatch as PM
import qualified Lambda.DataType.Type as Ty
import qualified Lambda.DataType.Expr as E
import qualified Lambda.DataType.SExpr as SE
import Lambda.DataType (runLambda, SExpr, PM, Name)
import Util.Pseudo

import System.IO.Unsafe (unsafePerformIO)

-- | Run Unit-Test of this module. 
run = runTestTT $ TestList 
        [
          TestList testCase_unifyType,

          TestList testCase_pdfoldMap
    	]

-- *UnitTest> run

----------------------------------------------------------------
-- 
----------------------------------------------------------------

testCase_unifyType = (\t -> "unifyType" ~: "" ~: t) |$>
    [ t (typeunify [(Ty.CONS (Ty.CHAR), Ty.CONS (Ty.VAR "a1"))]) ~?= "[(\"a1\",Char)]"

--    , show 
    ]
  where
    t x = let (mv, _, _) = unsafePerformIO $ runLambda x initEnv initStates
          in  case mv of 
                Left err -> show err
                Right v  -> show v

testCase_pdfoldMap = (\t -> "pdfoldMap" ~: "" ~: t) |$>
    [ (pdfoldMap takename (PM.cons (PM.var "a") PM.nil)) ~?= ["a"],
      (pdfoldMap takename (PM.cons (PM.var "a") (PM.cons (PM.var "b") PM.nil))) ~?= ["a", "b"],
      (pdfoldMap takename (PM.cons (PM.var "a") (PM.cons (PM.var "b") (PM.cons (PM.var "c") PM.nil)))) ~?= ["a", "b", "c"]

    ]
  where
    takename :: PM -> [Name]
    takename (PM.VAR name _) = [name]
    takename (PM.CONS a d _) = takename a ++ takename d
    takename _               = []

