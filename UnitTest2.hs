module UnitTest where 
import Test.HUnit

import MonadX.Applicative
import MonadX.Monad
import MonadX.Arrow
import Lambda.Parser
import Lambda.Compiler
import Lambda.Evaluator

import Lambda.DataType.Prim.Type (Type((:->)))
import qualified Lambda.DataType.Prim.Type as Ty
import Lambda.DataType.Expr (Expr)
import qualified Lambda.DataType.Expr as E
import Lambda.DataType.SExpr (SExpr, showSExpr)
import qualified Lambda.DataType.SExpr as SE
import Lambda.DataType (runLambda)

import Data.List (intersperse)
import Text.ParserCombinators.Parsec
import System.IO.Unsafe (unsafePerformIO)

-- | Run Unit-Test of this module. 
run = runTestTT $ TestList 
        [
          TestList testCase_matchType
    	]

-- *UnitTest> run

----------------------------------------------------------------
-- template
----------------------------------------------------------------

-- read & show
rs :: (SExpr -> a) -> String -> a
rs func str = 
  case readSExpr str of
    Left err           -> error $ show err ++"\n"
    Right (expr, [])   -> func expr
    Right (expr, rest) -> error $ "*** parser failed to exhaust whole string:\n" 
                                ++"*** parsed: "++ show expr ++"\n" 
                                ++"*** rest: " ++ show rest ++"\n"

----------------------------------------------------------------
-- 
----------------------------------------------------------------


testCase_matchType = (\t -> "matchType" ~: "" ~: t) |$>
    [ show (matchType (Ty.VAR "a" :-> Ty.VAR "a", (Ty.INT :-> Ty.INT :-> Ty.BOOL) :-> Ty.INT :-> Ty.INT :-> Ty.BOOL) (Ty.VAR "a")) ~?= "Int -> Int -> Bool"

--    , show 
    ]


