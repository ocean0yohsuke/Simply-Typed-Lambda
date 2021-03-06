module Lambda.Evaluator.Def where

import DeepControl.Applicative
import DeepControl.Monad

import Lambda.DataType (Term(CONST, COMND, AFUNC, WAFUNC, PROC, META), Name, Def)
import Lambda.DataType.Type
import Lambda.Evaluator.Micro
import Lambda.Evaluator.Prelude
import Lambda.Evaluator.Meta
import Lambda.Evaluator.Debug
import Lambda.Evaluator.IO

-- for debug
import Debug.Trace 

initDef :: Def
initDef = microDef 
        ++ preludeDef 
        ++ metaDef
        ++ debugDef 
        ++ ioDef

----------------------------------------------------------------------
-- Def
----------------------------------------------------------------------

microDef :: Def
microDef = [
    -- primitive
      ("iszero", (INT :-> BOOL,  AFUNC "iszero" evalIsZero))
    , ("pred",   (INT :-> INT,   AFUNC "pred" evalPred))
    , ("succ",   (INT :-> INT,   AFUNC "succ" evalSucc))

    , ("==", (VAR "a" :-> VAR "a" :-> BOOL,  AFUNC "(==)" evalEqual))

    -- list
    , ("head", (CONS (VAR "a") :-> VAR "a",                    WAFUNC "head" evalHead)) 
    , ("tail", (CONS (VAR "a") :-> CONS (VAR "a"),             WAFUNC "tail" evalTail))
    , (":"   , (VAR "a" :-> CONS (VAR "a") :-> CONS (VAR "a"), WAFUNC "(:)"  evalCons))
    , ("null", (CONS (VAR "a") :-> BOOL,                       AFUNC "null" evalNull))
    ]

preludeDef :: Def
preludeDef = [
    -- arithmetic
      ("+",   (INT :-> INT :-> INT,  AFUNC "(+)" evalPlus))
    , ("-",   (INT :-> INT :-> INT,  AFUNC "(-)" evalMinus))
    , ("*",   (INT :-> INT :-> INT,  AFUNC "(*)" evalMult))
    , ("/",   (INT :-> INT :-> INT,  AFUNC "(/)" evalDiv))
    , ("^",   (INT :-> INT :-> INT,  AFUNC "(^)" evalPower))
    , ("mod", (INT :-> INT :-> INT,  AFUNC "mod" evalMod))

    , ("<",   (INT :-> INT :-> BOOL,  AFUNC "(<)" evalLtNum))
    , (">",   (INT :-> INT :-> BOOL,  AFUNC "(>)" evalGtNum))
    , ("<=",  (INT :-> INT :-> BOOL,  AFUNC "(<=)" evalLtEq))
    , (">=",  (INT :-> INT :-> BOOL,  AFUNC "(>=)" evalGtEq))

    , ("show", (VAR "a" :-> CONS CHAR, AFUNC "show" evalShow))
    ]

metaDef :: Def
metaDef = [
    -- evaluator
      ("eval",         (UNIT :-> UNIT,          META "eval" evalEval))
    , ("evalN",        (INT :-> UNIT :-> UNIT,  META "evalN" evalEvalN))
    , ("lazyeval",     (UNIT :-> UNIT,          META "lazyeval" evalLazyEval))
    , ("lazyevalN",    (INT :-> UNIT :-> UNIT,  META "lazyevalN" evalLazyEvalN))
    , ("macro_expand", (QUT :-> UNIT,           AFUNC "evalMacroExpand" evalMacroExpand))

　   , ("typeof",  　　(UNIT :-> UNIT,  　　　　　　　　　　　META "typeof" evalTypeof))

    -- compiler
    , ("desugar", (CONS CHAR :-> CONS CHAR,  PROC "desugar" evalDesugar))
    , ("toterm",  (CONS CHAR :-> CONS CHAR,  PROC "toterm"  evalToTerm))
    , ("restore", (CONS CHAR :-> CONS CHAR,  PROC "restore" evalRestore))

    ]

debugDef :: Def
debugDef = [
      ("unittest", (CONS CHAR :-> CONS (TUPLE [UNIT, UNIT]) :-> CONS CHAR,     META "unittest" evalUnitTest))

    -- show
    , ("showExpr",  (CONS CHAR :-> CONS CHAR,  PROC "showExpr" evalShowExpr))
    , ("showSExpr", (CONS CHAR :-> CONS CHAR,  PROC "showSExpr" evalShowSExpr))

    , ("showDef", (NULL, COMND "showDef" evalShowDef))
    , ("showDef",     (NULL, COMND "showDef" evalShowDef))
    , ("showDef_",    (NULL, COMND "showDef_" evalShowDef_))
    ]

ioDef :: Def
ioDef = [
      ("import", (CONS CHAR :-> NULL,     PROC "import" evalImport))

    , ("print",  (VAR "a" :-> NULL,             AFUNC "print" evalPrint))
 --   , (">>",     (VAR "a" :-> VAR "b" :-> NULL, AFUNC "(>>)" evalSeq))
    ]



