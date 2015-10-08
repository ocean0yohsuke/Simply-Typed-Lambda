{-# LANGUAGE 
    MultiParamTypeClasses, TypeSynonymInstances, 
    DeriveFunctor, DeriveFoldable, DeriveTraversable,
    GeneralizedNewtypeDeriving,
    FlexibleInstances
 #-}
module Lambda.DataType (
    module Lambda.DataType.Common,
    module Lambda.DataType.Error,
    module Lambda.DataType.Gadget,
    module MonadX.Monad.RWS,
    module Util.Pseudo,

    PM, Type((:->)), Expr, SExpr, Expr_, SExpr_, PM_,
    Term(..), isList, toList, fromList,
    SCode(..), SFile, Filename,
    Return(..), ReturnT, ReturnE,
    LambdaError(..),

    runLambda, Lambda(..), unLambda, 
    LambdaEnv(..), setContext, setMSP, setConfig,
    LambdaStates,
    Context, Config(..), 
    Def, TypeDef, TypeVarCounter,

    isValue, isThunked,
    isVAR, isTHUNK, isQUOTE, isQQUOTE,

    unit, var, bool, int, char, lam, lamm, fix, app,
    list, tpl, tag,
) where

import DeepControl.Applicative
import DeepControl.Monad
import MonadX.Monad.RWS hiding (liftCatch)
import MonadX.Monad.Error hiding (liftCatch)

import Config
import Lambda.DataType.Common
import Lambda.DataType.Error
import Lambda.DataType.Gadget
import Lambda.DataType.PatternMatch (PM)
import qualified Lambda.DataType.PatternMatch as PM
import Lambda.DataType.Type (Type((:->)))
import qualified Lambda.DataType.Type as Ty
import Lambda.DataType.Expr (Expr)
import Lambda.DataType.SExpr (SExpr)
import Lambda.DataType.Shadow.Expr_ (Expr_)
import Lambda.DataType.Shadow.SExpr_ (SExpr_)
import Lambda.DataType.Shadow.PatternMatch_ (PM_)
import Util.Pseudo

import Prelude hiding (or, foldl)
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Foldable hiding (toList)
import Data.Traversable (Traversable)
import Data.IORef

-------------------------------------
-- Term
-------------------------------------

-- TODO: NIL
data Term = NULL
          | BOOL Bool MSP
          | INT Integer MSP
          | CHAR Char MSP
          | LAM (PM, Type) Term MSP
          | LAMM (PM, Type) Term MSP     -- lambda-macro
          --
          | UNIT MSP
          | VAR Index MSP             -- de Bruijn index
          | FIX Term MSP
          | APP Term Term MSP
          --
          | CONST Name (Lambda Term)           -- constant
          | COMND Name (Lambda ReturnT)        -- command
          | AFUNC Name (Term -> Lambda Term)   -- actual function
          | WAFUNC Name (Term -> Lambda Term)  -- weekly actual function for infinite-list in lazy-evaluation
          | PROC Name (Term -> Lambda ReturnT) -- procedure
          | META Name (Term -> Lambda ReturnT) -- meta
          -- gadget
          | SYN (Syntax Term) MSP
          | SEN (Sentence Term) MSP
          | QUT (Quote Term) MSP
          | LIST (List Term) MSP
          | TPL (Tuple Term) MSP
          | TAG (Tag Term) MSP
          --
          | THUNK Term

unit = UNIT Nothing
var name = VAR name Nothing
bool b = BOOL b Nothing
int n = INT n Nothing
char c = CHAR c Nothing
lam p e = LAM p e Nothing
lamm p e = LAMM p e Nothing
fix e = FIX e Nothing
app e1 e2 = APP e1 e2 Nothing
list g = LIST g Nothing
tpl g = TPL g Nothing
tag g = TAG g Nothing

isList :: List Term -> Bool
isList NIL                 = True
isList (CONS _ (LIST g _)) = isList g
isList _                   = False
toList :: List Term -> [Term]
toList NIL                            = []
toList (CONS a (LIST NIL _))          = [a]
toList (CONS a (LIST d@(CONS _ _) _)) = a : (toList d)
fromList :: [Term] -> List Term
fromList [] = NIL
fromList [a] = CONS a (LIST NIL Nothing)
fromList (a:as) = CONS a (LIST (fromList as) Nothing)

instance Show Term where
    show NULL            = "()"
    show (BOOL bool _)   = show bool
    show (INT n _)       = show n
    show (CHAR c _)      = "'"++ [c] ++"'"
    show (UNIT _)        = "_"
    show (VAR index _)   = "'"++ show index
    show (LAM _ t _)     = "\\ "++ show t 
    show (LAMM _ t _)    = "# "++ show t
    --
    show (CONST name _)  = "<"++ name ++">"
    show (COMND name _)  = "<"++ name ++">"
    show (AFUNC name _)  = "<"++ name ++">"
    show (WAFUNC name _) = "<"++ name ++">"
    show (PROC name _)   = "<"++ name ++">"
    show (META name _)   = "<"++ name ++">"
    --
    show (FIX lambda _)  = "fix " ++ show lambda 
    show (APP t1 t2 _)   = showOperator t1 ++ " " ++ showOperand t2
      where
        showOperator t@(LAM _ _ _)  = "("++ show t ++")"
        showOperator t@(FIX _ _)    = "("++ show t ++")"
        showOperator t@(SYN _ _)    = "("++ show t ++")"
        showOperator t              = show t
        showOperand t@(APP _ _ _)   = "("++ show t ++")"
        showOperand t               = showOperator t
    -- list
    show x@(LIST g@(CONS a d) _) = 
        if isList g
        then case a of
            CHAR _ _ -> show $ toString $ toList g
            _        -> show $ toList g
        else showWithParens a ++ ":"++ showWithParens d
      where
        toString []                = [] 
        toString ((CHAR c _):cs)   = c : toString cs
        showWithParens x@(LAM _ _ _) = "("++ show x ++")"
        showWithParens x@(FIX _ _)   = "("++ show x ++")"
        showWithParens x@(APP _ _ _) = "("++ show x ++")"
        showWithParens x@(SYN _ _)   = "("++ show x ++")"
        showWithParens x             = show x
    -- gadget
    show (SYN g _)   = show g
    show (SEN g _)   = show g
    show (QUT g _)   = show g
    show (LIST g _)  = show g
    show (TPL g _)   = show g
    show (TAG g _)   = show g
    --
    show (THUNK x)      = "_["++ show x ++"]_"

instance Eq Term where
    NULL     == NULL      = True
    BOOL x _ == BOOL y  _ = x == y
    INT x _  == INT y _   = x == y
    CHAR x _ == CHAR y _  = x == y
    UNIT _   == _         = True
    _        == UNIT _    = True
    VAR x _  == VAR y _   = x == y
    LAM x1 x2 _  == LAM y1 y2 _  = (x1,x2) == (y1,y2)
    LAMM x1 x2 _ == LAMM y1 y2 _ = (x1,x2) == (y1,y2)
    --
    CONST x _ == CONST y _ = x == y
    COMND x _ == COMND y _ = x == y
    AFUNC x _ == AFUNC y _ = x == y
    WAFUNC x _ == WAFUNC y _ = x == y
    PROC x _  == PROC y _  = x == y
    META x _  == META y _  = x == y
    --
    FIX x _     == FIX y _       = x == y
    APP x1 x2 _ == APP y1 y2 _   = (x1,x2) == (y1,y2)
    -- gadget
    SYN x _     == SYN y _     = x == y
    SEN x _     == SEN y _     = x == y
    QUT x _     == QUT y _     = x == y
    LIST x _    == LIST y _    = x == y
    TPL x _     == TPL y _     = x == y
    TAG x _     == TAG y _     = x == y
    --
    THUNK x     == THUNK y     = x == y
    _           == _           = False

--------------------------------------------------
-- SCode, SFile
--------------------------------------------------

data SCode = EXPR SExpr
           | COMMENT String
           | LINEBREAK
           | EOF

instance Show SCode where
  show (EXPR x)    = show x
  show (COMMENT s) = s
  show LINEBREAK   = ""

type SFile = [SCode]
type Filename = String

--------------------------------------------------
-- Eval
--------------------------------------------------

data Return a = RETURN a
              | VOID
  deriving (Show, Eq, Functor, Foldable, Traversable)

type ReturnT = Return Term
type ReturnE = Return Expr

-------------------------------------
-- Lambda
-------------------------------------

newtype Lambda a = Lambda (ErrorT LambdaError 
                          (RWST LambdaEnv () LambdaStates IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

unLambda :: Lambda a -> ErrorT LambdaError 
                        (RWST LambdaEnv () LambdaStates IO) a
unLambda (Lambda a) = a

data LambdaEnv = LambdaEnv {
      context :: Context
    , msp :: MSP
    , config :: Config
    }
setContext :: Context -> LambdaEnv -> LambdaEnv
setContext x (LambdaEnv _ b c) = LambdaEnv x b c
setMSP :: MSP -> LambdaEnv -> LambdaEnv
setMSP x (LambdaEnv a _ c) = LambdaEnv a x c
setConfig :: Config -> LambdaEnv -> LambdaEnv
setConfig x (LambdaEnv a b _) = LambdaEnv a b x

type Context  = [(Name, (Type, Term))]

type LambdaStates = (Def, TypeDef, TypeVarCounter)  -- TODO: records

type Def = [(Name, (Type, Term))]           -- Function Definition 
type TypeDef = M.Map Name [(Name, [Type])]
type TypeVarCounter = Int

runLambda :: Lambda a 
             -> LambdaEnv    -- Reader
             -> LambdaStates -- States
             -> IO (Either LambdaError a, LambdaStates, ())
runLambda (Lambda x) env states = x >- runErrorT
                                    >- (runRWST >-> (|>env) >-> (|>states)) 
          
----------------------------------------------------------------------
-- Monad instances
----------------------------------------------------------------------

instance MonadReader LambdaEnv Lambda where
    ask     = Lambda $ trans $ ask
    local m = Lambda . (mapErrorT (local m)) . unLambda
instance MonadState LambdaStates Lambda where
    get   = Lambda $ trans $ get
    put   = Lambda . trans . put
    state = Lambda . trans . state
instance MonadError LambdaError Lambda where
    throwError              = Lambda . throwError
    catchError (Lambda x) f = 
        let f' = unLambda |$> f
        in Lambda $ catchError x f'

-------------------------------------
-- other
-------------------------------------

isVAR :: Term -> Bool
isVAR (VAR _ _) = True
isVAR _         = False
isTHUNK :: Term -> Bool
isTHUNK (THUNK _) = True
isTHUNK _         = False
isQUOTE :: Term -> Bool
isQUOTE (QUT (QUOTE _) _) = True
isQUOTE _                 = False
isQQUOTE :: Term -> Bool
isQQUOTE (QUT (QQUOTE _) _) = True
isQQUOTE _                  = False

isValue :: Term -> Bool
isValue (BOOL _ _)   = True
isValue (INT _ _)    = True
isValue (CHAR _ _)   = True
isValue (LAM _ _ _)  = True
isValue (LAMM _ _ _) = True
isValue (CONST _ _)  = True
isValue (COMND _ _)  = True
isValue (AFUNC _ _)  = True
isValue (WAFUNC _ _) = True
isValue (PROC _ _)   = True
isValue (META _ _)   = True
isValue (LIST NIL _)        = True
isValue (LIST (CONS _ _) _) = True
isValue (TPL (TUPLE _) _)   = True
isValue (TAG (TAGAs _ _) _) = True
isValue _            = False

isThunked :: Term -> Bool
isThunked (THUNK _)      = True
isThunked (LAM _ x _)    = isThunked x
isThunked (LAMM _ x _)   = isThunked x
isThunked (FIX x _)      = isThunked x
isThunked (APP x1 x2 _)  = isThunked x1 || isThunked x2
isThunked (SYN syn _)    = foldl (||) False $ isThunked |$> syn
isThunked (SEN sen _)    = foldl (||) False $ isThunked |$> sen
isThunked (LIST (CONS a d) _)  = isThunked a || isThunked d
isThunked (TPL (TUPLE xs) _)   = or $ isThunked |$> xs
isThunked (TPL (TPLPrj x _) _) = isThunked x
isThunked (TAG (TAGAs _ xs) _) = or $ isThunked |$> xs
isThunked (TAG (TAGPrj x _) _) = isThunked x
isThunked _              = False


