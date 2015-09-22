import Config
import MonadX.Applicative
import MonadX.Monad
import MonadX.Arrow
import MonadX.Monad.RWS
import Util.FileSystem

import Lambda.Parser
import Lambda.Evaluator
import Lambda.DataType
import Lambda.Action

import System.IO
import qualified Data.Map as M 
import Data.List (sortBy, drop, elemIndex)
import Data.Char (isDigit)
import System.Directory (readable)
import System.FilePath ((</>), takeExtension, takeDirectory)

-- for debug
import Debug.Trace 

main :: IO ()
main = run

run :: IO ()
run = do
    let sd = startDir initConfig
    fdInfos <- lambdaFDInfos sd
    (runRWST runFI) fdInfos (sd, Nothing)
    (*:) ()

lambdaFDInfos :: FilePath -> IO [FDInfo]
lambdaFDInfos fp = do
    x <- toFDInfo fp
    xs <- lookoverP ((isLamFile <$|(||)|*> isDirectory) <$|(&&)|*> isReadable) (sortBy compare) fp
    (*:) $ x:xs
  where
    isLamFile :: FDInfo -> Bool
    isLamFile = extension <$|(==".hs")
      where
        extension :: FDInfo -> String 
        extension = takeExtension |$> fdPath
    isReadable :: FDInfo -> Bool
    isReadable = readability <$|(==(Just True))
      where
        readability :: FDInfo -> Maybe Bool
        readability = readable |$>> fdPerms

--------------------------------------------------------------------------
-- FileInterpreter
--------------------------------------------------------------------------

type CD = String         -- Current Dirctory
type FN = Maybe Int      -- File Number
type Vars = (CD, FN)
type FileInterpreter a = RWST [FDInfo] () Vars IO a -- TODO: エラー処理

putCD :: CD -> FileInterpreter ()
putCD x = do
    (cd, n) <- get
    put (x, n)
getCD :: FileInterpreter CD
getCD = do
    (cd, n) <- get
    (*:) cd
putFN :: FN -> FileInterpreter ()
putFN x = do
    (cd, n) <- get
    put (cd, x)
getFN :: FileInterpreter FN
getFN = do
    (cd, n) <- get
    (*:) n

runFI :: FileInterpreter ()
runFI = do 
    cd <- getCD
    fdInfos <- liftIO $ lambdaFDInfos cd
    local (const fdInfos) $ showFilenames >> loop
  where 
    showFilenames :: FileInterpreter ()
    showFilenames = do 
        fdInfos <- ask
        cd <- getCD
        let paths = fdInfos <$| (fdPath &&& isDirectory >>> (neat cd))
        liftIO $ putStrLn $ "Files in '" ++ cd ++ "'."
        liftIO $ putStrLn "Input number or command; 'list', 'up' or 're-eval' (abbr. ':l', ':u', ':r', respectively)."
        liftIO $ mapM_ (\(i,path) -> putStrLn $ "- "++ show i ++". "++ path) $ zip [0..] paths
      where
        neat cd (path, isdir)
            | path == (cd</>"") = "../"
            | isdir             = "[" ++ drop (length cd + 1) path ++ "]"
            | otherwise         = drop (length cd + 1) path
    loop :: FileInterpreter ()
    loop = do
        fdInfos <- ask
        cd <- getCD
        liftIO $ putStr $ "[" ++ cd ++ "]: "
        input <- liftIO getLine
        if null input then loop else do 
        if input `elem` ["l", ":l", "list"] then runFI else do
        if input `elem` ["u", ":u", "up"] then upDirectory else do
        if input `elem` ["r", ":r", "re-eval"]
        then do fn <- getFN
                case fn of
                  Just n  -> evalFile (fdInfos!!n)
                  Nothing -> runFI
        else do
        if not (and $ isDigit |$> input) then loop else do
        let n = (read input :: Int)
        if n >= length fdInfos then loop else do
        let fdInfo = fdInfos!!n
        if fdPath fdInfo == cd then upDirectory else do
        if isDirectory fdInfo then diveDirectory fdInfo else do
        evalFile fdInfo
      where
        upDirectory :: FileInterpreter ()
        upDirectory = do
            putFN Nothing
            cd <- getCD
            putCD $ takeDirectory cd
            runFI
        diveDirectory :: FDInfo -> FileInterpreter ()
        diveDirectory fdInfo = do
            putFN Nothing
            putCD $ fdPath fdInfo 
            runFI
        evalFile :: FDInfo -> FileInterpreter ()
        evalFile fdInfo = do
            fdInfos <- ask
            putFN $ fdInfo `elemIndex` fdInfos
            runREPL fdInfo
            loop 

----------------------------------------------------------------------------------------------------------------
-- REP (read - eval - print - loop)
----------------------------------------------------------------------------------------------------------------

runREPL :: FDInfo -> FileInterpreter ()
runREPL fdInfo = do
    fn <- getFN
    case fn of
      Just n -> do
        let path = fdPath fdInfo
        mmv <- liftIO $ loadFile path
        liftIO $ case mmv of   
          Left err -> do
            putStrLn "" *> putStrLn ("*** Parse error: ")
            putStr $ show err
            putStrLn "" *> putStrLn ("________________________________________")
          Right codes -> do
            putStrLn $ "[ begin: " ++ show n ++ ". " ++ path ++ " ]"
            rec initStates codes
            putStrLn $ "[ end: " ++ show n ++ ". " ++ path ++ " ]"
      Nothing -> runFI
    where
      rec :: LambdaStates -> [SExpr] -> IO ()
      rec _      []     = (*:) ()
      rec states (c:cs) = do
          (mv, states', _) <- runLambdaE (initEnv, states) c
          --let newstates = refreshFreeVars states'
          let newstates = states'
          case mv of
            Left err            -> showError err >> rec newstates cs
            Right (RETURN expr) -> do
              putStr "=> "
              putStrLn $ show expr
              rec newstates cs
            Right VOID          -> rec newstates cs
        where
          showError err@(PARSE _)     = error $ show err   -- TODO: throwError
          showError err@(COMPILE _ _) = error $ show err   -- TODO: throwError
          showError err               = do
              putStr "*** "
              putStrLn $ show err

