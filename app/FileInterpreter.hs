{-# LANGUAGE MultiWayIf #-}
import Config
import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.Arrow
import DeepControl.Monad.RWS
import DeepControl.MonadTrans (liftIO)
import Util.FileSystem

import Lambda.Parser
import Lambda.Evaluator
import Lambda.DataType
import Lambda.Action

import System.IO
import Data.List (sortBy, drop, elemIndex)
import Data.Char (isDigit)
import System.Directory (readable)
import System.FilePath ((</>), takeExtension, takeDirectory)

-- for debug
import Debug.Trace 
import System.IO.Unsafe

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
        if | null input                       -> loop
           | input `elem` ["l", ":l", "list"] -> runFI 
           | input `elem` ["u", ":u", "up"]   -> upDirectory 
           | input `elem` ["r", ":r", "re-eval"] -> do
                fn <- getFN
                case fn of
                  Just n  -> evalFile (fdInfos!!n)
                  Nothing -> runFI
           | not (and $ isDigit |$> input) -> loop
           | otherwise -> let n = (read input :: Int)
                          in if n >= length fdInfos 
                             then loop 
                             else let fdInfo = fdInfos!!n
                                  in if fdPath fdInfo == cd 
                                     then upDirectory 
                                     else if isDirectory fdInfo 
                                          then diveDirectory fdInfo 
                                          else evalFile fdInfo
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
            rec initEnv initStates codes
            putStrLn $ "[ end: " ++ show n ++ ". " ++ path ++ " ]"
      Nothing -> runFI
    where
      rec :: LambdaEnv -> LambdaStates -> [SExpr] -> IO ()
      rec _   _      []     = (*:) ()
      rec env states (c:cs) = do
          (mv, newstates, _) <- runInterpretE (env, states) c
          case mv of
            Left err            -> showError err >> rec env newstates cs
            Right (RETURN expr) -> do
              putStr "=> "
              putStrLn $ show expr
              rec env newstates cs
            Right VOID          -> rec env newstates cs
        where
          showError err@(PARSE _)     = error $ show err   -- TODO: throwError
          showError err@(COMPILE _ _) = error $ show err   -- TODO: throwError
          showError err               = do
              putStr "*** "
              putStrLn $ show err

