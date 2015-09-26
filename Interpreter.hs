import MonadX.Applicative
import MonadX.Monad
import MonadX.Arrow
import MonadX.Monad.RWS

import Lambda.Parser (readSCode)
import Lambda.Evaluator
import Lambda.DataType

import System.IO

main :: IO ()
main = run

run :: IO ()
run = do
    xs <- hGetContents stdin
    putStr "> "
    putStrLn ""
    repl (initEnv, initStates) xs

----------------------------------------------------------------------------------------------------------------
-- REPL (read - eval - print - loop)
----------------------------------------------------------------------------------------------------------------

-- read-eval-print-loop
repl :: (LambdaEnv, LambdaStates) -> String -> IO ()
repl (env, states) xs = do
    hFlush stdout
    case readSCode xs of
      Left e                  -> do
        putStrLn "" *> putStrLn ("*** Read error: ")
        putStr $ show e
        putStrLn "" *> putStrLn ("________________________________________")
        repl (env, states) (tail xs)
      Right (COMMENT _, rest) -> repl (env, states) rest
      Right (LINEBREAK, rest) -> repl (env, states) rest
      Right (EOF, _)          -> (*:) ()
      Right (EXPR sexpr, rest) -> do
          (mv, states', _) <- runInterpretE (env, states) sexpr
          case mv of
            Left err            -> showError err >> repl (env, states') rest
            Right (RETURN expr) -> do
              putStr "=> "
              putStrLn $ show expr
              repl (env, states') rest
            Right VOID          -> repl (env, states') rest
        where
          showError err = do
              putStr "*** "
              putStrLn $ show err
  
