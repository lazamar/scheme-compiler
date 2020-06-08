module Repl where

import Control.Monad.Except
import Lisp
import System.IO

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = extractValue $ do
    parsed <- ExceptT (return $ readExpr expr)
    env <- liftIO nullEnv
    toScheme <$> eval env parsed

evalAndPrint :: String -> IO ()
evalAndPrint expr =  evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
   result <- prompt
   if predicate result
      then return ()
      else action result >> until_ predicate prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue $ trapError action
