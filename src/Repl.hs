module Repl where

import Control.Monad.Except
import Lisp
import System.IO

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = extractValue $ do
    parsed <- ExceptT (return $ readExpr expr)
    toScheme <$> eval env parsed

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
   result <- prompt
   if predicate result
      then return ()
      else action result >> until_ predicate prompt action

runRepl :: IO ()
runRepl = do
    env <- primitiveBindings
    until_ (== "quit") (readPrompt "Lisp>>> ") (evalAndPrint env)

runOne :: String -> IO ()
runOne str = primitiveBindings >>= flip evalAndPrint str

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue $ trapError action
