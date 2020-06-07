{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Repl (evalAndPrint, runRepl)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \case
    []       -> runRepl
    [scheme] ->  evalAndPrint scheme
    _        -> putStrLn "Program takes only 0 or 1 argument"
