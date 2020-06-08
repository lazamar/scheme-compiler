{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Repl (runRepl, runOne)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \case
    []       -> runRepl
    [scheme] -> runOne scheme
    _        -> putStrLn "Program takes only 0 or 1 argument"
