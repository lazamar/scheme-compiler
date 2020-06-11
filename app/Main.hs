{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Repl (runRepl, runOne)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \case
    []   -> runRepl
    args -> runOne args
