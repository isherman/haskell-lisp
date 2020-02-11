module Main where

import System.IO
import System.Environment

import Repl

-- Run the REPL if no arguments are provided.
-- Evaluate and exit if a single argument is provided.
main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne args
