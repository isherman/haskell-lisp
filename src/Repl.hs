module Repl (runOne, runRepl) where
import System.IO
import Control.Monad

import Base
import Parse
import Eval
import Env
import Primitives

-- Print a string and immediately flush it
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- Print a prompt and collect user input
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- Parse and evaluate an input string in the provided environment
-- Return a string representation of the eval'ed value (or error, if an error was thrown).
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

-- Eval an input string and print it
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- A generic loop, parameterized by
--   a prompt, that returns a value 'a'
--   a predicate, that has the effect of breaking out of the loop if its evaluation of 'a' is True
--   an action to evaluate on 'a' if the predicate is false
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action

-- Evaluate a single expression
runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    runIOThrows (show <$> eval env (List [Atom "load", String (head args)]))
        >>= hPutStrLn stderr

-- The REPL. Loops until user enters "quit".
runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint
