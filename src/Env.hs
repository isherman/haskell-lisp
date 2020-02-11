module Env (nullEnv, getVar, setVar, defineVar, bindVars) where
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Control.Monad.Except
import Data.IORef
import Data.Maybe

import Base

-- An empty environment
nullEnv :: IO Env
nullEnv = newIORef []

-- Returns true if the given variable name is defined in the given environment
isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef

-- Returns the value of the given variable name in the given environment
-- Throws an error if undefined
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
            env <- liftIO $ readIORef envRef
            maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                  (liftIO . readIORef)
                  (lookup var env)

-- Sets the value of the given variable name to the given LispVal in the given environment
-- Throws an error if undefined
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
            env <- liftIO $ readIORef envRef
            maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                  (liftIO . flip writeIORef value)
                  (lookup var env)
            return value

-- Adds the given variable name bound to the given LispVal to the given environment
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

-- Adds the given set of <variable name, LispVal> pairs to the given environment
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)
