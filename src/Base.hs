{-# LANGUAGE ExistentialQuantification #-}

module Base (
  ThrowsError, IOThrowsError, Env, LispVal (..), LispError (..), Unpacker(..),
  liftThrows, runIOThrows, unpackNum, unpackStr, unpackBool, showVal) where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import System.IO
import Control.Monad
import Control.Monad.Except
import Data.IORef

-- Construct a custom Error type by parameterizing the `Either` type with `LispError`
type ThrowsError = Either LispError

-- Construct a new monad by adding (LispError) exceptions to the IO monad
type IOThrowsError = ExceptT LispError IO

-- A stateful <string, LispVal> map for storing variable bindings
type Env = IORef [(String, IORef LispVal)]

-- Value types
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Port Handle
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Func { params :: [String], vararg :: Maybe String,
                      body :: [LispVal], closure :: Env }

-- Error types
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

-- Lift a value from the inner IO monad to the outer IOThrowsError monad
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

-- Run an action in the outer IOThrowsError monad and return its value in the inner IO monad
-- Trap and convert any errors to strings
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)

-- Trap errors (in their string representation)
trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

-- Extract a value from the result of trapError
-- (which should never itself throw an error, so leave this undefined for the `Left` constructor)
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- Unpack integers from LispVals, coercing if possible (e.g. single-element lists)
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

-- Unpack bools from LispVals
unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

-- Unpack strings from LispVals, coercing if possible
unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

-- A generic AnyUnpacker type, useful for creating a collection of our unpack functions
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

-- String representations for LispVals
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"
showVal Func {params = args, vararg = varargs, body = body, closure = env} =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

-- String representations for LispErrors
showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                                     ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                          ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError
