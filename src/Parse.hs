module Parse (readExpr, readExprList) where
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Control.Monad.Except
import Control.Applicative ((<$>))

import Base

-- Returns a character if the input stream is a valid identifier symbol
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- Skips one or spaces
spaces :: Parser ()
spaces = skipMany1 space

-- Returns a String LispVal if the input stream is a valid quoted string
parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf ['"', '\\'] <|> escapedChars)
                char '"'
                return $ String x

-- Returns a character if the input stream is a valid escape sequence (e.g. \n)
escapedChars :: Parser Char
escapedChars = do
  char '\\'
  c <- oneOf ['"', 'n', 'r', 't', '\\']
  return $ case c of
    '"' -> '"'
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    '\\' -> '\\'

-- Returns a Bool or Atom LispVal if the input stream is
-- a valid boolean literal ("#t", "#f") or atom, respectively.
parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

-- Returns a Number LispVal if the input stream is a valid number
parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

-- Returns a List LispVal if the input stream is a valid list (e.g. `(1, 2, 3)`)
parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

-- Returns a DottedList LispVal if the input stream is a valid dotted list (e.g. `(1, 2 . 3)`)
parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

-- Returns a List LispVal starting with "quote" if the input stream is a valid quoted expression
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- Returns one of many possible types of LispVals if any match
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

-- Invokes the given parser on a given string,
-- returns a value (of a type inferred from the parser). Or throws an error.
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

-- Given a string containing a Scheme expression, returns a LispVal (or throws an error)
readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

-- Given a string containing multiple whitespace-separated Scheme expressions,
-- returns a list of LispVals (or throws an error)
readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)
