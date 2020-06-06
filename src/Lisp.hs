module Lisp
    ( runLisp
    ) where

import Prelude
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

runLisp :: IO ()
runLisp  = do
    (expr:_) <- getArgs
    putStrLn $ "Input expression: " <> expr
    putStrLn (readExpr expr)

-- Datatypes

data LispVal
    = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    deriving (Show)

-- Parsers

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value: " <> show val

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (escaped <|> notQuote)
    char '"'
    return $ String x
    where
        escaped = do
            char '\\'
            quote <|> newline <|> tab <|> backslash

        quote     = char '"' >> return '"'
        newline   = char 'n' >> return '\n'
        tab       = char 't' >> return '\t'
        backslash = char '\\' >> return '\\'
        notQuote = noneOf "\""


parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit
