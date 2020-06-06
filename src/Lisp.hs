{-# LANGUAGE BinaryLiterals #-}

module Lisp where

import Prelude
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.Foldable (asum)
import Numeric (readOct, readHex, readDec, readFloat)

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
    | Float Double
    | String String
    | Bool Bool
    | Char Char
    deriving (Eq, Show)

-- # Parsers

-- ## Basic types

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value: " <> show val

parseExpr :: Parser LispVal
parseExpr = parseString
        <|> try parseNumber
        <|> try parseChar
        <|> parseAtom
        <|> parseQuoted
        <|> do  char '('
                x <- parseList
                char ')'
                return x

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseChar :: Parser LispVal
parseChar = do
    string "#\\"
    fmap Char $ space <|> newline <|> anyChar
        where
            space   = string "space" >> return ' '
            newline = string "newline" >> return '\n'

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
parseNumber = withBase <|> float <|> floatOrInt
    where
        withBase = do
            char '#'
            Number <$> asum
                [ char 'b' >> readBinary . fmap (read . pure) <$> many1 (oneOf "01")
                , char 'o' >> fst . head . readOct <$> many1 (oneOf ['0'..'7'])
                , char 'd' >> read <$> many1 digit
                , char 'x' >> fst . head . readHex <$> many1
                    (oneOf $ ['0'..'9'] <> ['a'..'f'] <> ['A'..'F'])
                ]

        float = fromDot ""

        floatOrInt = do
            predot <- many1 digit
            fromDot predot <|> return (Number $ read predot)

        fromDot :: String -> Parser LispVal
        fromDot predot = do
            char '.'
            postdot <- many1 digit
            return  $ Float $ fst . head . readFloat $ '0':predot <> "." <> postdot

        readBinary :: [Integer] -> Integer
        readBinary v = go 0 (reverse v) 0
            where
                go pos []       = id
                go pos (0:rest) = go (pos + 1) rest
                go pos (1:rest) = go (pos + 1) rest . (+ 2^pos)

-- ## Recursive types

parseList :: Parser LispVal
parseList = do
    first  <- parseExpr
    others <- many (try $ spaces >> parseExpr)
    let head = first:others
    dottedList head <|> return (List head)
    where
        dottedList head = do
            tail <- spaces >> char '.' >> spaces >> parseExpr
            return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]
