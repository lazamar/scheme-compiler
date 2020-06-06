module Lisp where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.Foldable (asum)
import Numeric (readOct, readHex, readFloat)

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
    fmap Char $ pspace <|> pnewline <|> anyChar
        where
            pspace   = string "space" >> return ' '
            pnewline = string "newline" >> return '\n'

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (escaped <|> notQuote)
    char '"'
    return $ String x
    where
        escaped = do
            char '\\'
            quote <|> pnewline <|> ptab <|> backslash

        quote     = char '"' >> return '"'
        pnewline  = char 'n' >> return '\n'
        ptab      = char 't' >> return '\t'
        backslash = char '\\' >> return '\\'
        notQuote  = noneOf "\""


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
                go :: Int -> [Integer] -> Integer -> Integer
                go pos (0:rest) = go (pos + 1) rest
                go pos (1:rest) = go (pos + 1) rest . (+ 2^pos)
                go _   []       = id
                go _ _          = error "Unexpected non-binary number"

-- ## Recursive types

parseList :: Parser LispVal
parseList = do
    first  <- parseExpr
    others <- many (try $ spaces >> parseExpr)
    let h = first:others
    dottedList h <|> return (List h)
    where
        dottedList h = do
            t <- spaces >> char '.' >> spaces >> parseExpr
            return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]
