{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TupleSections  #-}

module Lisp where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.Foldable (asum)
import Numeric (readOct, readHex, readFloat)

runLisp :: IO ()
runLisp = getArgs >>= print . eval . readExpr . head

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> String $ "No match: " ++ show err
    Right val -> val

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
    deriving (Eq)

instance Show LispVal where
    show = \case
        String contents -> "\"" ++ contents ++ "\""
        Atom name       -> name
        Bool True       -> "#t"
        Bool False      -> "#f"
        Float val       -> show val
        Number val      -> show val
        Char val        -> "'" <> show val <> "'"
        List contents   -> "(" ++ unwordsList contents ++ ")"
        DottedList h t  -> "(" ++ unwordsList h ++ " . " ++ show t ++ ")"
        where
            unwordsList :: [LispVal] -> String
            unwordsList = unwords . map show

-- # Parsers

-- ## Basic types

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

-- # Evaluation

eval :: LispVal -> LispVal
eval val = case val of
    String _ -> val
    Number _ -> val
    Float _  -> val
    Bool _   -> val
    Char _   -> val
    List [Atom "quote", v]  -> v
    List (Atom func : args) -> apply func $ map eval args
    List contents           -> List $ eval <$> contents
    DottedList h t          -> DottedList (eval <$> h) (eval t)

    Atom _       -> val
    where
        apply :: String -> [LispVal] -> LispVal
        apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
    [ ("+"          , numericBinop (+))
    , ("-"          , numericBinop (-))
    , ("*"          , numericBinop (*))
    , ("/"          , numericBinop div)
    , ("mod"        , numericBinop mod)
    , ("quotient"   , numericBinop quot)
    , ("remainder"  , numericBinop rem)
    , unary "string?" stringOp
    , unary "number?" numberOp
    ]
    where
        numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
        numericBinop op params = Number $ foldl1 op $ map unpackNum params

        unpackNum :: LispVal -> Integer
        unpackNum (List [n]) = unpackNum n
        unpackNum (Number n) = n
        unpackNum (String n) =
            let parsed = reads n :: [(Integer, String)]
            in if null parsed then 0 else fst $ parsed !! 0
        unpackNum _ = 0

        stringOp (String _ ) = Bool True
        stringOp _           = Bool False

        numberOp (Number _ ) = Bool True
        numberOp (Float _ )  = Bool True
        numberOp _           = Bool False

        unary :: String -> (LispVal -> LispVal) -> (String, [LispVal] -> LispVal)
        unary op fun = (op,) $ \case
            arg:[] -> fun arg
            args   -> error $ unwords ["Wrong number of arguments to", op, ". Expected 1 argument, but was given", show (length args)]






