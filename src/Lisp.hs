{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TupleSections  #-}

module Lisp where

import Data.Either (fromRight)
import Data.Foldable (foldlM, asum)
import Numeric (readOct, readHex, readFloat)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad ((>=>))

runLisp :: IO ()
runLisp = getArgs >>= print . (readExpr >=> eval) . head

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
     Left err  -> throwError $ Parser err
     Right val -> return val

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

data LispError
    = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String
    deriving (Eq)

instance Show LispError where
    show = unwords . \case
        UnboundVar message varname  -> [message, ":", varname]
        BadSpecialForm message form -> [message, ":", show form]
        NotFunction message func    -> [message, ":", show func]
        NumArgs expected found      -> ["Expected", show expected," args; found values ", unwordsList found]
        TypeMismatch expected found -> ["Invalid type: expected", expected, ", found", show found]
        Parser parseErr             -> ["Parse error at ", show parseErr]
        Default _                   -> ["Default?"]

type ThrowsError = Either LispError

eval :: LispVal -> ThrowsError LispVal
eval val = case val of
    String _ -> return val
    Number _ -> return val
    Float _  -> return val
    Bool _   -> return val
    Char _   -> return val
    List [Atom "quote", v]  -> return v
    List (Atom func : args) -> apply func =<< traverse eval args
    List contents           -> List <$> traverse eval contents
    DottedList h t          -> DottedList <$> (traverse eval h) <*> (eval t)

    Atom _       -> return val
    where
        apply :: String -> [LispVal] -> ThrowsError LispVal
        apply func args = case lookup func primitives of
            Nothing -> throwError $ NotFunction "Unrecognized primitive function args" func
            Just f  -> f args

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
    [ ("+"          , twoOrMore $ onNumbers (+))
    , ("-"          , twoOrMore $ onNumbers (-))
    , ("*"          , twoOrMore $ onNumbers (*))
    , ("/"          , twoOrMore $ onNumbers div)
    , ("mod"        , binary $ onNumbers mod   )
    , ("quotient"   , binary $ onNumbers quot  )
    , ("remainder"  , binary $ onNumbers rem   )
    , ("string?"    , unary stringOp           )
    , ("number?"    , unary numberOp           )
    , ("symbol?"    , unary symbolOp           )
    ]
    where
        onNumbers :: (Integer -> Integer -> Integer) -> (LispVal -> LispVal -> ThrowsError LispVal)
        onNumbers fun arg1 arg2 = fmap Number $ fun <$> unpackNum arg1 <*> unpackNum arg2

        unpackNum :: LispVal -> ThrowsError Integer
        unpackNum = \case
            List [n] -> unpackNum n
            Number n -> return n
            val      -> throwError $ TypeMismatch "Number" val

        stringOp = return . \case
            String _ -> Bool True
            _        -> Bool False

        numberOp = return . \case
            Number _ -> Bool True
            Float _  -> Bool True
            _        -> Bool False

        symbolOp = return . \case
            Atom _  -> Bool True
            _       -> Bool False

        unary :: (LispVal -> ThrowsError LispVal) -> ([LispVal] -> ThrowsError LispVal)
        unary fun = \case
            arg:[] -> fun arg
            args   -> throwError $ NumArgs 1 args

        binary :: (LispVal -> LispVal -> ThrowsError LispVal) -> ([LispVal] -> ThrowsError LispVal)
        binary fun = \case
            arg1:arg2:[] -> fun arg1 arg2
            args   -> throwError $ NumArgs 2 args

        twoOrMore :: (LispVal -> LispVal -> ThrowsError LispVal) -> ([LispVal] -> ThrowsError LispVal)
        twoOrMore fun = \case
            args@[]  -> throwError $ NumArgs 2 args
            args@[_] -> throwError $ NumArgs 2 args
            arg:rest -> foldlM fun arg rest

catchError :: Either a b -> (a -> Either a b) -> Either a b
catchError e f = either f return e

throwError :: e -> Either e a
throwError = Left

trapError :: Show a => Either a String -> Either a String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue = fromRight undefined
