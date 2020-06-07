{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE ScopedTypeVariables  #-}

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
    deriving (Eq, Show)

toScheme :: LispVal -> String
toScheme = \case
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
parseNumber = withBase <|>  try float <|> try integer
    where
        withBase = do
            char '#'
            Number <$> asum
                [ char 'b' >> sign >>= \m -> m . readBinary . fmap (read . pure) <$> many1 (oneOf "01")
                , char 'o' >> sign >>= \m -> m . fst . head . readOct <$> many1 (oneOf ['0'..'7'])
                , char 'd' >> sign >>= \m -> m . read <$> many1 digit
                , char 'x' >> sign >>= \m -> m . fst . head . readHex <$> many1
                    (oneOf $ ['0'..'9'] <> ['a'..'f'] <> ['A'..'F'])
                ]

        sign :: Num a => Parser (a -> a)
        sign = (char '-' >> return negate) <|> return id

        integer = fmap Number $ sign <*> fmap read (many1 digit)

        float = fmap Float $ sign <*> do
            predot <- many digit
            char '.'
            postdot <- many1 digit
            return  $ fst . head . readFloat $ '0':predot <> "." <> postdot

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
parseList = fullList <|> emptyList
    where
        emptyList = return $ List []

        fullList  = do
            first  <- parseExpr
            others <- many (try $ spaces >> parseExpr)
            let h = first:others
            dottedList h <|> return (List h)

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
    List [Atom "if", predicate, conseq, alt] -> ifFun predicate conseq alt
    List (Atom "car"  : args)                -> car args
    List (Atom "cdr"  : args)                -> cdr args
    List (Atom "cons" : args)                -> cons args
    List (Atom "eqv?" : args)                -> eqv args
    List (Atom "eq?"  : args)                -> eqv args
    List (Atom func   : args)                -> apply func =<< traverse eval args
    List contents                            -> List <$> traverse eval contents
    DottedList h t                           -> DottedList <$> (traverse eval h) <*> (eval t)
    Atom _       -> return val
    where
        apply :: String -> [LispVal] -> ThrowsError LispVal
        apply func args = case lookup func primitives of
            Nothing -> throwError $ NotFunction "Unrecognized primitive function args" func
            Just f  -> f args

        ifFun predicate conseq alt = eval predicate >>= \case
            Bool False -> eval alt
            _          -> eval conseq

        car = unary $ \case
            List (Atom "quote":xs) -> car xs
            List (x:_)             -> return x
            DottedList (x:_) _     -> return x
            badArg                 -> throwError $ TypeMismatch "pair" badArg

        cdr = unary $ \case
            List (Atom "quote":xs) -> cdr xs
            List (_:xs)            -> return $ List xs
            DottedList (_:xs) t    -> return $ if null xs then t else DottedList xs t
            badArg                 -> throwError $ TypeMismatch "pair" badArg

        cons = binary $ \new list -> case list of
            List (Atom "quote":xs) -> cons (new:xs)
            List xs                -> return $ List (new:xs)
            DottedList xs t        -> return $ DottedList (new:xs) t
            other                  -> return $ DottedList [new] other

        eqv = binary $ on return Bool eqv'
            where
                eqv' :: LispVal -> LispVal -> Bool
                eqv' a b = case (a,b) of
                    (Bool arg1      , Bool arg2  )     -> arg1 == arg2
                    (Number arg1    , Number arg2)     -> arg1 == arg2
                    (Float arg1     , Float arg2 )     -> arg1 == arg2
                    (Char arg1      , Char arg2  )     -> arg1 == arg2
                    (String arg1    , String arg2)     -> arg1 == arg2
                    (Atom arg1      , Atom arg2  )     -> arg1 == arg2
                    (DottedList xs x, DottedList ys y) -> eqv' (List $ xs ++ [x]) (List $ ys ++ [y])
                    (List arg1, List arg2) ->
                        let eqvPair (x1, x2) = eqv' x1 x2
                        in (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
                    (_, _) -> False


primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
    [ ("+"          , twoOrMore $ on num Number (+))
    , ("-"          , twoOrMore $ on num Number (-))
    , ("*"          , twoOrMore $ on num Number (*))
    , ("/"          , twoOrMore $ on num Number div)
    , ("="          , binary $ on num Bool (==))
    , ("<"          , binary $ on num Bool (<))
    , (">"          , binary $ on num Bool (>))
    , ("/="         , binary $ on num Bool (/=))
    , (">="         , binary $ on num Bool (>=))
    , ("<="         , binary $ on num Bool (<=))
    , ("&&"         , binary $ on bool Bool (&&))
    , ("||"         , binary $ on bool Bool (||))
    , ("string=?"   , binary $ on str Bool (==))
    , ("string<?"   , binary $ on str Bool (<))
    , ("string>?"   , binary $ on str Bool (>))
    , ("string<=?"  , binary $ on str Bool (<=))
    , ("string>=?"  , binary $ on str Bool (>=))
    , ("mod"        , binary $ on num Number mod)
    , ("quotient"   , binary $ on num Number quot)
    , ("remainder"  , binary $ on num Number rem )
    , ("string?"    , unary stringOp)
    , ("number?"    , unary numberOp)
    , ("symbol?"    , unary symbolOp)
    ]
    where
        num :: LispVal -> ThrowsError Integer
        num = \case
            List [n] -> num n
            Number n -> return n
            val      -> throwError $ TypeMismatch "Number" val

        bool :: LispVal -> ThrowsError Bool
        bool = \case
            List [n] -> bool n
            Bool n   -> return n
            val      -> throwError $ TypeMismatch "Bool" val

        str :: LispVal -> ThrowsError String
        str = \case
            String v -> return v
            Number v -> return $ show v
            Bool v   -> return $ show v
            val      -> throwError $ TypeMismatch "String" val

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

        twoOrMore :: (LispVal -> LispVal -> ThrowsError LispVal) -> ([LispVal] -> ThrowsError LispVal)
        twoOrMore fun = \case
            args@[]  -> throwError $ NumArgs 2 args
            args@[_] -> throwError $ NumArgs 2 args
            arg:rest -> foldlM fun arg rest

on :: (LispVal -> ThrowsError a)
   -> (b -> LispVal)
   -> (a -> a -> b)
   -> (LispVal -> LispVal -> ThrowsError LispVal)
on from to fun arg1 arg2 = fmap to $ fun <$> from arg1 <*> from arg2

unary :: (LispVal -> ThrowsError LispVal) -> ([LispVal] -> ThrowsError LispVal)
unary fun = \case
    arg:[] -> fun arg
    args   -> throwError $ NumArgs 1 args

binary :: (LispVal -> LispVal -> ThrowsError LispVal) -> ([LispVal] -> ThrowsError LispVal)
binary fun = \case
    arg1:arg2:[] -> fun arg1 arg2
    args   -> throwError $ NumArgs 2 args

catchError :: Either a b -> (a -> Either a b) -> Either a b
catchError e f = either f return e

throwError :: e -> Either e a
throwError = Left

trapError :: Show a => Either a String -> Either a String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue = fromRight undefined
