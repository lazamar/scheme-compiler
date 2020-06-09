{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE RecordWildCards  #-}

module Lisp where

import Control.Monad.Except
import Data.Bifunctor (second)
import Data.Either (fromRight)
import Data.Foldable (foldlM, asum)
import Data.IORef
import Data.Maybe (isJust, isNothing)
import Numeric (readOct, readHex)
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Read (readMaybe)

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
    | String String
    | Bool Bool
    | Char Char
    | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
    | Func
        { params    :: [String]
        , vararg    :: (Maybe String)
        , body      :: [LispVal]
        , closure   :: Env
        }

instance Eq LispVal where
    (==) (Atom var1)        (Atom var2)         = var1 ==  var2
    (==) (List l1)          (List l2)           = l1 ==  l2
    (==) (DottedList h1 t1) (DottedList h2 t2)  = t1 == t2 && h1 == h2
    (==) (Number n1)        (Number n2)         = n1 ==  n2
    (==) (String s1)        (String s2)         = s1 ==  s2
    (==) (Bool b1)          (Bool b2)           = b1 ==  b2
    (==) (Char c1)          (Char c2)           = c1 ==  c2
    (==) (PrimitiveFunc _)  _                   = False
    (==) (Func _ _ _ _)     _                   = False
    (==) _ _                                    = False

instance Show LispVal where
    show (Atom var) = "Atom \"" <> var <> "\""
    show (List l) = "List (" <> show l <> ")"
    show (DottedList h t) = "DottedList " <> show h <> " (" <> show t <> ")"
    show (Number n) = "Number " <> show n
    show (String s) = "String \"" <> show s <> "\""
    show (Bool b) = "Bool " <> show b
    show (Char c) = "Char '" <> show c <> "'"
    show (PrimitiveFunc f) = "PrimitiveFunc <func>"
    show (Func{..}) =
        "Func { params = " <> show params
         <> ", vararg = " <> show vararg
         <> ", body = " <> show body
         <> ", closure = <closure>"


-- | This is the inverse of parsing
toScheme :: LispVal -> String
toScheme = \case
    String contents -> "\"" ++ contents ++ "\""
    Atom name       -> name
    Bool True       -> "#t"
    Bool False      -> "#f"
    Number val      -> show val
    Char val        -> "#\\" <> [val] <> ""
    List contents   -> "(" ++ unwordsList contents ++ ")"
    DottedList h t  -> "(" ++ unwordsList h ++ " . " ++ toScheme t ++ ")"
    PrimitiveFunc _ -> "<primitive>"
    Func{..}        -> "(lambda (" ++ unwords (map show params) ++ (maybe "" (" . " ++) vararg) ++ ") ...)"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map toScheme

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
            pspace   = try $ string "space" >> return ' '
            pnewline = try $ string "newline" >> return '\n'

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
parseNumber = withBase <|> try integer
    where
        withBase = do
            char '#'
            Number <$> asum
                [ char 'b' >> sign >>= \m -> m . readBinary . fmap (read . pure) <$> many1 (oneOf "01")
                , char 'o' >> sign >>= \m -> m . fst . head . readOct <$> many1 (oneOf ['0'..'7'])
                , char 'd' >> decimal
                , char 'x' >> sign >>= \m -> m . fst . head . readHex <$> many1
                    (oneOf $ ['0'..'9'] <> ['a'..'f'] <> ['A'..'F'])
                ]

        sign :: Num a => Parser (a -> a)
        sign = (char '-' >> return negate) <|> return id

        -- 10e-7
        eNotation :: Parser Integer
        eNotation = (char 'e' >> (sign <*> fmap read (many1 digit))) <|> return 1

        integer = fmap Number decimal

        decimal = do
            m <- sign
            nums <- fmap read (many1 digit)
            power <- eNotation
            return $ m nums ^ power

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

type Env = IORef [(String, IORef LispVal)]

type ThrowsError = Either LispError

type MonadLispError m = MonadError LispError m

type IOThrowsError = ExceptT LispError IO

nullEnv :: IO Env
nullEnv = newIORef []

-- | An environment with primitives defined
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (second PrimitiveFunc) primitives)

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows = either throwError return

isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    case lookup var env of
        Nothing  -> throwError $ UnboundVar "Getting an unbound variable" var
        Just ref -> liftIO $ readIORef ref

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    case lookup var env of
        Nothing  -> throwError $ UnboundVar "Setting an unbound variable" var
        Just ref -> liftIO $ writeIORef ref value
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value
        else liftIO $ do
             valueRef <- newIORef value
             env      <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef binds = readIORef envRef >>= extendEnv binds >>= newIORef
    where
        extendEnv bindings env = (++ env) <$> (traverse addBinding bindings)

        addBinding (var, value) = do
            ref <- newIORef value
            return (var, ref)


eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val = case val of
    String _ -> return val
    Number _ -> return val
    Bool _   -> return val
    Char _   -> return val
    Atom var -> getVar env var
    List [Atom "quote", v]  -> return v
    List [Atom "if", predicate, conseq, alt] -> ifFun predicate conseq alt
    List (Atom "cond" : args)                -> cond args
    List [Atom "set!", Atom var, form]       -> eval' form >>= setVar env var
    List [Atom "define", Atom var, form]     -> eval' form >>= defineVar env var
    List (Atom "eqv?" : args)                -> eqv =<< traverse eval' args
    List (Atom "eq?"  : args)                -> eqv =<< traverse eval' args
    List (Atom "equal?" : args)              -> equal =<< traverse eval' args

    List (Atom "define" : List (Atom var : params)               : body) -> makeNormalFunc env params body >>= defineVar env var
    List (Atom "define" : DottedList (Atom var : params) varargs : body) -> makeVarArgs varargs env params body >>= defineVar env var
    List (Atom "lambda" : List params                            : body) -> makeNormalFunc env params body
    List (Atom "lambda" : DottedList params varargs              : body) -> makeVarArgs varargs env params body
    List (Atom "lambda" : varargs@(Atom _)                       : body) -> makeVarArgs varargs env [] body

    List (func : args) -> join $ apply <$> eval' func <*> traverse eval' args
    DottedList h t     -> DottedList <$> (traverse eval' h) <*> (eval' t)
    _                  -> throwError $ BadSpecialForm "Unrecognised special form" val
    where
        eval' = eval env

        apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
        apply func args = case func of
            PrimitiveFunc f -> liftThrows $ f args
            Func{..}           ->
                if num params /= num args && isNothing vararg
                   then throwError $ NumArgs (num params) args
                   else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs >>= evalBody
                where
                    remainingArgs = drop (length params) args
                    num = toInteger . length
                    evalBody env' = last <$> traverse (eval env') body
                    bindVarArgs env' = case vararg of
                        Just argName -> liftIO $ bindVars env' [(argName, List $ remainingArgs)]
                        Nothing -> return env'
            _ -> throwError $ NotFunction "Trying to invoke a value that is not a function" (toScheme func)

        ifFun predicate conseq alt = eval' predicate >>= \case
            Bool False -> eval' alt
            Bool True -> eval' conseq
            arg       -> throwError $ TypeMismatch "boolean" arg


        eqv = binary $ on return Bool eqv'

        eqv' :: LispVal -> LispVal -> Bool
        eqv' a b = case (a,b) of
            (Bool arg1      , Bool arg2  )     -> arg1 == arg2
            (Number arg1    , Number arg2)     -> arg1 == arg2
            (Char arg1      , Char arg2  )     -> arg1 == arg2
            (String arg1    , String arg2)     -> arg1 == arg2
            (Atom arg1      , Atom arg2  )     -> arg1 == arg2
            (DottedList xs x, DottedList ys y) -> eqv' (List $ xs ++ [x]) (List $ ys ++ [y])
            (List arg1, List arg2) ->
                let eqvPair (x1, x2) = eqv' x1 x2
                in (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
            (_, _) -> False

        equal = binary $ \a b -> do
            primitiveEq <- or <$> traverse (unpackEqual a b) [AnyUnpacker asNum, AnyUnpacker asBool, AnyUnpacker asStr]
            let eqvEqual = eqv' a b
            return $ Bool $ primitiveEq || eqvEqual

        cond = \case
            []                   -> throwError $ Default "No clauses matched in conditional"
            [List [Atom "else", alt]] -> eval' alt
            clause:rest          -> case clause of
                List vals -> flip binary vals $ \predicate conseq -> do
                    p <- eval' predicate
                    if eqv' p (Bool True)
                       then eval' conseq
                       else cond rest

                _ -> throwError $ TypeMismatch "List" clause

        makeFunc varargs env' params body = return $ Func (map toScheme params) varargs body env'
        makeNormalFunc = makeFunc Nothing
        makeVarArgs    = makeFunc . Just . toScheme


data Unpacker m = forall a. (Eq a) => AnyUnpacker (LispVal -> m a)

data Coerced a
    = CVal a
    | CList [Coerced a]
    deriving (Eq, Show)

unpackEqual :: MonadLispError m => LispVal -> LispVal -> Unpacker m -> m Bool
unpackEqual v1 v2 (AnyUnpacker unpack)
    = flip catchError (const $ return False)
    $ (==) <$> deepUnpack v1 <*> deepUnpack v2
    where
        deepUnpack v = case v of
            Atom _         -> CVal <$> unpack v
            Number _       -> CVal <$> unpack v
            String _       -> CVal <$> unpack v
            Bool _         -> CVal <$> unpack v
            Char _         -> CVal <$> unpack v
            DottedList h t -> CList <$> traverse deepUnpack (h ++ [t])
            List [Atom "quote", val] -> deepUnpack val
            List [val]     -> deepUnpack val
            List vals      -> CList <$> traverse deepUnpack vals

primitives :: MonadLispError m => [(String, [LispVal] -> m LispVal)]
primitives =
    [ ("+"          , twoOrMore $ on asNum Number (+))
    , ("-"          , twoOrMore $ on asNum Number (-))
    , ("*"          , twoOrMore $ on asNum Number (*))
    , ("/"          , twoOrMore $ on asNum Number div)
    , ("="          , binary $ on asNum Bool (==))
    , ("<"          , binary $ on asNum Bool (<))
    , (">"          , binary $ on asNum Bool (>))
    , ("/="         , binary $ on asNum Bool (/=))
    , (">="         , binary $ on asNum Bool (>=))
    , ("<="         , binary $ on asNum Bool (<=))
    , ("&&"         , binary $ on asBool Bool (&&))
    , ("||"         , binary $ on asBool Bool (||))
    , ("string=?"   , binary $ on asStr Bool (==))
    , ("string<?"   , binary $ on asStr Bool (<))
    , ("string>?"   , binary $ on asStr Bool (>))
    , ("string<=?"  , binary $ on asStr Bool (<=))
    , ("string>=?"  , binary $ on asStr Bool (>=))
    , ("mod"        , binary $ on asNum Number mod)
    , ("quotient"   , binary $ on asNum Number quot)
    , ("remainder"  , binary $ on asNum Number rem )
    , ("string?"    , unary stringOp)
    , ("number?"    , unary numberOp)
    , ("symbol?"    , unary symbolOp)
    , ("car"        , unary car     )
    , ("cdr"        , unary cdr     )
    , ("cons"       , binary cons   )
    ]
    where

        stringOp = return . \case
            String _ -> Bool True
            _        -> Bool False

        numberOp = return . \case
            Number _ -> Bool True
            _        -> Bool False

        symbolOp = return . \case
            Atom _  -> Bool True
            _       -> Bool False

        twoOrMore :: MonadLispError m => (LispVal -> LispVal -> m LispVal) -> ([LispVal] -> m LispVal)
        twoOrMore fun = \case
            args@[]  -> throwError $ NumArgs 2 args
            args@[_] -> throwError $ NumArgs 2 args
            arg:rest -> foldlM fun arg rest

        car = \case
            List (x:_)             -> return x
            DottedList (x:_) _     -> return x
            badArg                 -> throwError $ TypeMismatch "pair" badArg

        cdr = \case
            List (_:xs)            -> return $ List xs
            DottedList (_:xs) t    -> return $ if null xs then t else DottedList xs t
            badArg                 -> throwError $ TypeMismatch "pair" badArg

        cons = \new list -> case list of
            List xs                -> return $ List (new:xs)
            DottedList xs t        -> return $ DottedList (new:xs) t
            other                  -> return $ DottedList [new] other

on :: MonadLispError m
   => (LispVal -> m a)
   -> (b -> LispVal)
   -> (a -> a -> b)
   -> (LispVal -> LispVal -> m LispVal)
on from to fun arg1 arg2 = fmap to $ fun <$> from arg1 <*> from arg2

unary :: MonadLispError m => (LispVal -> m LispVal) -> ([LispVal] -> m LispVal)
unary fun = \case
    arg:[] -> fun arg
    args   -> throwError $ NumArgs 1 args

binary :: MonadLispError m => (LispVal -> LispVal -> m LispVal) -> ([LispVal] -> m LispVal)
binary fun = \case
    arg1:arg2:[] -> fun arg1 arg2
    args   -> throwError $ NumArgs 2 args

asNum :: MonadLispError m => LispVal -> m Integer
asNum = \case
    List [n] -> asNum n
    Number n -> return n
    String s | Just n <- readMaybe s -> return  n
    val      -> throwError $ TypeMismatch "Number" val

asBool :: MonadLispError m => LispVal -> m Bool
asBool = \case
    List [n] -> asBool n
    Bool n   -> return n
    val      -> throwError $ TypeMismatch "boolean" val

asStr :: MonadLispError m => LispVal -> m String
asStr = \case
    String v -> return v
    Number v -> return $ show v
    Bool v   -> return $ show v
    val      -> throwError $ TypeMismatch "String" val

trapError :: (Show e, MonadError e m) => m String -> m String
trapError = flip catchError (return . show)

extractValue :: IOThrowsError String -> IO String
extractValue = fmap (fromRight undefined) . runExceptT . trapError
