{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TupleSections  #-}
{-# OPTIONS_GHC -Wno-orphans  #-}

module Main where

import Control.Monad.Except
import Test.Hspec
import Test.QuickCheck (oneof, Arbitrary(..), property, listOf, listOf1, elements, sized)
import Lisp
import Text.ParserCombinators.Parsec hiding (spaces, oneOf)
import Data.Bifunctor (first)
import System.IO.Unsafe (unsafePerformIO)

runLisp' :: String -> ThrowsError LispVal
runLisp' str = do
    parsed <- first Parser $ parse parseExpr "lisp" str
    unsafePerformIO $ runExceptT $ do
        env <- liftIO primitiveBindings
        eval env parsed

lispValue :: String -> LispVal -> Expectation
lispValue str val = runLisp' str `shouldBe` Right val

lispParse :: String -> LispVal -> Expectation
lispParse str val = (first Parser $ parse parseExpr "lisp" str) `shouldBe` Right val

lispThrows ::  String -> String -> Expectation
lispThrows str err = first errorConstructor (runLisp' str) `shouldBe` Left err
    where
        errorConstructor = \case
            NumArgs _ _         -> isNumArgs
            TypeMismatch _ _    -> isTypeMismatch
            Parser _            -> isParser
            BadSpecialForm _ _  -> isBadSpecialForm
            NotFunction _ _     -> isNotFunction
            UnboundVar _ _      -> isUnboundVar
            Default _           -> isDefault

isNumArgs        = "NumArgs"
isTypeMismatch   = "TypeMismatch"
isParser         = "Parser"
isBadSpecialForm = "BadSpecialForm"
isNotFunction    = "NotFunction"
isUnboundVar     = "UnboundVar"
isDefault        = "Default"

-- | Gets the name of the constructor
lispConstructor ::  LispVal -> String
lispConstructor = \case
    Atom _          -> "Atom"
    List  _         -> "List"
    DottedList _ _  -> "DottedList"
    Number _        -> "Number"
    String _        -> "String"
    Bool _          -> "Bool"
    Char _          -> "Char"

main :: IO ()
main = hspec $ do
    describe "Parses" $ do
        describe "Bool" $ do
            it "true"  $ lispParse "#t" $ Bool True
            it "false" $ lispParse "#f" $ Bool False

        describe "Char" $ do
            it "#\\space"    $ lispParse "#\\space"   $ Char ' '
            it "#\\newline"  $ lispParse "#\\newline" $ Char '\n'
            it "#\\A"        $ lispParse "#\\A"       $ Char 'A'
            it "#\\2"        $ lispParse "#\\2"       $ Char '2'
            it "#\\\\"       $ lispParse "#\\\\"      $ Char '\\'
            it "#\\s"        $ lispParse "#\\s"       $ Char 's'
            it "#\\n"        $ lispParse "#\\n"       $ Char 'n'

        describe "String" $ do
            let quoted str = "\"" <> str <> "\""
            it "no ecaped values"   $ lispParse (quoted "Hello") $ String "Hello"
            it "escaped tab"        $ lispParse (quoted "\\t")   $ String "\t"
            it "escaped newline"    $ lispParse (quoted "\\n")   $ String "\n"
            it "escaped quote"      $ lispParse (quoted "\\\"")  $ String "\""
            it "escaped backslash"  $ lispParse (quoted "\\\\")  $ String "\\"

        describe "Number" $ do
            it "just digits"            $ lispParse "10"     $ Number 10
            it "decimal"                $ lispParse "#d10"   $ Number 10
            it "binary"                 $ lispParse "#b1010" $ Number 10
            it "octal"                  $ lispParse "#o12"   $ Number 10
            it "hexadecimal capital"    $ lispParse "#xA"    $ Number 10
            it "hexadecimal lower case" $ lispParse "#xa"    $ Number 10

        describe "List" $ do
            it "of values"  $ lispParse "'(1 2 3)" $ List [Atom "quote", List [Number 1, Number 2, Number 3]]
            it "empty"      $ lispParse "'()"      $ List [Atom "quote", List []]
            xit "throws on unquoted empty list"   $ lispThrows "()" isParser
            it "of lists"   $ lispParse "'('(2 3) '(4 5))"
                $ List  [ Atom "quote"
                        , List  [ List [Atom "quote", List [Number 2, Number 3]]
                                , List [Atom "quote", List [Number 4, Number 5]]
                                ]
                        ]

        describe "Dotted list" $ do
            it "of values" $ lispParse "(1 2 . 3)" $ DottedList [Number 1, Number 2] (Number 3)

        describe "Atom" $ do
            it "identifier"  $ lispParse "hello" $ Atom "hello"

        describe "toScheme" $ do
            it "is the inverse of readExpr" $ property $ \x -> readExpr (toScheme x) `shouldBe` Right x

    describe "Evaluates" $ do
        describe "if statement" $ do
            it "evaluates second argument if true"            $ lispValue "(if #t 1 2)" $ Number 1
            it "evaluates third argument if false"            $ lispValue "(if #f 1 2)" $ Number 2
            it "errors with non-boolean values as predicate"  $ lispThrows "(if 5 1 2)" isTypeMismatch

        describe "cond statement" $ do
            it "matches first clause"                  $ lispValue "(cond (#t 2))"          $ Number 2
            it "matches second clause"                 $ lispValue "(cond (#f 1) (#t 2))"   $ Number 2
            it "evaluates predicates"                  $ lispValue "(cond ((eqv? 1 (- 5 4)) 1) (#t 2))" $ Number 1
            it "falls to else if no predicate is true" $ lispValue "(cond (#f 1) (else 2))" $ Number 2
            it "throws it there is no match or else "  $ lispThrows "(cond (#f 1))"         $ isDefault

        describe "List" $ do
            describe "car" $ do
                it "on multiple element list" $ lispValue "(car '(1 2 3))"    $ Number 1
                it "on one element list"      $ lispValue "(car '(1))"        $ Number 1
                it "on dotted list"           $ lispValue "(car '(1 2 . 3))"  $ Number 1
                it "errors if applied to non-list"              $ lispThrows  "(car '1)"    isTypeMismatch
                it "errors if applied to wrong number of args"  $ lispThrows  "(car '1 '2)" isNumArgs

            describe "cdr" $ do
                it "returns tail on list with multiple items"           $ lispValue "(cdr '(a b c))"   $ List [Atom "b", Atom "c"]
                it "returns tail on list with two items"                $ lispValue "(cdr '(a b))"     $ List [Atom "b"]
                it "returns empty list on list with one item"           $ lispValue "(cdr '(a))"       $ List []
                it "returns last on dotted list with two items"         $ lispValue "(cdr '(a . b))"   $ Atom "b"
                it "returns dotted list on dotted list witht two items" $ lispValue "(cdr '(a b . c))" $ DottedList [Atom "b"] (Atom "c")
                it "throws if arg is not list"                          $ lispThrows "(cdr 'a)"        isTypeMismatch
                it "throws if arg is empty list"                        $ lispThrows "(cdr '())"       isTypeMismatch
                it "throws if given more arguments "                    $ lispThrows "(cdr 'a 'b)"     isNumArgs

            describe "cons" $ do
                it "cons item to list"              $ lispValue "(cons 1 '(2 3))"   $ List [Number 1, Number 2, Number 3]
                it "cons item to empty list"        $ lispValue "(cons 1 '())"      $ List [Number 1]
                it "cons item to dotted list"       $ lispValue "(cons 1 '(2 . 3))" $ DottedList [Number 1, Number 2] (Number 3)
                it "cons item to non-list"          $ lispValue "(cons 1 2)"        $ DottedList [Number 1] (Number 2)
                it "throws if given more arguments" $ lispThrows "(cons 1 2 3)"     isNumArgs

        describe "Equality" $ do
            describe "strong" $ do
                it "recognises equal values" $ property $ \x -> lispValue (call "eqv?" [x, x]) $ Bool True
                it "doesn't say values with different constructors are equal"
                    $ property $ \x y -> lispValue (call "eqv?" [x, y]) $ Bool $ x == y

            describe "weak" $ do
                it "recognises equal values" $ property $ \x -> lispValue (call "equal?" [x, x]) $ Bool True
                it "coerces singleton lists to element" $ lispValue "(equal? '(2) 2)"            $ Bool True
                it "coerces strings to numbers"         $ lispValue "(equal? \"2\" 2)"           $ Bool True
                it "performs deep coercion"             $ lispValue "(equal? '(1 \"2\") '(1 2))" $ Bool True
                it "performs deep of nested lists"      $ lispValue "(equal? '('('(1)) \"2\") '(1 '('(2))))" $ Bool True
                it "recognises unequal types"           $ lispValue "(equal? 1 \"2\")"          $ Bool False
                it "recognises unequal values"          $ lispValue "(equal? 1 2)"              $ Bool False

        describe "define" $ do
            it "returns the defined value"      $ lispValue "(define a 2)"                         $ Number 2
            it "allows us to use the value"     $ lispValue "(+ (define a 2) a)"                   $ Number 4
            it "allows function definition"     $ lispValue "((define (f a b) (+ a b)) 2 3)"       $ Number 5
            it "add functions to environment"   $ lispValue "((define (f a b) (+ a b)) 2 (f 1 2))" $ Number 5
            it "doesn't do crazy recursion"     $ lispThrows "(define a a)" isUnboundVar
            it "expects only two arguments"     $ lispThrows "(define a 2 3)" isUnboundVar
            it "throws on wrong argument count" $ lispThrows "((define (f a b) (a b)) 2 3 4)" isNumArgs

        describe "lambda" $ do
            it "functions"                      $ lispValue "((lambda (a b) (+ a b)) 2 3)"     $ Number 5
            it "polyvariadic functions"         $ lispValue "((lambda (a . b) b) 2 3 4)"       $ List [Number 3, Number 4]
            it "polyvariadic functions no args" $ lispValue "((lambda args (cdr args)) 2 3 4)" $ List [Number 3, Number 4]
            it "throws on wrong argument count" $ lispThrows "((lambda (a b) (a b)) 2 3 4)" isNumArgs

        describe "Primitive operations" $ do
            it "+ adds multiple numbers"            $ lispValue "(+ 1 2 3 4)"       $ Number 10
            it "* multiplies multiple numbers"      $ lispValue "(* 1 2 3 4)"       $ Number 24
            it "- subtracts multiple numbers"       $ lispValue "(- 1 2 3 4)"       $ Number (-8)
            it "/ divides multiple numbers"         $ lispValue "(/ 12 3 2)"        $ Number 2
            it "mod"                                $ lispValue "(mod 19 10)"       $ Number 9
            it "quotient of multiple numbers"       $ lispValue "(quotient 19 2)"   $ Number 9
            it "string? returns True for string"    $ lispValue "(string? \"Hi\")"  $ Bool True
            it "string? returns False for others"   $ lispValue "(string? 123)"     $ Bool False
            it "number? returns True for Number"    $ lispValue "(number? 123)"     $ Bool True
            it "number? returns False for others"   $ lispValue "(number? #t)"      $ Bool False
            it "number? returns False for others"   $ lispValue "(number? #t)"      $ Bool False
            it "symbol? is True for identifiers"    $ lispValue "(symbol? 'hello)"   $ Bool True
            it "symbol? is False for primitives"    $ lispValue "(symbol? 123)"     $ Bool False
            it "= is True for equal nums"           $ lispValue "(= 2 2)"           $ Bool True
            it "= is False for different nums"      $ lispValue "(= 2 3)"           $ Bool False
            it "< handles true case"                $ lispValue "(< 1 2)"           $ Bool True
            it "> handles true case"                $ lispValue "(> 2 1)"           $ Bool True
            it "/= handles true case"               $ lispValue "(/= 1 2)"          $ Bool True
            it ">= handles true case"               $ lispValue "(>= 2 2)"          $ Bool True
            it "<= handles true case"               $ lispValue "(<= 2 2)"          $ Bool True
            it "&& handles true case"               $ lispValue "(&& #t #t)"        $ Bool True
            it "|| handles true case"               $ lispValue "(|| #t #f)"        $ Bool True
            it "string=? handles true case"         $ lispValue "(string=? \"hi\" \"hi\")"  $ Bool True
            it "string<? handles true case"         $ lispValue "(string<? \"a\" \"b\")"    $ Bool True
            it "string>? handles true case"         $ lispValue "(string>? \"b\" \"a\")"    $ Bool True
            it "string<=? handles true case"        $ lispValue "(string<=? \"hi\" \"hi\")" $ Bool True
            it "string>=? handles true case"        $ lispValue "(string>=? \"hi\" \"hi\")" $ Bool True

    describe "Evaluation errors" $ do
            it "thrown when unary functions is applied to multiple values"  $ lispThrows "(number? 1 2 3)" isNumArgs
            it "thrown when binary function is applied to multiple values"  $ lispThrows "(number? 1 2 3)" isNumArgs
            it "thrown when numeric function is applied to non-number"      $ lispThrows "(+ 1 \"Hi\")"    isTypeMismatch
            it "thrown when unknown function is used"                       $ lispThrows "(what? 1 4)"     isUnboundVar
            it "not thrown for nested computations"                         $ lispValue "(+ 1 (+ 2 3 4))" $ Number 10


-- | Produce scheme code to call a function with given args
call :: String -> [LispVal] -> String
call fname args = toScheme $ List $ Atom fname : args

instance Arbitrary LispVal where
    arbitrary = sized lisp
        where
            lisp depth =
                if depth == 0
                   then oneof nonRecursive
                   else oneof $ recursive <> nonRecursive
                where
                    next = lisp $ (min depth maxDepth) - 1

                    maxDepth = 2

                    recursive =
                        [ (\v -> List [Atom "quote", List v]) <$> listOf next
                        , DottedList <$> listOf1 next <*> next
                        ]
                    nonRecursive =
                        [ Number <$> arbitrary
                        , String  <$> listOf charsAllowed
                        , Bool <$> arbitrary
                        , Char <$> charsAllowed

                        -- No atoms to avoid random unbound variables
                        --, Atom <$> ((:) <$> letter <*> listOf (oneof [ letter, digit ]))
                        ]

                    letter = elements $ ['a'..'z'] <> ['A'..'Z']
                    digit  = elements ['0'..'9']

                    charsAllowed = elements $ filter (not . (`elem` "\\")) $ take 79 ['0'..]

crop depth val = if depth > 3 then Atom "end" else case val of
    List args -> List $ c <$> take 3 args
    DottedList h t  -> DottedList (c <$> take 3 h) (c t)
    Number _        -> val
    String  _       -> val
    Bool _          -> val
    Char _          -> val
    Atom _          -> val
    where
        c = crop newDepth
        newDepth = depth + 1




