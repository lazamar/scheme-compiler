{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TupleSections  #-}
import Test.Hspec
import Test.Hspec
import Test.QuickCheck
import Lisp hiding (runLisp)
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Bifunctor (first)

runLisp :: String -> ThrowsError LispVal
runLisp str = do
    parsed <- first Parser $ parse parseExpr "lisp" str
    eval parsed

lispValue :: String -> LispVal -> Expectation
lispValue str val = runLisp str `shouldBe` Right val

lispThrows :: (LispError -> Bool) -> String -> Expectation
lispThrows onError str = first onError (runLisp str) `shouldBe` Left True

main :: IO ()
main = hspec $ do
    describe "Parses" $ do
        describe "Bool" $ do
            it "true"  $ lispValue "#t" $ Bool True
            it "false" $ lispValue "#f" $ Bool False

        describe "Char" $ do
            it "#\\space"    $ lispValue "#\\space"   $ Char ' '
            it "#\\newline"  $ lispValue "#\\newline" $ Char '\n'
            it "#\\A"        $ lispValue "#\\A"       $ Char 'A'
            it "#\\2"        $ lispValue "#\\2"       $ Char '2'
            it "#\\\\"       $ lispValue "#\\\\"      $ Char '\\'

        describe "String" $ do
            let quoted str = "\"" <> str <> "\""
            it "no ecaped values"   $ lispValue (quoted "Hello") $ String "Hello"
            it "escaped tab"        $ lispValue (quoted "\\t")   $ String "\t"
            it "escaped newline"    $ lispValue (quoted "\\n")   $ String "\n"
            it "escaped quote"      $ lispValue (quoted "\\\"")  $ String "\""
            it "escaped backslash"  $ lispValue (quoted "\\\\")  $ String "\\"

        describe "Number" $ do
            it "just digits"            $ lispValue "10"     $ Number 10
            it "decimal"                $ lispValue "#d10"   $ Number 10
            it "binary"                 $ lispValue "#b1010" $ Number 10
            it "octal"                  $ lispValue "#o12"   $ Number 10
            it "hexadecimal capital"    $ lispValue "#xA"    $ Number 10
            it "hexadecimal lower case" $ lispValue "#xa"    $ Number 10
            it "float"                  $ lispValue "1.5"    $ Float 1.5
            it "float starts with dot"  $ lispValue ".5"     $ Float 0.5

        describe "List" $ do
            it "of values" $ lispValue "(1 2 3)" $ List [Number 1, Number 2, Number 3]
            it "of lists"   $ lispValue "((2 3) (4 5))"
                $ List [List [Number 2, Number 3] , List [Number 4, Number 5]]

        describe "Dotted list" $ do
            it "of values" $ lispValue "(1 2 . 3)" $ DottedList [Number 1, Number 2] (Number 3)

        describe "Atom" $ do
            it "identifier"  $ lispValue "hello" $ Atom "hello"

    describe "Basic operations" $ do
            it "+ adds multiple numbers"            $ lispValue "(+ 1 2 3 4)"       $ Number 10
            it "* multiplies multiple numbers"      $ lispValue "(* 1 2 3 4)"       $ Number 24
            it "- subtracts multiple numbers"       $ lispValue "(- 1 2 3 4)"       $ Number (-8)
            it "/ divides multiple numbers"         $ lispValue "(/ 12 3 2)"        $ Number 2
            it "mod"                                $ lispValue "(mod 19 10)"       $ Number 9
            it "quotient of multiple numbers"       $ lispValue "(quotient 19 2)"   $ Number 9
            it "string? returns True for string"    $ lispValue "(string? \"Hi\")"  $ Bool True
            it "string? returns False for others"   $ lispValue "(string? 123)"     $ Bool False
            it "number? returns True for Number"    $ lispValue "(number? 123)"     $ Bool True
            it "number? returns True for Float"     $ lispValue "(number? .12)"     $ Bool True
            it "number? returns False for others"   $ lispValue "(number? #t)"      $ Bool False
            it "number? returns False for others"   $ lispValue "(number? #t)"      $ Bool False
            it "symbol? is True for identifiers"    $ lispValue "(symbol? hello)"   $ Bool True
            it "symbol? is False for primitives"    $ lispValue "(symbol? 123)"     $ Bool False

    describe "Evaluation errors" $ do
            it "thrown when unary functions is applied to multiple values"  $ lispThrows isNumArgs "(number? 1 2 3)"
            it "thrown when binary function is applied to multiple values"  $ lispThrows isNumArgs "(number? 1 2 3)"
            it "thrown when numeric function is applied to non-number"      $ lispThrows isTypeMismatch "(+ 1 \"Hi\")"
            it "not thrown for nested computations"                         $ lispValue "(+ 1 (+ 2 3 4))" $ Number 10
            it "thrown when unknown function is used"                       $ lispThrows isNotFunction "(what? 1 4)"

isNumArgs = \case
    NumArgs _ _     -> True
    _               -> False
isTypeMismatch = \case
    TypeMismatch _ _ -> True
    _                -> False
isParser = \case
   Parser  _        -> True
   _                -> False
isBadSpecialForm = \case
   BadSpecialForm _ _ -> True
   _                  -> False
isNotFunction = \case
   NotFunction _ _  -> True
   _                -> False
isUnboundVar = \case
   UnboundVar _ _   -> True
   _                -> False
isDefault = \case
   Default _        -> True
   _                -> False
