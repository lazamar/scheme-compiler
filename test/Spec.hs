import Test.Hspec
import Test.Hspec
import Test.QuickCheck
import Lisp
import Text.ParserCombinators.Parsec hiding (spaces)

lispValue :: String -> LispVal -> Expectation
lispValue str val =
    (eval <$> parse parseExpr "lisp" str) `shouldBe` Right val


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
            it "mod multiple numbers"               $ lispValue "(mod 19 10 5)"     $ Number 4
            it "quotient of multiple numbers"       $ lispValue "(quotient 19 2 2)" $ Number 4
            it "string? returns True for string"    $ lispValue "(string? \"Hi\")"  $ Bool True
            it "string? returns False for others"   $ lispValue "(string? 123)"     $ Bool False
            it "number? returns True for Number"    $ lispValue "(number? 123)"     $ Bool True
            it "number? returns True for Float"     $ lispValue "(number? .12)"     $ Bool True
            it "number? returns False for others"   $ lispValue "(number? #t)"      $ Bool False
            it "symbol? is True for identifiers"    $ lispValue "(symbol? hello)"   $ Bool True
            it "symbol? is False for primitives"    $ lispValue "(symbol? 123)"     $ Bool False
