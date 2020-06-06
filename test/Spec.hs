import Test.Hspec
import Test.Hspec
import Test.QuickCheck
import Lisp
import Text.ParserCombinators.Parsec hiding (spaces)

lispValue :: String -> LispVal -> Expectation
lispValue str val =
    parse parseExpr "lisp" str `shouldBe` Right val


main :: IO ()
main = hspec $ do
    describe "Parses" $ do
        describe "Bool" $ do
            it "true" $ lispValue "#t" $ Bool True
            it "false" $ lispValue "#f" $ Bool False

        describe "String" $ do
            let quoted str = "\"" <> str <> "\""
            it "no ecaped values" $ lispValue (quoted "Hello") $ String "Hello"
            it "escaped tab" $ lispValue (quoted "\\t") $ String "\t"
            it "escaped newline" $ lispValue (quoted "\\n") $ String "\n"
            it "escaped quote" $ lispValue (quoted "\\\"") $ String "\""
            it "escaped backslash" $ lispValue (quoted "\\\\") $ String "\\"

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
