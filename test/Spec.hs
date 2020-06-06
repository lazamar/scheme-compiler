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
            it "just digits" $ lispValue "10"     $ Number 10
            it "decimal"     $ lispValue "#d10"   $ Number 10
            it "binary"      $ lispValue "#b1010" $ Number 10
            it "octal"       $ lispValue "#o12"   $ Number 10
            it "hexadecimal" $ lispValue "#xA"    $ Number 10
            it "hexadecimal" $ lispValue "#xa"    $ Number 10
