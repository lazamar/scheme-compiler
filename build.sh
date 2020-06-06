stack ghc -- \
    --make app/Main.hs src/Lisp.hs \
    -o scheme-parser \
    -XLambdaCase

