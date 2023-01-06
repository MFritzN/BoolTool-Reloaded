module TestHelp exposing (..)

import BoolImpl exposing (..)


testFormulas : List Formula
testFormulas =
    let
        a =
            Var "a"

        b =
            Var "b"

        c =
            Var "c"

        d =
            Var "d"

        x =
            Var "x"

        y =
            Var "y"
    in
    [ Or x y
    , Or (And (Neg d) (Or (And (Neg c) a) (And c b))) (And d (Or (And (Neg a) b) (And a c)))
    , And a b
    , Impl a b
    , Xor a b
    , Neg (And a b)
    , Neg (Impl a b)
    , And (Or a b) (And (Or c d) (Or x y))
    ]
