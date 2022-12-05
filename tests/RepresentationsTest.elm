module RepresentationsTest exposing (..)

import BoolImpl exposing (..)
import Expect
import Test exposing (Test, describe, test)
import ANF exposing (calculateANF)
import ANF exposing (listToANF)


anfTestSuite : Test
anfTestSuite =
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
    describe "ANF Test"
        [ anfTestHelp (Or x y) (Xor x (Xor y (And x y)))
        , anfTestHelp (Or (And (Neg d) (Or (And (Neg c) a) (And c b))) (And d (Or (And (Neg a) b) (And a c)))) (Xor a (Xor (And a c) (Xor (And a d) (Xor (And b c) (Xor (And b d) (Xor (And a (And b d)) (And b (And c d))))))))
        ]


anfTestHelp : Formula -> Formula -> Test
anfTestHelp input expected =
    test (toString input) <|
        \_ ->
            calculateANF input
                |> listToANF
                |> equals expected
                |> Expect.equal Basics.True
                |> Expect.onFail ("Expected " ++ toString expected ++ " but got " ++ toString (listToANF (calculateANF input)))
