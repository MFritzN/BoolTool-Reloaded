module AdequacyTest exposing (..)

import Adequacy exposing (existsAllInputNotEqInput)
import BoolImpl exposing (..)
import Expect exposing (..)
import Test exposing (Test, describe, test)


firstSecondCondition : Test
firstSecondCondition =
    describe "Test Adequacy Conditions"
        [ describe "∃f ∈ X such that f (0,...,0) ≠ 0 "
            [ testFirstSecondConditionHelp [ BoolImpl.True ] Basics.False Basics.True
            , testFirstSecondConditionHelp [ BoolImpl.False ] Basics.False Basics.False
            , testFirstSecondConditionHelp [ Neg (Var "x") ] Basics.False Basics.True
            , testFirstSecondConditionHelp [ And (Var "x") (Var "y") ] Basics.False Basics.False
            , testFirstSecondConditionHelp [ Or (Var "x") (Var "y") ] Basics.False Basics.False
            ]
        , describe "∃f ∈ X such that f (1,...,1) ≠ 1 "
            [ testFirstSecondConditionHelp [ BoolImpl.True ] Basics.True Basics.False
            , testFirstSecondConditionHelp [ BoolImpl.False ] Basics.True Basics.True
            , testFirstSecondConditionHelp [ Neg (Var "x") ] Basics.True Basics.True
            , testFirstSecondConditionHelp [ And (Var "x") (Var "y") ] Basics.True Basics.False
            , testFirstSecondConditionHelp [ Or (Var "x") (Var "y") ] Basics.True Basics.False
            ]
        ]


testFirstSecondConditionHelp : List Formula -> Basics.Bool -> Basics.Bool -> Test
testFirstSecondConditionHelp testset allVariables expect =
    test ("Testset: [" ++ String.slice 0 -2 (String.concat (List.map (\formula -> toString formula ++ ", ") testset)) ++ "]") <|
        \_ ->
            existsAllInputNotEqInput testset allVariables
                |> Expect.equal expect
