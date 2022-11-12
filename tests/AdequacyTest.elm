module AdequacyTest exposing (..)

import Adequacy exposing (existsAllInputNotEqInput, exsistsIsNotMonotone)
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
            , testFirstSecondConditionHelp [ Xor (Var "x") (Var "y") ] Basics.False Basics.False
            ]
        , describe "∃f ∈ X such that f (1,...,1) ≠ 1 "
            [ testFirstSecondConditionHelp [ BoolImpl.True ] Basics.True Basics.False
            , testFirstSecondConditionHelp [ BoolImpl.False ] Basics.True Basics.True
            , testFirstSecondConditionHelp [ Neg (Var "x") ] Basics.True Basics.True
            , testFirstSecondConditionHelp [ And (Var "x") (Var "y") ] Basics.True Basics.False
            , testFirstSecondConditionHelp [ Or (Var "x") (Var "y") ] Basics.True Basics.False
            , testFirstSecondConditionHelp [ Xor (Var "x") (Var "y") ] Basics.True Basics.True
            ]
        , describe "∃f ∈ X which is not monotone "
            [ monotonicityTestHelp [ BoolImpl.True ] Basics.False
            , monotonicityTestHelp [ BoolImpl.False ] Basics.False
            , monotonicityTestHelp [ Neg (Var "x") ] Basics.True
            , monotonicityTestHelp [ And (Var "x") (Var "y") ] Basics.False
            , monotonicityTestHelp [ Or (Var "x") (Var "y") ] Basics.False
            , monotonicityTestHelp [ Xor (Var "x") (Var "y") ] Basics.True
            ]
        ]


testFirstSecondConditionHelp : List Formula -> Basics.Bool -> Basics.Bool -> Test
testFirstSecondConditionHelp testset allVariables expect =
    test (testSetToString testset) <|
        \_ ->
            existsAllInputNotEqInput testset allVariables
                |> Expect.equal expect


monotonicityTestHelp : List Formula -> Basics.Bool -> Test
monotonicityTestHelp testset expect =
    test (testSetToString testset) <|
        \_ ->
            exsistsIsNotMonotone testset
                |> Expect.equal expect


testSetToString : List Formula -> String
testSetToString testset =
    "Testset: [" ++ String.slice 0 -2 (String.concat (List.map (\formula -> toString formula ++ ", ") testset)) ++ "]"
