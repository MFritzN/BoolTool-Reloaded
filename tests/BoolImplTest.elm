module BoolImplTest exposing (..)

import BoolImpl exposing (..)
import Dict
import Expect exposing (..)
import Parser exposing (run)
import Set
import Test exposing (..)


parserSuite : Test
parserSuite =
    describe "ParserTest"
        [ describe "Associativity"
            [ test "And" <|
                \_ ->
                    let
                        expectedFormula =
                            And (Var "a") (And (Var "b") (Var "c"))
                    in
                    case run BoolImpl.formula_p "a & b & c" of
                        Ok formula ->
                            equals formula expectedFormula
                                |> Expect.equal Basics.True
                                |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")

                        Err _ ->
                            Expect.fail "expected the parser to succeed"
            , test "Or" <|
                \_ ->
                    let
                        expectedFormula =
                            Or (Var "a") (Or (Var "b") (Var "c"))
                    in
                    case run BoolImpl.formula_p "a | b | c" of
                        Ok formula ->
                            equals formula expectedFormula
                                |> Expect.equal Basics.True
                                |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")

                        Err _ ->
                            Expect.fail "expected the parser to succeed"
            , test "Impl" <|
                \_ ->
                    let
                        expectedFormula =
                            Impl (Var "a") (Impl (Var "b") (Var "c"))
                    in
                    case run BoolImpl.formula_p "a -> b -> c" of
                        Ok formula ->
                            equals formula expectedFormula
                                |> Expect.equal Basics.True
                                |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")

                        Err _ ->
                            Expect.fail "expected the parser to succeed"
            ]
        , describe "Binding Precedence"
            [ test "1] ~ > &" <|
                \_ ->
                    let
                        expectedFormula =
                            And (Neg (Var "a")) (Var "b")
                    in
                    case run BoolImpl.formula_p "~a & b" of
                        Ok formula ->
                            equals formula expectedFormula
                                |> Expect.equal Basics.True
                                |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")

                        Err _ ->
                            Expect.fail "expected the parser to succeed"
            , test "2] ~ > &" <|
                \_ ->
                    let
                        expectedFormula =
                            And (Var "a") (Neg (Var "b"))
                    in
                    case run BoolImpl.formula_p "a & ~b" of
                        Ok formula ->
                            equals formula expectedFormula
                                |> Expect.equal Basics.True
                                |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")

                        Err _ ->
                            Expect.fail "expected the parser to succeed"
            , test "1] ~ > |" <|
                \_ ->
                    let
                        expectedFormula =
                            Or (Neg (Var "a")) (Var "b")
                    in
                    case run BoolImpl.formula_p "~a | b" of
                        Ok formula ->
                            equals formula expectedFormula
                                |> Expect.equal Basics.True
                                |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")

                        Err _ ->
                            Expect.fail "expected the parser to succeed"
            , test "2] ~ > |" <|
                \_ ->
                    let
                        expectedFormula =
                            Or (Var "a") (Neg (Var "b"))
                    in
                    case run BoolImpl.formula_p "a | ~b" of
                        Ok formula ->
                            equals formula expectedFormula
                                |> Expect.equal Basics.True
                                |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")

                        Err _ ->
                            Expect.fail "expected the parser to succeed"
            , test "1] ~ > ->" <|
                \_ ->
                    let
                        expectedFormula =
                            Impl (Neg (Var "a")) (Var "b")
                    in
                    case run BoolImpl.formula_p "~a -> b" of
                        Ok formula ->
                            equals formula expectedFormula
                                |> Expect.equal Basics.True
                                |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")

                        Err _ ->
                            Expect.fail "expected the parser to succeed"
            , test "2] ~ > ->" <|
                \_ ->
                    let
                        expectedFormula =
                            Impl (Var "a") (Neg (Var "b"))
                    in
                    case run BoolImpl.formula_p "a -> ~b" of
                        Ok formula ->
                            equals formula expectedFormula
                                |> Expect.equal Basics.True
                                |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")

                        Err _ ->
                            Expect.fail "expected the parser to succeed"
            , test "1] & > ->" <|
                \_ ->
                    let
                        expectedFormula =
                            Impl (And (Var "a") (Var "b")) (Var "c")
                    in
                    case run BoolImpl.formula_p "a & b -> c" of
                        Ok formula ->
                            equals formula expectedFormula
                                |> Expect.equal Basics.True
                                |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")

                        Err _ ->
                            Expect.fail "expected the parser to succeed"
            , test "2] & > ->" <|
                \_ ->
                    let
                        expectedFormula =
                            Impl (Var "a") (And (Var "b") (Var "c"))
                    in
                    case run BoolImpl.formula_p "a -> b & c" of
                        Ok formula ->
                            equals formula expectedFormula
                                |> Expect.equal Basics.True
                                |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")

                        Err _ ->
                            Expect.fail "expected the parser to succeed"
            , test "1] | > ->" <|
                \_ ->
                    let
                        expectedFormula =
                            Impl (Or (Var "a") (Var "b")) (Var "c")
                    in
                    case run BoolImpl.formula_p "a | b -> c" of
                        Ok formula ->
                            equals formula expectedFormula
                                |> Expect.equal Basics.True
                                |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")

                        Err _ ->
                            Expect.fail "expected the parser to succeed"
            , test "2] | > ->" <|
                \_ ->
                    let
                        expectedFormula =
                            Impl (Var "a") (Or (Var "b") (Var "c"))
                    in
                    case run BoolImpl.formula_p "a -> b | c" of
                        Ok formula ->
                            equals formula expectedFormula
                                |> Expect.equal Basics.True
                                |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")

                        Err _ ->
                            Expect.fail "expected the parser to succeed"
            ]
        ]


evaluationSuite : Test
evaluationSuite =
    describe "Evaluation"
        [ test "1] getVariables" <|
            \_ ->
                let
                    expectedVariables =
                        Set.fromList [ "a", "b" ]

                    formula =
                        And (Var "a") (Var "b")
                in
                Expect.equal (getVariables formula) expectedVariables
        , test "2] getVariables - multiple used vars" <|
            \_ ->
                let
                    expectedVariables =
                        Set.fromList [ "a" ]

                    formula =
                        And (Var "a") (Var "a")
                in
                Expect.equal (getVariables formula) expectedVariables
        , test "evaluate true" <|
            \_ ->
                case evaluate BoolImpl.True Dict.empty of
                    Ok result ->
                        result
                            |> Expect.equal Basics.True

                    Err err ->
                        Expect.fail ("This evaluation should have suceeded but it terminated with this Error: " ++ err)
        , test "evaluate false" <|
            \_ ->
                case evaluate BoolImpl.False Dict.empty of
                    Ok result ->
                        result
                            |> Expect.equal Basics.False

                    Err err ->
                        Expect.fail ("This evaluation should have suceeded but it terminated with this Error: " ++ err)
        , describe "evaluate implication"
            [ test "T -> T" <|
                \_ ->
                    let
                        variables =
                            Dict.fromList [ ( "a", Basics.True ), ( "b", Basics.True ) ]

                        formula =
                            Impl (Var "a") (Var "b")
                    in
                    case evaluate formula variables of
                        Ok result ->
                            result
                                |> Expect.equal Basics.True

                        Err err ->
                            Expect.fail ("This evaluation should have suceeded but it terminated with this Error: " ++ err)
            , test "T -> F" <|
                \_ ->
                    let
                        variables =
                            Dict.fromList [ ( "a", Basics.True ), ( "b", Basics.False ) ]

                        formula =
                            Impl (Var "a") (Var "b")
                    in
                    case evaluate formula variables of
                        Ok result ->
                            result
                                |> Expect.equal Basics.False

                        Err err ->
                            Expect.fail ("This evaluation should have suceeded but it terminated with this Error: " ++ err)
            , test "F -> T" <|
                \_ ->
                    let
                        variables =
                            Dict.fromList [ ( "a", Basics.False ), ( "b", Basics.True ) ]

                        formula =
                            Impl (Var "a") (Var "b")
                    in
                    case evaluate formula variables of
                        Ok result ->
                            result
                                |> Expect.equal Basics.True

                        Err err ->
                            Expect.fail ("This evaluation should have suceeded but it terminated with this Error: " ++ err)
            , test "F -> F" <|
                \_ ->
                    let
                        variables =
                            Dict.fromList [ ( "a", Basics.False ), ( "b", Basics.False ) ]

                        formula =
                            Impl (Var "a") (Var "b")
                    in
                    case evaluate formula variables of
                        Ok result ->
                            result
                                |> Expect.equal Basics.True

                        Err err ->
                            Expect.fail ("This evaluation should have suceeded but it terminated with this Error: " ++ err)
            ]
        , describe "evaluate or"
            [ test "T | T" <|
                \_ ->
                    let
                        variables =
                            Dict.fromList [ ( "a", Basics.True ), ( "b", Basics.True ) ]

                        formula =
                            Or (Var "a") (Var "b")
                    in
                    case evaluate formula variables of
                        Ok result ->
                            result
                                |> Expect.equal Basics.True

                        Err err ->
                            Expect.fail ("This evaluation should have suceeded but it terminated with this Error: " ++ err)
            , test "T | F" <|
                \_ ->
                    let
                        variables =
                            Dict.fromList [ ( "a", Basics.True ), ( "b", Basics.False ) ]

                        formula =
                            Or (Var "a") (Var "b")
                    in
                    case evaluate formula variables of
                        Ok result ->
                            result
                                |> Expect.equal Basics.True

                        Err err ->
                            Expect.fail ("This evaluation should have suceeded but it terminated with this Error: " ++ err)
            , test "F | T" <|
                \_ ->
                    let
                        variables =
                            Dict.fromList [ ( "a", Basics.False ), ( "b", Basics.True ) ]

                        formula =
                            Or (Var "a") (Var "b")
                    in
                    case evaluate formula variables of
                        Ok result ->
                            result
                                |> Expect.equal Basics.True

                        Err err ->
                            Expect.fail ("This evaluation should have suceeded but it terminated with this Error: " ++ err)
            , test "F | F" <|
                \_ ->
                    let
                        variables =
                            Dict.fromList [ ( "a", Basics.False ), ( "b", Basics.False ) ]

                        formula =
                            Or (Var "a") (Var "b")
                    in
                    case evaluate formula variables of
                        Ok result ->
                            result
                                |> Expect.equal Basics.False

                        Err err ->
                            Expect.fail ("This evaluation should have suceeded but it terminated with this Error: " ++ err)
            ]
        , describe "evaluate and"
            [ test "T & T" <|
                \_ ->
                    let
                        variables =
                            Dict.fromList [ ( "a", Basics.True ), ( "b", Basics.True ) ]

                        formula =
                            And (Var "a") (Var "b")
                    in
                    case evaluate formula variables of
                        Ok result ->
                            result
                                |> Expect.equal Basics.True

                        Err err ->
                            Expect.fail ("This evaluation should have suceeded but it terminated with this Error: " ++ err)
            , test "T & F" <|
                \_ ->
                    let
                        variables =
                            Dict.fromList [ ( "a", Basics.True ), ( "b", Basics.False ) ]

                        formula =
                            And (Var "a") (Var "b")
                    in
                    case evaluate formula variables of
                        Ok result ->
                            result
                                |> Expect.equal Basics.False

                        Err err ->
                            Expect.fail ("This evaluation should have suceeded but it terminated with this Error: " ++ err)
            , test "F & T" <|
                \_ ->
                    let
                        variables =
                            Dict.fromList [ ( "a", Basics.False ), ( "b", Basics.True ) ]

                        formula =
                            And (Var "a") (Var "b")
                    in
                    case evaluate formula variables of
                        Ok result ->
                            result
                                |> Expect.equal Basics.False

                        Err err ->
                            Expect.fail ("This evaluation should have suceeded but it terminated with this Error: " ++ err)
            , test "F & F" <|
                \_ ->
                    let
                        variables =
                            Dict.fromList [ ( "a", Basics.False ), ( "b", Basics.False ) ]

                        formula =
                            And (Var "a") (Var "b")
                    in
                    case evaluate formula variables of
                        Ok result ->
                            result
                                |> Expect.equal Basics.False

                        Err err ->
                            Expect.fail ("This evaluation should have suceeded but it terminated with this Error: " ++ err)
            ]
        , describe "evaluate not"
            [ test "~T" <|
                \_ ->
                    let
                        variables =
                            Dict.singleton "a" Basics.True

                        formula =
                            Neg (Var "a")
                    in
                    case evaluate formula variables of
                        Ok result ->
                            result
                                |> Expect.equal Basics.False

                        Err err ->
                            Expect.fail ("This evaluation should have suceeded but it terminated with this Error: " ++ err)
            , test "~F" <|
                \_ ->
                    let
                        variables =
                            Dict.singleton "a" Basics.False

                        formula =
                            Neg (Var "a")
                    in
                    case evaluate formula variables of
                        Ok result ->
                            result
                                |> Expect.equal Basics.True

                        Err err ->
                            Expect.fail ("This evaluation should have suceeded but it terminated with this Error: " ++ err)
            ]
        ]
