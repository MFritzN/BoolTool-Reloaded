module BoolImplTest exposing (..)

import BoolImpl exposing (..)
import Dict exposing (..)
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
            , test "Xor" <|
                \_ ->
                    let
                        expectedFormula =
                            Xor (Var "a") (Xor (Var "b") (Var "c"))
                    in
                    case run BoolImpl.formula_p "a ^ b ^ c" of
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
            , test "1] ~ > ^" <|
                \_ ->
                    let
                        expectedFormula =
                            Xor (Neg (Var "a")) (Var "b")
                    in
                    case run BoolImpl.formula_p "~a ^ b" of
                        Ok formula ->
                            equals formula expectedFormula
                                |> Expect.equal Basics.True
                                |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")

                        Err _ ->
                            Expect.fail "expected the parser to succeed"
            , test "2] ~ > ^" <|
                \_ ->
                    let
                        expectedFormula =
                            Xor (Var "a") (Neg (Var "b"))
                    in
                    case run BoolImpl.formula_p "a ^ ~b" of
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
            , test "1] ^ > ->" <|
                \_ ->
                    let
                        expectedFormula =
                            Impl (Xor (Var "a") (Var "b")) (Var "c")
                    in
                    case run BoolImpl.formula_p "a ^ b -> c" of
                        Ok formula ->
                            equals formula expectedFormula
                                |> Expect.equal Basics.True
                                |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")

                        Err _ ->
                            Expect.fail "expected the parser to succeed"
            , test "2] ^ > ->" <|
                \_ ->
                    let
                        expectedFormula =
                            Impl (Var "a") (Xor (Var "b") (Var "c"))
                    in
                    case run BoolImpl.formula_p "a -> b ^ c" of
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
        , evaluateTestHelp Dict.empty BoolImpl.True Basics.True
        , evaluateTestHelp Dict.empty BoolImpl.False Basics.False
        , let
            formula =
                Impl (Var "a") (Var "b")
          in
          describe "evaluate implication"
            [ evaluateTestHelp (Dict.fromList [ ( "a", Basics.True ), ( "b", Basics.True ) ]) formula Basics.True
            , evaluateTestHelp (Dict.fromList [ ( "a", Basics.True ), ( "b", Basics.False ) ]) formula Basics.False
            , evaluateTestHelp (Dict.fromList [ ( "a", Basics.False ), ( "b", Basics.True ) ]) formula Basics.True
            , evaluateTestHelp (Dict.fromList [ ( "a", Basics.False ), ( "b", Basics.False ) ]) formula Basics.True
            ]
        , let
            formula =
                Or (Var "a") (Var "b")
          in
          describe "evaluate or"
            [ evaluateTestHelp (Dict.fromList [ ( "a", Basics.True ), ( "b", Basics.True ) ]) formula Basics.True
            , evaluateTestHelp (Dict.fromList [ ( "a", Basics.True ), ( "b", Basics.False ) ]) formula Basics.True
            , evaluateTestHelp (Dict.fromList [ ( "a", Basics.False ), ( "b", Basics.True ) ]) formula Basics.True
            , evaluateTestHelp (Dict.fromList [ ( "a", Basics.False ), ( "b", Basics.False ) ]) formula Basics.False
            ]
        , let
            formula =
                Xor (Var "a") (Var "b")
          in
          describe "evaluate xor"
            [ evaluateTestHelp (Dict.fromList [ ( "a", Basics.True ), ( "b", Basics.True ) ]) formula Basics.False
            , evaluateTestHelp (Dict.fromList [ ( "a", Basics.True ), ( "b", Basics.False ) ]) formula Basics.True
            , evaluateTestHelp (Dict.fromList [ ( "a", Basics.False ), ( "b", Basics.True ) ]) formula Basics.True
            , evaluateTestHelp (Dict.fromList [ ( "a", Basics.False ), ( "b", Basics.False ) ]) formula Basics.False
            ]
        , let
            formula =
                And (Var "a") (Var "b")
          in
          describe "evaluate and"
            [ evaluateTestHelp (Dict.fromList [ ( "a", Basics.True ), ( "b", Basics.True ) ]) formula Basics.True
            , evaluateTestHelp (Dict.fromList [ ( "a", Basics.True ), ( "b", Basics.False ) ]) formula Basics.False
            , evaluateTestHelp (Dict.fromList [ ( "a", Basics.False ), ( "b", Basics.True ) ]) formula Basics.False
            , evaluateTestHelp (Dict.fromList [ ( "a", Basics.False ), ( "b", Basics.False ) ]) formula Basics.False
            ]
        , let
            formula =
                Neg (Var "a")
          in
          describe "evaluate neg"
            [ evaluateTestHelp (Dict.singleton "a" Basics.True) formula Basics.False
            , evaluateTestHelp (Dict.singleton "a" Basics.False) formula Basics.True
            ]
        ]


evaluateTestHelp : Dict String Bool -> Formula -> Basics.Bool -> Test
evaluateTestHelp variables formula expected =
    test (formulaVariblesToString variables formula) <|
        \_ ->
            evaluate formula variables
                |> Expect.equal expected
                |> Expect.onFail
                    ("Expected this to be "
                        ++ (if expected then
                                "True but got False"

                            else
                                "False but got True"
                           )
                    )


formulaVariblesToString : Dict String Bool -> Formula -> String
formulaVariblesToString variables formula =
    formula
        |> toString
        |> String.map
            (\c ->
                Dict.get (String.fromChar c) variables
                    |> Maybe.andThen
                        (\value ->
                            if value then
                                Just 'T'

                            else
                                Just 'F'
                        )
                    |> Maybe.withDefault c
            )
