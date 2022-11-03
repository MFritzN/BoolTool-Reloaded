module BoolImplTest exposing (..)

import Expect exposing (..)
import Test exposing (..)
import BoolImpl exposing (..)
import Parser exposing (run)

suite : Test
suite = 
    describe "The BoolImpl Module"
        [ describe "ParserTest"
            [ describe "Associativity"
                [ test "And" <|
                \_ ->
                    let expectedFormula = And (Var "a") (And (Var "b") (Var "c"))
                    in
                    case (run BoolImpl.formula_p "a & b & c") of
                        Ok formula -> equals formula expectedFormula
                            |> Expect.equal Basics.True
                            |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ "instead")
                        Err _ -> Expect.fail  "expected the parser to succeed"
                , test "Or" <|
                \_ ->
                    let expectedFormula = Or (Var "a") (Or (Var "b") (Var "c"))
                    in
                    case (run BoolImpl.formula_p "a | b | c") of
                        Ok formula -> equals formula expectedFormula
                            |> Expect.equal Basics.True
                            |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ "instead")
                        Err _ -> Expect.fail  "expected the parser to succeed"
                , test "Impl" <|
                \_ ->
                    let expectedFormula = Impl (Var "a") (Impl (Var "b") (Var "c"))
                    in
                    case (run BoolImpl.formula_p "a -> b -> c") of
                        Ok formula -> equals formula expectedFormula
                            |> Expect.equal Basics.True
                            |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ "instead")
                        Err _ -> Expect.fail  "expected the parser to succeed"
                ]
            ]
        ]