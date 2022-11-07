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
                            |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")
                        Err _ -> Expect.fail  "expected the parser to succeed"
                , test "Or" <|
                \_ ->
                    let expectedFormula = Or (Var "a") (Or (Var "b") (Var "c"))
                    in
                    case (run BoolImpl.formula_p "a | b | c") of
                        Ok formula -> equals formula expectedFormula
                            |> Expect.equal Basics.True
                            |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")
                        Err _ -> Expect.fail  "expected the parser to succeed"
                , test "Impl" <|
                \_ ->
                    let expectedFormula = Impl (Var "a") (Impl (Var "b") (Var "c"))
                    in
                    case (run BoolImpl.formula_p "a -> b -> c") of
                        Ok formula -> equals formula expectedFormula
                            |> Expect.equal Basics.True
                            |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")
                        Err _ -> Expect.fail  "expected the parser to succeed"
                ]
            ]
            ,describe "Binding Precedence"
                [test " ~ > & 1" <|
                \_ ->
                    let expectedFormula = And (Neg (Var "a")) (Var "b")
                    in
                    case (run BoolImpl.formula_p "~a & b") of
                        Ok formula -> equals formula expectedFormula
                            |> Expect.equal Basics.True
                            |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")
                        Err _ -> Expect.fail  "expected the parser to succeed"
                , test " ~ > & 2" <|
                \_ ->
                    let expectedFormula = And (Var "a") (Neg(Var "b"))
                    in
                    case (run BoolImpl.formula_p "a & ~b") of
                        Ok formula -> equals formula expectedFormula
                            |> Expect.equal Basics.True
                            |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")
                        Err _ -> Expect.fail  "expected the parser to succeed"
                , test " ~ > | 1" <|
                \_ ->
                    let expectedFormula = Or (Neg (Var "a")) (Var "b")
                    in
                    case (run BoolImpl.formula_p "~a | b") of
                        Ok formula -> equals formula expectedFormula
                            |> Expect.equal Basics.True
                            |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")
                        Err _ -> Expect.fail  "expected the parser to succeed"
                , test "~ > | 2" <|
                \_ ->
                    let expectedFormula = Or (Var "a") (Neg(Var "b"))
                    in
                    case (run BoolImpl.formula_p "a | ~b") of
                        Ok formula -> equals formula expectedFormula
                            |> Expect.equal Basics.True
                            |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")
                        Err _ -> Expect.fail  "expected the parser to succeed"
                , test " ~ > -> 1" <|
                \_ ->
                    let expectedFormula = Impl (Neg (Var "a")) (Var "b")
                    in
                    case (run BoolImpl.formula_p "~a -> b") of
                        Ok formula -> equals formula expectedFormula
                            |> Expect.equal Basics.True
                            |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")
                        Err _ -> Expect.fail  "expected the parser to succeed"
                , test "~ > ->  2" <|
                \_ ->
                    let expectedFormula = Impl (Var "a") (Neg(Var "b"))
                    in
                    case (run BoolImpl.formula_p "a -> ~b") of
                        Ok formula -> equals formula expectedFormula
                            |> Expect.equal Basics.True
                            |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")
                        Err _ -> Expect.fail  "expected the parser to succeed"
                , test " & > -> 1" <|
                \_ ->
                    let expectedFormula = Impl (And (Var "a") (Var "b")) (Var "c")
                    in
                    case (run BoolImpl.formula_p "a & b -> c") of
                        Ok formula -> equals formula expectedFormula
                            |> Expect.equal Basics.True
                            |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")
                        Err _ -> Expect.fail  "expected the parser to succeed"
                , test "& > ->  2" <|
                \_ ->
                    let expectedFormula = Impl (Var "a") (And (Var "b") (Var "c"))
                    in
                    case (run BoolImpl.formula_p "a -> b & c") of
                        Ok formula -> equals formula expectedFormula
                            |> Expect.equal Basics.True
                            |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")
                        Err _ -> Expect.fail  "expected the parser to succeed"
                , test " | > -> 1" <|
                \_ ->
                    let expectedFormula = Impl (Or (Var "a") (Var "b")) (Var "c")
                    in
                    case (run BoolImpl.formula_p "a | b -> c") of
                        Ok formula -> equals formula expectedFormula
                            |> Expect.equal Basics.True
                            |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")
                        Err _ -> Expect.fail  "expected the parser to succeed"
                , test "| > ->  2" <|
                \_ ->
                    let expectedFormula = Impl (Var "a") (Or (Var "b") (Var "c"))
                    in
                    case (run BoolImpl.formula_p "a -> b | c") of
                        Ok formula -> equals formula expectedFormula
                            |> Expect.equal Basics.True
                            |> Expect.onFail ("Expected Formula " ++ toString expectedFormula ++ " but got " ++ toString formula ++ " instead")
                        Err _ -> Expect.fail  "expected the parser to succeed"
                ]
        ]