module ParserError exposing (..)

import BoolImpl exposing (Context, Problem(..))
import List
import Parser.Advanced exposing (DeadEnd)


parserError : List (DeadEnd Context Problem) -> String -> String
parserError list input =
    let
        length =
            String.length input
    in
    if length == 0 then
        "No Input"

    else
        list
            |> List.sortBy
                (\error ->
                    case error.problem of
                        ExpectingClosingBracket ->
                            error.col

                        ExpectingVariable ->
                            error.col + length

                        ExpectingOperator ->
                            error.col + length * 2

                        ExpectingEnd ->
                            error.col + length * 3

                        _ ->
                            length * 3
                )
            |> List.head
            |> Maybe.andThen
                (\error ->
                    case error.problem of
                        ExpectingClosingBracket ->
                            Just "I was expecting a closing bracket."

                        ExpectingVariable ->
                            Just "I was expecting a subterm here, e.g. a variable."

                        ExpectingOperator ->
                            Just "I was expecting an operator here."

                        ExpectingEnd ->
                            Just "I was done here and did not expect anymore. Maybe you forgot an operator?"

                        _ ->
                            Nothing
                )
            |> Maybe.withDefault "Invalid Input"
