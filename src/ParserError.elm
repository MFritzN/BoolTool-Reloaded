module ParserError exposing (..)

import BoolImpl exposing (Context, Problem(..))
import Html exposing (Html, div, section, span, text)
import Html.Attributes exposing (align, attribute, class, property)
import List
import Parser.Advanced exposing (DeadEnd)


parserError : List (DeadEnd Context Problem) -> String -> Html a
parserError list input =
    let
        length =
            String.length input
    in
    if length == 0 then
        section [] [ text "No Input" ]

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
                            Just <| addMessageToRecord error "I was expecting a closing bracket."

                        ExpectingVariable ->
                            Just <| addMessageToRecord error "I was expecting a subterm here, e.g. a variable."

                        ExpectingOperator ->
                            Just <| addMessageToRecord error "I was expecting an operator here."

                        ExpectingEnd ->
                            Just <| addMessageToRecord error "I was done here and did not expect anymore. Maybe you forgot an operator?"

                        _ ->
                            Nothing
                )
            |> (\mError ->
                    case mError of
                        Just error ->
                            div []
                                [ div [ class "progress-ww" ]
                                    [ div [ class "wavy" ] [ span [] [ text <| String.dropRight (length - error.column + 1) input ], text <| String.slice (error.column - 1) error.column input, span [] [ text <| String.dropLeft error.column input ] ]
                                    , div [] [ span [] [], text "⬆", span [] [] ]
                                    , div [] [ span [] [], text error.message, span [] [] ]
                                    ]
                                ]

                        Nothing ->
                            section [] [ text "Invalid Input" ]
               )


addMessageToRecord : DeadEnd Context Problem -> String -> { column : Int, message : String, problem : Problem }
addMessageToRecord error message =
    { column = error.col, message = message, problem = error.problem }
