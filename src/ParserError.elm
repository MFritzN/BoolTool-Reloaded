module ParserError exposing (..)

import BoolImpl exposing (Context, Problem(..))
import Html exposing (Html, div, section, span, table, td, text, tr)
import Html.Attributes exposing (class)
import List
import Parser.Advanced exposing (DeadEnd)



-- VIEW


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
                                [ table []
                                    [ tr [ class "wavy" ]
                                        [ td [] [ text <| String.dropRight (length - error.column + 1) input ]
                                        , td [] [ text <| String.slice (error.column - 1) error.column input ]
                                        , td [] [ text <| String.dropLeft error.column input ]
                                        ]
                                    , tr []
                                        [ td [] []
                                        , td [] [ span [] [], text "⬆", span [] [] ]
                                        , td [] []
                                        ]
                                    ]
                                , div [] [ span [] [], text error.message, span [] [] ]
                                ]

                        Nothing ->
                            section [] [ text "Invalid Input" ]
               )



-- OTHER FUNCTIONS


addMessageToRecord : DeadEnd Context Problem -> String -> { column : Int, message : String, problem : Problem }
addMessageToRecord error message =
    { column = error.col, message = message, problem = error.problem }
