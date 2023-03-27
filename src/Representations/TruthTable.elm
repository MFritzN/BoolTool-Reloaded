module Representations.TruthTable exposing (..)

import BoolImpl exposing (..)
import Dict
import Html exposing (Html, table, td, text, th, tr)
import Html.Attributes exposing (class)
import Set



-- VIEW


renderTruthTable : Formula -> { title : String, render : Html a }
renderTruthTable formula =
    let
        truthTable =
            calculateTruthTable formula
    in
    { title = "Truth Table"
    , render =
        table [ class "table is-narrow is-striped is-hoverable is-bordered" ]
            (tr [] (List.map (\variable -> th [] [ text variable ]) truthTable.vars ++ [ th [] [ text "Result" ] ])
                :: List.map (\row -> tr [] (List.map (\value -> td [] [ prettyPrintBool value ]) (Tuple.first row) ++ [ td [] [ prettyPrintBool (Tuple.second row) ] ]))
                    truthTable.results
            )
    }


prettyPrintBool : Basics.Bool -> Html a
prettyPrintBool bool =
    if bool then
        text "T"

    else
        text "F"



-- OTHER FUNCTIONS


{-| Inner representation of a Truth Table. `vars` defines the order of the results. `results` is a tuple which first contains the input values sorted according to `vars` and secondly the corresponding result for those input variables.
-}
type alias TruthTable =
    { vars : List String
    , results : List ( List Basics.Bool, Basics.Bool )
    }


{-| Calculates the Truthtable of a given formula in the variable order for the given variables.
-}
calculateTruthTable : Formula -> TruthTable
calculateTruthTable formula =
    let
        variables =
            Dict.fromList (List.map (\variable -> ( variable, Basics.False )) (Set.toList (getVariables formula)))
    in
    sortTruthTable <| { vars = Dict.keys variables, results = ( Dict.values variables, evaluateUnsafe formula variables ) :: calculateTruthTableHelp formula variables }


calculateTruthTableHelp : Formula -> Dict.Dict String Basics.Bool -> List ( List Basics.Bool, Basics.Bool )
calculateTruthTableHelp formula variables =
    case iterateVariables variables of
        Nothing ->
            []

        Just newVariables ->
            ( Dict.values newVariables, evaluateUnsafe formula newVariables ) :: calculateTruthTableHelp formula newVariables


{-| Sorts the input TruthTable so that input [1,1,...,1] comes first. Our implmentation would otherwise always return [0,0,...,0] first.
-}
sortTruthTable : TruthTable -> TruthTable
sortTruthTable truthTable =
    let
        bToI b =
            if b then
                0

            else
                1
    in
    { truthTable
        | results =
            List.sortWith
                (\p1 p2 ->
                    case ( p1, p2 ) of
                        ( ( vs1, _ ), ( vs2, _ ) ) ->
                            compare (List.map bToI vs1) (List.map bToI vs2)
                )
                truthTable.results
    }
