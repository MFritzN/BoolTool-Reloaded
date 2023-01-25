module Representations.Properties exposing (..)

import BoolImpl exposing (..)
import Html exposing (Html, table, td, text, tr)
import List exposing (unzip)
import Representations.TruthTable exposing (calculateTruthTable)
import ViewHelpers exposing (boolToSymbol)


type alias FormulaProperties =
    { tautology : Bool
    , satisfiable : Bool
    , contradiction : Bool
    }



-- VIEW


renderProperties : Formula -> { title : String, render : Html a }
renderProperties formula =
    let
        properties =
            calculateProperties formula
    in
    { title = "Properties"
    , render =
        table []
            [ tr []
                [ td [] [ text "Tautology" ]
                , td [] [ text (boolToSymbol properties.tautology) ]
                ]
            , tr []
                [ td [] [ text "Satisfiable" ]
                , td [] [ text (boolToSymbol properties.satisfiable) ]
                ]
            , tr []
                [ td [] [ text "Contradiction" ]
                , td [] [ text (boolToSymbol properties.contradiction) ]
                ]
            ]
    }



-- OTHER FUNCTIONS


calculateProperties : Formula -> FormulaProperties
calculateProperties formula =
    let
        truthTable =
            calculateTruthTable formula

        results =
            truthTable.results
                |> unzip
                |> Tuple.second
    in
    { tautology = not (List.any (\a -> not a) results)
    , satisfiable = List.any (\a -> a) results
    , contradiction = not (List.any (\a -> a) results)
    }
