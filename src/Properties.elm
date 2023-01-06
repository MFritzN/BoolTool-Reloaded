module Properties exposing (..)

import BoolImpl exposing (..)
import Dict exposing (Dict)
import List exposing (unzip)
import Set


type alias FormulaProperties =
    { tautology : Bool
    , satisfiable : Bool
    , contradiction : Bool
    }


{-| Inner representation of a Truth Table. `vars` defines the order of the results. `results` is a tuple which first contains the input values sorted according to `vars` and secondly the corresponding result for those input variables.
-}
type alias TruthTable =
    { vars : List String
    , results : List ( List Basics.Bool, Basics.Bool )
    }


{-| Calculates the Truthtable of a given formula in the variable order for the given variables.
The return value is a
-}
calculateTruthTable : Formula -> TruthTable
calculateTruthTable formula =
    let
        variables =
            Dict.fromList (List.map (\variable -> ( variable, Basics.False )) (Set.toList (getVariables formula)))
    in
    { vars = Dict.keys variables, results = ( Dict.values variables, evaluateUnsafe formula variables ) :: calculateTruthTableHelp formula variables }


calculateTruthTableHelp : Formula -> Dict String Basics.Bool -> List ( List Basics.Bool, Basics.Bool )
calculateTruthTableHelp formula variables =
    case iterateVariables variables of
        Nothing ->
            []

        Just newVariables ->
            ( Dict.values newVariables, evaluateUnsafe formula newVariables ) :: calculateTruthTableHelp formula newVariables


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
