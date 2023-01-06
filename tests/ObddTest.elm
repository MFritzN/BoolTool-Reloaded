module ObddTest exposing (..)

import BoolImpl exposing (Formula, evaluateUnsafe, getVariables, iterateVariables, toString)
import Dict exposing (Dict)
import Expect
import Graph as G
import IntDict
import List.Extra
import OBDD exposing (computeOBDD)
import Set
import Test exposing (Test, describe, test)
import TestHelp exposing (testFormulas)


bbdSuite : Test
bbdSuite =
    describe "BDD Equivalence"
        (List.map
            (\formula ->
                describe (toString formula)
                    (let
                        varOrder =
                            allVariableCombinations (Set.toList (getVariables formula))
                     in
                     List.map (\vars -> describe ("Var Order: " ++ Debug.toString vars) (isEqualBdd formula vars)) varOrder
                    )
            )
            testFormulas
        )


isEqualBdd : Formula -> List String -> List Test
isEqualBdd formula variables =
    let
        bdd =
            computeOBDD formula variables

        values =
            Dict.fromList (List.map (\variable -> ( variable, Basics.False )) variables)
    in
    isEqualBddHelp formula values bdd


isEqualBddHelp : Formula -> Dict String Basics.Bool -> G.Graph String Basics.Bool -> List Test
isEqualBddHelp formula variables graph =
    (test ("Varvalues: " ++ Debug.toString variables) <|
        \_ ->
            case evaluateGraph graph variables of
                Ok result ->
                    Expect.equal (evaluateUnsafe formula variables) result

                Err err ->
                    Expect.fail err
    )
        :: (case iterateVariables variables of
                Just newValues ->
                    isEqualBddHelp formula newValues graph

                Nothing ->
                    []
           )


evaluateGraph : G.Graph String Basics.Bool -> Dict String Basics.Bool -> Result String Basics.Bool
evaluateGraph graph variables =
    List.Extra.find (\context -> IntDict.isEmpty context.incoming) (getContexts graph)
        |> Result.fromMaybe "Could not find root node."
        |> Result.andThen (\context -> evaluateGraphHelp context graph variables)


evaluateGraphHelp : G.NodeContext String Basics.Bool -> G.Graph String Basics.Bool -> Dict String Basics.Bool -> Result String Basics.Bool
evaluateGraphHelp context graph variables =
    case ( context.node.id, Dict.get context.node.label variables ) of
        ( 0, _ ) ->
            Ok Basics.False

        ( 1, _ ) ->
            Ok Basics.True

        ( id, Just value ) ->
            IntDict.toList context.outgoing
                |> List.Extra.find (\( k, v ) -> v == value)
                |> Result.fromMaybe ("Was not able to find the correct edge from node {id: " ++ String.fromInt context.node.id ++ ", label: " ++ context.node.label ++ ".")
                |> Result.andThen (\( k, _ ) -> Result.fromMaybe "I couldn't find the end node of an edge" (G.get k graph))
                |> Result.andThen (\c -> evaluateGraphHelp c graph variables)

        ( id, Nothing ) ->
            Err ("Could not find the Variable Value for label " ++ context.node.label ++ ".")


allVariableCombinations : List String -> List (List String)
allVariableCombinations variables =
    case allVariableCombinationsHelp [ variables ] [] variables variables of
        Err a ->
            a

        Ok a ->
            a


allVariableCombinationsHelp : List (List String) -> List String -> List String -> List String -> Result (List (List String)) (List (List String))
allVariableCombinationsHelp variablesList currentTry remainingVars originalVars =
    case remainingVars of
        [] ->
            Err variablesList

        var :: vars ->
            if List.any ((==) var) currentTry then
                allVariableCombinationsHelp variablesList currentTry vars originalVars

            else if List.length currentTry == List.length originalVars - 1 then
                if List.any (\list -> list == (var :: currentTry)) variablesList then
                    allVariableCombinationsHelp variablesList currentTry vars originalVars

                else
                    Ok ((var :: currentTry) :: variablesList)

            else
                case allVariableCombinationsHelp variablesList (var :: currentTry) originalVars originalVars of
                    Ok result ->
                        Ok result

                    Err result ->
                        allVariableCombinationsHelp result (var :: currentTry) vars originalVars


getContexts : G.Graph String Basics.Bool -> List (G.NodeContext String Basics.Bool)
getContexts graph =
    G.nodes graph
        |> List.map (\a -> G.get a.id graph)
        |> List.filterMap identity
