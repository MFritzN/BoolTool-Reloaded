module Adequacy.Monotonicity exposing (..)

import BoolImpl exposing (Formula, getVariables, iterateVariables)
import Dict exposing (Dict)
import Html exposing (Html, text)
import Set
import ViewHelpers exposing (boolToSymbol, renderTooltip)



-- VIEW


renderMonotone : Formula -> Html a
renderMonotone formula =
    case isNotMontone formula of
        Nothing ->
            text <| boolToSymbol Basics.False

        Just vars ->
            renderTooltip (text <| boolToSymbol Basics.True) <| (String.dropRight 2 <| List.foldl (\var str -> str ++ var ++ ", ") "f (" vars) ++ ") = x̄"



-- OTHER FUNCTIONS


isNotMontone : Formula -> Maybe (List String)
isNotMontone formula =
    let
        variables =
            Dict.fromList (List.map (\variable -> ( variable, Basics.False )) (Set.toList (getVariables formula)))
    in
    isNotMonotoneHelp formula variables (Dict.keys variables)


isNotMonotoneHelp : Formula -> Dict String Bool -> List String -> Maybe (List String)
isNotMonotoneHelp formula variables remainingVariables =
    case remainingVariables of
        [] ->
            iterateVariables variables
                |> Maybe.andThen (\newVariables -> isNotMonotoneHelp formula newVariables (Dict.keys newVariables))

        currentVar :: remainingVariablesTail ->
            if not (BoolImpl.evaluateUnsafe formula (Dict.insert currentVar Basics.True variables)) && BoolImpl.evaluateUnsafe formula (Dict.insert currentVar Basics.False variables) then
                variables
                    |> Dict.map
                        (\_ v ->
                            if v then
                                "1"

                            else
                                "0"
                        )
                    |> Dict.insert currentVar "x"
                    |> Dict.values
                    |> Just

            else
                isNotMonotoneHelp formula variables remainingVariablesTail


exsistsIsNotMonotone : List Formula -> Basics.Bool
exsistsIsNotMonotone list =
    List.any
        (\el ->
            case isNotMontone el of
                Just _ ->
                    Basics.True

                Nothing ->
                    Basics.False
        )
        list
