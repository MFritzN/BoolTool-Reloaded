module Adequacy.SelfDualness exposing (..)

import BoolImpl exposing (Formula, evaluateUnsafe, getVariables, iterateVariables, varsToString)
import Dict exposing (Dict)
import Html exposing (Html, text)
import Set
import ViewHelpers exposing (renderTooltip)


renderSelfDualness : Formula -> Html a
renderSelfDualness formula =
    case isNotSelfDual formula of
        Just vars ->
            renderTooltip (text "✓") <| varsToString vars ++ " = " ++ varsToString (Dict.map (\_ v -> not v) vars)

        Nothing ->
            text "✕"


exsistsIsNotSelfDual : List Formula -> Basics.Bool
exsistsIsNotSelfDual list =
    List.any
        (\formula ->
            case isNotSelfDual formula of
                Nothing ->
                    Basics.False

                Just _ ->
                    Basics.True
        )
        list


isNotSelfDual : Formula -> Maybe (Dict String Bool)
isNotSelfDual formula =
    let
        variables =
            Dict.fromList (List.map (\variable -> ( variable, Basics.False )) (Set.toList (getVariables formula)))
    in
    isNotSelfDualHelp formula variables


isNotSelfDualHelp : Formula -> Dict String Bool -> Maybe (Dict String Bool)
isNotSelfDualHelp formula variables =
    let
        inverse_variables =
            Dict.map (\_ v -> not v) variables
    in
    if evaluateUnsafe formula variables == evaluateUnsafe formula inverse_variables then
        Just variables

    else
        Maybe.andThen (\newVariables -> isNotSelfDualHelp formula newVariables) (iterateVariables variables)
