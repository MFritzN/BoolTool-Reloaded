module Adequacy exposing (..)

import BoolImpl exposing (..)
import Dict exposing (..)
import Parser exposing (variable)
import Set
import Test.Runner.Failure exposing (Reason(..))


existsAllInputNotEqInput : List Formula -> Basics.Bool -> Basics.Bool
existsAllInputNotEqInput list x =
    List.any
        (\formula ->
            evaluate formula (Dict.fromList (List.map (\variable -> ( variable, x )) (Set.toList (getVariables formula)))) /= x
        )
        list



-- is not montone


exsistsIsNotMonotone : List Formula -> Basics.Bool
exsistsIsNotMonotone list =
    List.any isNotMontone list


isNotMontone : Formula -> Basics.Bool
isNotMontone formula =
    let
        variables =
            Dict.fromList (List.map (\variable -> ( variable, Basics.False )) (Set.toList (getVariables formula)))
    in
    isNotMonotoneHelp formula variables (Dict.keys variables)


isNotMonotoneHelp : Formula -> Dict String Bool -> List String -> Bool
isNotMonotoneHelp formula variables remainingVariables =
    case remainingVariables of
        [] ->
            case iterateVariables variables of
                Nothing ->
                    Basics.False

                Just newVariables ->
                    isNotMonotoneHelp formula newVariables (Dict.keys newVariables)

        currentVar :: remainingVariablesTail ->
            if not (BoolImpl.evaluate formula (Dict.insert currentVar Basics.True variables)) && BoolImpl.evaluate formula (Dict.insert currentVar Basics.False variables) then
                Basics.True

            else
                isNotMonotoneHelp formula variables remainingVariablesTail



-- is not self-dual


exsistsIsNotSelfDual : List Formula -> Basics.Bool
exsistsIsNotSelfDual list =
    List.any isNotSelfDual list


isNotSelfDual : Formula -> Basics.Bool
isNotSelfDual formula =
    let
        variables =
            Dict.fromList (List.map (\variable -> ( variable, Basics.False )) (Set.toList (getVariables formula)))
    in
    isNotSelfDualHelp formula variables


isNotSelfDualHelp : Formula -> Dict String Bool -> Bool
isNotSelfDualHelp formula variables =
    let
        inverse_variables =
            Dict.map (\_ v -> not v) variables
    in
    if evaluate formula variables == evaluate formula inverse_variables then
        Basics.True

    else
        case iterateVariables variables of
            Nothing ->
                Basics.False

            Just newVariables ->
                isNotSelfDualHelp formula newVariables
