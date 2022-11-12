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
            case evaluate formula (Dict.fromList (List.map (\variable -> ( variable, x )) (Set.toList (getVariables formula)))) of
                Ok r ->
                    r /= x

                Err _ ->
                    Basics.False
        )
        list



-- is montone


exsistsIsNotMonotone : List Formula -> Basics.Bool
exsistsIsNotMonotone list =
    List.any isNotMontone list


isNotMontone : Formula -> Basics.Bool
isNotMontone formula =
    let
        variables =
            Dict.fromList (List.map (\variable -> ( variable, Basics.True )) (Set.toList (getVariables formula)))
    in
    isMonotoneHelp formula variables (Dict.keys variables)


isMonotoneHelp : Formula -> Dict String Bool -> List String -> Bool
isMonotoneHelp formula variables remainingVariables =
    case remainingVariables of
        [] ->
            case iterateVariables variables of
                Nothing ->
                    Basics.False

                Just newVariables ->
                    isMonotoneHelp formula newVariables (Dict.keys newVariables)

        currentVar :: remainingVariablesTail ->
            if not (Result.withDefault Basics.True (BoolImpl.evaluate formula (Dict.insert currentVar Basics.True variables))) && Result.withDefault Basics.True (BoolImpl.evaluate formula (Dict.insert currentVar Basics.True variables)) then
                Basics.True

            else
                isMonotoneHelp formula variables remainingVariablesTail


{-| Interprets the values in the dictonary as a binary number and increases it by 1 until all digits are 1.
The function can be used to try every possible combination of variables. To do that start with a dict that only contains False as values.

    iterateVariables {c: False, b: True, a: False} = {c: True, b: False, a: True}

-}
iterateVariables : Dict String Bool -> Maybe (Dict String Bool)
iterateVariables dict =
    iterateVariablesHelp [] (Dict.values dict)
        |> Maybe.andThen (\a -> Just (Dict.fromList (List.map2 Tuple.pair (Dict.keys dict) a)))


iterateVariablesHelp : List Basics.Bool -> List Basics.Bool -> Maybe (List Basics.Bool)
iterateVariablesHelp changedVariables unchangedVariables =
    case unchangedVariables of
        [] ->
            Nothing

        Basics.False :: unchangedVariablesTail ->
            Just (changedVariables ++ Basics.True :: unchangedVariablesTail)

        Basics.True :: unchangedVariablesTail ->
            iterateVariablesHelp (changedVariables ++ [ Basics.False ]) unchangedVariablesTail
