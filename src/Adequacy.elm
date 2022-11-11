module Adequacy exposing (..)

import BoolImpl exposing (..)
import Dict
import Set


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
