module BoolImpl exposing (..)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, end, keyword, succeed, symbol, variable)
import Pratt exposing (constant, infixRight, prefix)
import Set exposing (..)



--
-- A module for representing and handling boolean formulas and sets of functions internally
--


type Formula
    = False
    | True
    | And Formula Formula
    | Or Formula Formula
    | Xor Formula Formula
    | Neg Formula
    | Impl Formula Formula
    | Var String


equals : Formula -> Formula -> Bool
equals form1 form2 =
    case ( form1, form2 ) of
        ( True, True ) ->
            Basics.True

        ( False, False ) ->
            Basics.True

        ( And form11 form12, And form21 form22 ) ->
            equals form11 form21 && equals form12 form22

        ( Or form11 form12, Or form21 form22 ) ->
            equals form11 form21 && equals form12 form22

        ( Impl form11 form12, Impl form21 form22 ) ->
            equals form11 form21 && equals form12 form22

        ( Xor form11 form12, Xor form21 form22 ) ->
            equals form11 form21 && equals form12 form22

        ( Neg form11, Neg form21 ) ->
            equals form11 form21

        ( Var string1, Var string2 ) ->
            string1 == string2

        _ ->
            Basics.False



-- The following code was adapted from
-- https://github.com/dmy/elm-pratt-parser/blob/2.0.0/examples/Math.elm (2022-11-08)


typeVar : Parser String
typeVar =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c
        , reserved = Set.fromList [ "true", "false" ]
        }


typeVarHelp : Pratt.Config Formula -> Parser Formula
typeVarHelp config =
    succeed Var
        |= typeVar


boolExpression : Parser Formula
boolExpression =
    Pratt.expression
        { oneOf =
            [ typeVarHelp
            , constant (keyword "true") True
            , constant (keyword "false") False
            , prefix 3 (symbol "~") Neg
            , parenthesizedExpression
            ]
        , andThenOneOf =
            [ infixRight 2 (symbol "&") And
            , infixRight 2 (symbol "|") Or
            , infixRight 2 (symbol "^") Xor
            , infixRight 1 (symbol "->") Impl
            ]
        , spaces = Parser.spaces
        }


parenthesizedExpression : Pratt.Config Formula -> Parser Formula
parenthesizedExpression config =
    succeed identity
        |. symbol "("
        |= Pratt.subExpression 0 config
        |. symbol ")"


formula_p : Parser Formula
formula_p =
    succeed identity
        |= boolExpression
        |. end


toString : Formula -> String
toString formula =
    case formula of
        True ->
            "T"

        False ->
            "F"

        Var v ->
            v

        And l_form r_form ->
            "(" ++ toString l_form ++ "&" ++ toString r_form ++ ")"

        Or l_form r_form ->
            "(" ++ toString l_form ++ "|" ++ toString r_form ++ ")"

        Neg r_form ->
            "(" ++ "~" ++ toString r_form ++ ")"

        Impl l_form r_form ->
            "(" ++ toString l_form ++ "->" ++ toString r_form ++ ")"

        Xor l_form r_form ->
            "(" ++ toString l_form ++ "^" ++ toString r_form ++ ")"


getVariables : Formula -> Set String
getVariables formula =
    case formula of
        True ->
            Set.empty

        False ->
            Set.empty

        Var string ->
            Set.singleton string

        Neg subForm ->
            getVariables subForm

        And subFormA subFormB ->
            Set.union (getVariables subFormA) (getVariables subFormB)

        Or subFormA subFormB ->
            Set.union (getVariables subFormA) (getVariables subFormB)

        Impl subFormA subFormB ->
            Set.union (getVariables subFormA) (getVariables subFormB)

        Xor subFormA subFormB ->
            Set.union (getVariables subFormA) (getVariables subFormB)


evaluate : Formula -> Dict String Bool -> Result String Bool
evaluate formula variables =
    case formula of
        True ->
            Ok Basics.True

        False ->
            Ok Basics.False

        Var string ->
            case Dict.get string variables of
                Just value ->
                    Ok value

                Nothing ->
                    Err ("Could not find variable value for " ++ string)

        Or subFormA subFormB ->
            case ( evaluate subFormA variables, evaluate subFormB variables ) of
                ( Err err, _ ) ->
                    Err err

                ( _, Err err ) ->
                    Err err

                ( Ok boolA, Ok boolB ) ->
                    Ok (boolA || boolB)

        And subFormA subFormB ->
            case ( evaluate subFormA variables, evaluate subFormB variables ) of
                ( Err err, _ ) ->
                    Err err

                ( _, Err err ) ->
                    Err err

                ( Ok boolA, Ok boolB ) ->
                    Ok (boolA && boolB)

        Neg subForm ->
            case evaluate subForm variables of
                Ok bool ->
                    Ok (not bool)

                Err err ->
                    Err err

        Impl subFormA subFormB ->
            case ( evaluate subFormA variables, evaluate subFormB variables ) of
                ( Err err, _ ) ->
                    Err err

                ( _, Err err ) ->
                    Err err

                ( Ok boolA, Ok boolB ) ->
                    Ok (not boolA || boolB)

        Xor subFormA subFormB ->
            case ( evaluate subFormA variables, evaluate subFormB variables ) of
                ( Err err, _ ) ->
                    Err err

                ( _, Err err ) ->
                    Err err

                ( Ok boolA, Ok boolB ) ->
                    Ok (xor boolA boolB)
