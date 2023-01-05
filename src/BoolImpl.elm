module BoolImpl exposing (..)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, end, keyword, succeed, symbol, variable)
import Pratt exposing (constant, infixRight, prefix)
import Set exposing (..)



--
-- A module for representing and handling boolean formulas and sets of functions internally
--


{-| Internal Representation for Boolean Formulas and Functions
-}
type Formula
    = False
    | True
    | And Formula Formula
    | Or Formula Formula
    | Xor Formula Formula
    | Neg Formula
    | Impl Formula Formula
    | Var String


precedence : Formula -> Int
precedence operator =
    case operator of
        And _ _ ->
            3

        Impl _ _ ->
            1

        Xor _ _ ->
            2

        Or _ _ ->
            3

        Neg _ ->
            4

        _ ->
            5


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


typeVar : Parser String
typeVar =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c
        , reserved = Set.fromList [ "true", "false" ]
        }


typeVarHelp : Pratt.Config Formula -> Parser Formula
typeVarHelp _ =
    succeed Var
        |= typeVar


boolExpression : Parser Formula
boolExpression =
    Pratt.expression
        { oneOf =
            [ typeVarHelp
            , constant (keyword "True") True
            , constant (keyword "False") False
            , constant (symbol "⊤") True
            , constant (symbol "⊥") False
            , prefix (precedence (Neg True)) (symbol "¬") Neg
            , parenthesizedExpression
            ]
        , andThenOneOf =
            [ infixRight (precedence (And True True)) (symbol "∧") And
            , infixRight (precedence (Or True True)) (symbol "∨") Or
            , infixRight (precedence (Xor True True)) (symbol "⊕") Xor
            , infixRight (precedence (Impl True True)) (symbol "→") Impl
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
            "⊤"

        False ->
            "⊥"

        Var v ->
            v

        And lForm rForm ->
            toStringHelp "∧" (And lForm rForm) lForm rForm

        Or lForm rForm ->
            toStringHelp "∨" (Or lForm rForm) lForm rForm

        Neg r_form ->
            if precedence (Neg r_form) > precedence r_form then
                "¬" ++ "(" ++ toString r_form ++ ")"

            else
                "¬" ++ toString r_form

        Impl lForm rForm ->
            toStringHelp "→" (Impl lForm rForm) lForm rForm

        Xor lForm rForm ->
            toStringHelp "⊕" (Xor lForm rForm) lForm rForm


toStringHelp : String -> Formula -> Formula -> Formula -> String
toStringHelp symbol formula lForm rForm =
    (if precedence formula >= precedence lForm then
        "(" ++ toString lForm ++ ")"

     else
        toString lForm
    )
        ++ " "
        ++ symbol
        ++ " "
        ++ (if precedence formula > precedence rForm then
                "(" ++ toString rForm ++ ")"

            else
                toString rForm
           )


{-| Replaces some input symbols and latex equivalents with their correpsonding Unicode charackters.

    preprocessString "a & b \wedge c" == "a ∧ b ∧ c"

-}
preprocessString : String -> String
preprocessString string =
    string
        |> String.replace "\\wedge" "∧"
        |> String.replace "&" "∧"
        |> String.replace "\\vee" "∨"
        |> String.replace "|" "∨"
        |> String.replace "~" "¬"
        |> String.replace "\\neg" "¬"
        |> String.replace "!" "¬"
        |> String.replace "^" "⊕"
        |> String.replace "->" "→"
        |> String.replace "\\to" "→"
        |> String.replace "\\rightarrow" "→"
        |> String.replace "\\implies" "→"
        |> String.replace "\\oplus" "⊕"
        |> String.replace "\\top" "⊤"
        |> String.replace "\\bottom" "⊥"


reversePreprocessString : String -> String
reversePreprocessString string =
    string
        |> String.filter (\c -> c /= ' ')
        |> String.replace "∧" "\\wedge"
        |> String.replace "∨" "\\vee"
        |> String.replace "¬" "\\neg"
        |> String.replace "⊕" "\\oplus"
        |> String.replace "→" "\\implies"
        |> String.replace "⊤" "\\top"
        |> String.replace "⊥" "\\bottom"


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


{-| Evaluate a [`Formula`](BoolImpl#Formula).
This functions returns `True` if Variable Values are mssing. If this is a possible scnario use [`evaluateSafe`](BoolImpl#evaluateSafe)
-}
evaluateUnsafe : Formula -> Dict String Bool -> Bool
evaluateUnsafe formula variables =
    Result.withDefault Basics.True (evaluateSafe formula variables)


evaluateSafe : Formula -> Dict String Bool -> Result String Bool
evaluateSafe formula variables =
    case formula of
        True ->
            Ok Basics.True

        False ->
            Ok Basics.False

        Var string ->
            Result.fromMaybe ("Could not find value for " ++ string) (Dict.get string variables)

        Or subFormA subFormB ->
            Result.map2 (||) (evaluateSafe subFormA variables) (evaluateSafe subFormB variables)

        And subFormA subFormB ->
            Result.map2 (&&) (evaluateSafe subFormA variables) (evaluateSafe subFormB variables)

        Neg subForm ->
            Result.map not (evaluateSafe subForm variables)

        Impl subFormA subFormB ->
            Result.map2 (\a b -> not a || b) (evaluateSafe subFormA variables) (evaluateSafe subFormB variables)

        Xor subFormA subFormB ->
            Result.map2 xor (evaluateSafe subFormA variables) (evaluateSafe subFormB variables)


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


simplify : Formula -> Formula
simplify formula =
    case formula of
        True ->
            True

        False ->
            False

        Var string ->
            Var string

        And form1 form2 ->
            case ( simplify form1, simplify form2 ) of
                ( True, x ) ->
                    x

                ( x, True ) ->
                    x

                ( False, _ ) ->
                    False

                ( _, False ) ->
                    False

                ( x, y ) ->
                    And x y

        Xor form1 form2 ->
            case ( simplify form1, simplify form2 ) of
                ( True, True ) ->
                    False

                ( False, x ) ->
                    x

                ( x, False ) ->
                    x

                ( x, y ) ->
                    Xor x y

        Or form1 form2 ->
            case ( simplify form1, simplify form2 ) of
                ( True, _ ) ->
                    True

                ( _, True ) ->
                    True

                ( False, x ) ->
                    x

                ( x, False ) ->
                    x

                ( x, y ) ->
                    Or x y

        Impl form1 form2 ->
            case ( simplify form1, simplify form2 ) of
                ( False, _ ) ->
                    True

                ( True, x ) ->
                    x

                ( x, False ) ->
                    x

                ( x, y ) ->
                    Impl x y

        Neg form1 ->
            case simplify form1 of
                True ->
                    False

                False ->
                    True

                x ->
                    Neg x


varsToString : Dict String Basics.Bool -> String
varsToString vars =
    let
        stringVars =
            String.dropRight 2
                (List.foldl
                    (\value string ->
                        string
                            ++ (if value then
                                    "1, "

                                else
                                    "0, "
                               )
                    )
                    ""
                    (Dict.values vars)
                )
    in
    "f ( " ++ stringVars ++ " )"
