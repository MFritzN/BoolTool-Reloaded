module BoolImpl exposing (..)

import Dict exposing (Dict)
import Parser.Advanced exposing ((|.), (|=), Parser, end, inContext, keyword, succeed, symbol, variable)
import Pratt.Advanced exposing (constant, infixRight, prefix)
import Set exposing (..)



--
-- A module for representing and handling boolean formulas and sets of functions internally
--
-- Types


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
    | Equal Formula Formula


type alias MyParser a =
    Parser.Advanced.Parser Context Problem a


type Context
    = FormulaContext


type Problem
    = ExpectingVariable
    | ExpectingOperator
    | ExpectingOpeningBracket
    | ExpectingClosingBracket
    | ExpectingEnd



-- Parser


formula_p : Parser Context Problem Formula
formula_p =
    succeed identity
        |= boolExpression
        |. end ExpectingEnd


precedence : Formula -> Int
precedence operator =
    case operator of
        And _ _ ->
            4

        Impl _ _ ->
            2

        Xor _ _ ->
            3

        Or _ _ ->
            4

        Neg _ ->
            5

        Equal _ _ ->
            1

        _ ->
            6


typeVar : Parser c Problem String
typeVar =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c
        , reserved = Set.fromList [ "true", "false", "T", "F" ]
        , expecting = ExpectingVariable
        }


typeVarHelp : Pratt.Advanced.Config Context Problem Formula -> Parser Context Problem Formula
typeVarHelp _ =
    succeed Var
        |= typeVar


parenthesizedExpression : Pratt.Advanced.Config c Problem Formula -> Parser c Problem Formula
parenthesizedExpression config =
    succeed identity
        |. symbol (Parser.Advanced.Token "(" ExpectingOpeningBracket)
        |= Pratt.Advanced.subExpression 0 config
        |. symbol (Parser.Advanced.Token ")" ExpectingClosingBracket)


boolExpression : Parser Context Problem Formula
boolExpression =
    inContext FormulaContext <|
        Pratt.Advanced.expression
            { oneOf =
                [ typeVarHelp
                , constant (symbol <| Parser.Advanced.Token "⊤" ExpectingVariable) True
                , constant (symbol <| Parser.Advanced.Token "⊥" ExpectingVariable) False
                , prefix (precedence (Neg True)) (symbol (Parser.Advanced.Token "¬" ExpectingOperator)) Neg
                , parenthesizedExpression
                ]
            , andThenOneOf =
                [ infixRight (precedence (And True True)) (symbol <| Parser.Advanced.Token "∧" ExpectingOperator) And
                , infixRight (precedence (Or True True)) (symbol <| Parser.Advanced.Token "∨" ExpectingVariable) Or
                , infixRight (precedence (Xor True True)) (symbol <| Parser.Advanced.Token "⊕" ExpectingVariable) Xor
                , infixRight (precedence (Impl True True)) (symbol <| Parser.Advanced.Token "→" ExpectingVariable) Impl
                , infixRight (precedence (Equal True True)) (symbol <| Parser.Advanced.Token "↔" ExpectingVariable) Equal
                ]
            , spaces = Parser.Advanced.spaces
            }


{-| Replaces some input symbols and latex equivalents with their correpsonding Unicode charackters.

    preprocessString "a & b \wedge c" == "a ∧ b ∧ c"

-}
preprocessString : String -> String
preprocessString string =
    string
        |> String.replace "\\wedge" "∧"
        |> String.replace "&" "∧"
        |> String.replace "+" "∨"
        |> String.replace "⋅" "∧"
        |> String.replace "\\land" "∧"
        |> String.replace "\\vee" "∨"
        |> String.replace "\\lor" "∨"
        |> String.replace "|" "∨"
        |> String.replace "~" "¬"
        |> String.replace "\\neg" "¬"
        |> String.replace "\\lnot" "¬"
        |> String.replace "!" "¬"
        |> String.replace "^" "⊕"
        |> String.replace "<->" "↔"
        |> String.replace "<→" "↔"
        |> String.replace "->" "→"
        |> String.replace "\\rightarrow" "→"
        |> String.replace "\\implies" "→"
        |> String.replace "\\oplus" "⊕"
        |> String.replace "\\top" "⊤"
        |> String.replace "\\bot" "⊥"
        |> String.replace "T" "⊤"
        |> String.replace "F" "⊥"
        |> String.replace "true" "⊤"
        |> String.replace "false" "⊥"
        |> String.replace "\\leftrightarrow" "↔"



-- Pretty Print


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

        Equal lForm rForm ->
            toStringHelp "↔" (Equal lForm rForm) lForm rForm

        Impl lForm rForm ->
            toStringHelp "→" (Impl lForm rForm) lForm rForm

        Xor lForm rForm ->
            toStringHelp "⊕" (Xor lForm rForm) lForm rForm


toStringHelp : String -> Formula -> Formula -> Formula -> String
toStringHelp symbol formula lForm rForm =
    (if precedence formula > precedence lForm || (precedence formula == precedence lForm && (not <| topOperaterIsEqual formula lForm && operatorIsAssociative formula)) then
        "(" ++ toString lForm ++ ")"

     else
        toString lForm
    )
        ++ " "
        ++ symbol
        ++ " "
        ++ (if precedence formula > precedence rForm || (precedence formula == precedence rForm && (not <| topOperaterIsEqual formula rForm)) then
                "(" ++ toString rForm ++ ")"

            else
                toString rForm
           )


operatorIsAssociative : Formula -> Basics.Bool
operatorIsAssociative formula =
    case formula of
        And _ _ ->
            Basics.True

        Or _ _ ->
            Basics.True

        Xor _ _ ->
            Basics.True

        Equal _ _ ->
            Basics.True

        _ ->
            Basics.False


topOperaterIsEqual : Formula -> Formula -> Basics.Bool
topOperaterIsEqual formula1 formula2 =
    case ( formula1, formula2 ) of
        ( Var _, Var _ ) ->
            Basics.True

        ( And _ _, And _ _ ) ->
            Basics.True

        ( Or _ _, Or _ _ ) ->
            Basics.True

        ( Xor _ _, Xor _ _ ) ->
            Basics.True

        ( Impl _ _, Impl _ _ ) ->
            Basics.True

        ( Neg _, Neg _ ) ->
            Basics.True

        ( Equal _ _, Equal _ _ ) ->
            Basics.True

        _ ->
            Basics.False


functionHeaderToString : List String -> String
functionHeaderToString vars =
    if List.isEmpty vars then
        "f ()"

    else
        List.foldl (\var header -> header ++ var ++ ", ") "f (" vars
            |> String.dropRight 2
            |> (\str -> str ++ ")")


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


prettyPrintToLaTeX : String -> String
prettyPrintToLaTeX string =
    string
        |> String.replace "∧" "\\land"
        |> String.replace "∨" "\\lor"
        |> String.replace "¬" "\\lnot "
        |> String.replace "⊕" "\\oplus"
        |> String.replace "→" "\\implies"
        |> String.replace "⊤" "\\top"
        |> String.replace "⊥" "\\bot"
        |> String.replace "↔" "\\leftrightarrow"
        |> String.replace "⋅" "\\cdot"


{-| Remove unicode symbols from strings. This is mainly needed to save formulas into the URL.
-}
prettyPrintToURL : String -> String
prettyPrintToURL string =
    string
        |> prettyPrintToLaTeX
        |> String.filter (\c -> c /= ' ')


formulaToLaTeX : Formula -> String
formulaToLaTeX formula =
    prettyPrintToLaTeX <| toString formula



-- Comparing Formulas


{-| Test two formulas on total equality.

    equals (And a b) (And b a) == False
    equals (And a b) (And a b) == True

-}
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

        ( Equal form11 form12, Equal form21 form22 ) ->
            equals form11 form21 && equals form12 form22

        ( Neg form11, Neg form21 ) ->
            equals form11 form21

        ( Var string1, Var string2 ) ->
            string1 == string2

        _ ->
            Basics.False


getVariables : Formula -> Set String
getVariables formula =
    case formula of
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

        Equal subFormA subFormB ->
            Set.union (getVariables subFormA) (getVariables subFormB)

        -- True & False
        _ ->
            Set.empty


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

        Equal subFormA subFormB ->
            Result.map2 (==) (evaluateSafe subFormA variables) (evaluateSafe subFormB variables)

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
