module Representations exposing (..)

import BoolImpl exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, div, h4, input, p, table, td, text, th, tr)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onInput)
import Parser exposing (DeadEnd, run)
import Result.Extra
import Set
import ANF exposing (calculateANF, listToANF)



-- Model


type alias Model =
    { formulaInput : String
    , list : List BoolImpl.Formula
    , formulaInputParsed : Result (List DeadEnd) Formula
    }


initModel : String -> Model
initModel urlString =
    { formulaInput = urlString
    , list = []
    , formulaInputParsed = run formula_p urlString
    }



-- Update


type Msg
    = InputChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged newInput ->
            ( { model | formulaInput = newInput, formulaInputParsed = run formula_p newInput }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div []
        [ div [ class "field" ]
            [ input
                ((if Result.Extra.isOk model.formulaInputParsed then
                    class "is-success"

                  else
                    class "is-danger"
                 )
                    :: [ placeholder "Formula Input", value model.formulaInput, onInput InputChanged, class "input" ]
                )
                []
            , case model.formulaInputParsed of
                Ok formula ->
                    text (toString formula)

                Err x ->
                    p [ class "help is-danger" ] [ text (Debug.toString x) ]
            ]
        , div []
            (case model.formulaInputParsed of
                Ok formula ->
                    [ renderTruthTable formula
                    , renderANF formula
                    , renderNNF formula
                    , renderCNF formula
                    , renderDNF formula
                    ]

                _ ->
                    []
            )
        ]



-- ANF - Represented as a List of Lists of Strings whereas Strings represent Variables, a inner list conjunctions and the outer list Disjunctions


renderANF : Formula -> Html Msg
renderANF formula =
    let
        anf =
            calculateANF formula
    in
    div [ class "box content" ]
        [ h4 [] [ text "ANF" ]
        , text (toString (listToANF anf))
        ]

-- TruthTable
-- View


renderTruthTable : Formula -> Html Msg
renderTruthTable formula =
    let
        truthTable =
            calculateTruthTable formula
    in
    div [ class "content box" ]
        [ h4 [] [ text "Truth Table" ]
        , table [ class "table is-narrow is-striped is-bordered" ]
            (tr [] (List.map (\variable -> th [] [ text variable ]) truthTable.vars ++ [ th [] [ text "Result" ] ])
                :: List.map (\row -> tr [] (List.map (\value -> td [] [ prettyPrintBool value ]) (Tuple.first row) ++ [ td [] [ prettyPrintBool (Tuple.second row) ] ]))
                    truthTable.results
            )
        ]


prettyPrintBool : Basics.Bool -> Html Msg
prettyPrintBool bool =
    if bool then
        text "True"

    else
        text "False"


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



-- NNF


renderNNF : Formula -> Html Msg
renderNNF formula =
    let
        nnf =
            calculateNNF formula
    in
    div [ class "box content" ]
        [ h4 [] [ text "NNF" ]
        , text (toString nnf)
        ]

{-| Replaces Implications (Impl) and Exclusive Ors (Xor) by equal statements using And, Or and Neg.
This is needed as a preprocessing step for `calculateNNF`.
-}
replaceImplXor : Formula -> Formula
replaceImplXor formula =
    case formula of
        Neg a ->
            Neg (replaceImplXor a)

        And a b ->
            And (replaceImplXor a) (replaceImplXor b)

        Or a b ->
            Or (replaceImplXor a) (replaceImplXor b)

        Impl a b ->
            Or (Neg (replaceImplXor a)) (replaceImplXor b)

        Xor a b ->
            replaceImplXor (Or (And a (Neg b)) (And (Neg a) b))

        a ->
            a

calculateNNF : Formula -> Formula
calculateNNF formula =
    case replaceImplXor formula of
        Neg (Neg a) ->
            calculateNNF a

        Neg (And a b) ->
            Or (calculateNNF (Neg a)) (calculateNNF (Neg b))

        Neg (Or a b) ->
            And (calculateNNF (Neg a)) (calculateNNF (Neg b))

        And a b ->
            And (calculateNNF a) (calculateNNF b)

        Or a b ->
            Or (calculateNNF a) (calculateNNF b)

        a ->
            a



-- CNF


renderCNF : Formula -> Html Msg
renderCNF formula =
    let
        cnf =
            calculateCNF formula
    in
    div [ class "box content" ]
        [ h4 [] [ text "CNF" ]
        , text (toString cnf)
        ]


calculateCNF : Formula -> Formula
calculateCNF formula =
    case calculateNNF formula of
        And a b ->
            And (calculateCNF a) (calculateCNF b)

        Or a b ->
            distrCNF (calculateCNF a) (calculateCNF b)

        a ->
            a


distrCNF : Formula -> Formula -> Formula
distrCNF formula1 formula2 =
    case ( formula1, formula2 ) of
        ( And formula11 formula12, _ ) ->
            And (distrCNF formula11 formula2) (distrCNF formula12 formula2)

        ( _, And formula21 formula22 ) ->
            And (distrCNF formula1 formula21) (distrCNF formula1 formula22)

        ( a, b ) ->
            Or a b



-- DNF


renderDNF : Formula -> Html Msg
renderDNF formula =
    let
        dnf =
            calculateDNF formula
    in
    div [ class "box content" ]
        [ h4 [] [ text "DNF" ]
        , text (toString dnf)
        ]


calculateDNF : Formula -> Formula
calculateDNF formula =
    case calculateNNF formula of
        Or a b ->
            Or (calculateDNF a) (calculateDNF b)

        And a b ->
            distrDNF (calculateDNF a) (calculateDNF b)

        a ->
            a


distrDNF : Formula -> Formula -> Formula
distrDNF formula1 formula2 =
    case ( formula1, formula2 ) of
        ( Or formula11 formula12, _ ) ->
            Or (distrDNF formula11 formula2) (distrDNF formula12 formula2)

        ( _, Or formula21 formula22 ) ->
            And (distrDNF formula1 formula21) (distrDNF formula1 formula22)

        ( a, b ) ->
            And a b
