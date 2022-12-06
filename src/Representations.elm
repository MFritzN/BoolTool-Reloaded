module Representations exposing (..)

import ANF exposing (calculateANF, listToANF)
import BoolImpl exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, div, h4, input, p, table, td, text, th, tr)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onInput)
import NormalForms exposing (calculateCNF, calculateDNF, calculateNNF, replaceImplXor)
import Parser exposing (DeadEnd, run)
import Result.Extra
import Set



-- Model


type alias Model =
    { formulaInput : String
    , list : List BoolImpl.Formula
    , formulaInputParsed : Result (List DeadEnd) Formula
    }


initModel : String -> Model
initModel urlString =
    { formulaInput = preprocessString urlString
    , list = []
    , formulaInputParsed = run formula_p (preprocessString urlString)
    }



-- Update


type Msg
    = InputChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged newInput ->
            ( { model | formulaInput = preprocessString newInput, formulaInputParsed = run formula_p (preprocessString newInput) }, Cmd.none )



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
                    [ renderNormalForm "ANF" formula (\f -> listToANF (calculateANF f))
                    , renderNormalForm "NNF" formula calculateNNF
                    , renderNormalForm "CNF" formula calculateCNF
                    , renderNormalForm "DNF" formula calculateDNF
                    , renderTruthTable formula
                    ]

                _ ->
                    []
            )
        ]



-- ANF - Represented as a List of Lists of Strings whereas Strings represent Variables, a inner list conjunctions and the outer list Disjunctions


renderNormalForm : String -> Formula -> (Formula -> Formula) -> Html Msg
renderNormalForm title formula calculateNormalForm =
    let
        normalForm =
            calculateNormalForm formula
    in
    div [ class "box content" ]
        [ h4 [] [ text title ]
        , text (toString normalForm)
        ]


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
