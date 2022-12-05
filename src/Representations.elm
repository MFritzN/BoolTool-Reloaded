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
import NormalForms exposing (calculateCNF)
import NormalForms exposing (distrCNF)
import NormalForms exposing (calculateDNF)
import NormalForms exposing (distrDNF)
import NormalForms exposing (replaceImplXor)
import NormalForms exposing (calculateNNF)



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
                    , renderNormalForm "ANF" formula (\f -> listToANF (calculateANF f))
                    , renderNormalForm "NNF" formula calculateNNF
                    , renderNormalForm "CNF" formula calculateCNF
                    , renderNormalForm "DNF" formula calculateDNF
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