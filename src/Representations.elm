module Representations exposing (..)

import ANF exposing (calculateANF, listToANF)
import BoolImpl exposing (..)
import Browser.Navigation exposing (Key, replaceUrl)
import Dict exposing (Dict)
import Html exposing (Html, div, h4, input, p, table, td, text, th, tr)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onInput)
import NormalForms exposing (calculateCNF, calculateDNF, calculateNNF, replaceImplXor)
import Parser exposing (DeadEnd, run)
import Properties exposing (FormulaProperties, calculateProperties, calculateTruthTable)
import Result.Extra
import Set
import Url exposing (Url)
import ViewHelpers exposing (boolToSymbol)



-- Model


type alias Model =
    { formulaInput : String
    , list : List BoolImpl.Formula
    , formulaInputParsed : Result (List DeadEnd) Formula
    , key : Key
    , url : Url
    }


initModel : String -> Key -> Url -> Model
initModel urlString key url =
    { formulaInput = preprocessString urlString
    , list = []
    , formulaInputParsed = run formula_p (preprocessString urlString)
    , key = key
    , url = url
    }



-- Update


type Msg
    = InputChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged newInput ->
            let
                preprocessedInput =
                    preprocessString newInput

                oldUrl =
                    model.url

                newUrl =
                    { oldUrl | fragment = Just (reversePreprocessString preprocessedInput) }
            in
            ( { model | formulaInput = preprocessedInput, formulaInputParsed = run formula_p preprocessedInput, url = newUrl }, replaceUrl model.key (Url.toString newUrl) )



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
                    [ renderProperties formula
                    , renderNormalForm "ANF" formula (\f -> listToANF (calculateANF f))
                    , renderNormalForm "NNF" formula calculateNNF
                    , renderNormalForm "CNF" formula calculateCNF
                    , renderNormalForm "DNF" formula calculateDNF
                    , renderTruthTable formula
                    ]

                _ ->
                    []
            )
        ]


renderProperties : Formula -> Html Msg
renderProperties formula =
    let
        properties =
            calculateProperties formula
    in
    div [ class "box content" ]
        [ h4 [] [ text "Properties" ]
        , table []
            [ tr []
                [ td [] [ text "Tautology" ]
                , td [] [ text (boolToSymbol properties.tautology) ]
                ]
            , tr []
                [ td [] [ text "Satisfiable" ]
                , td [] [ text (boolToSymbol properties.satisfiable) ]
                ]
            , tr []
                [ td [] [ text "Contradiction" ]
                , td [] [ text (boolToSymbol properties.contradiction) ]
                ]
            ]
        ]


renderNormalForm : String -> Formula -> (Formula -> Formula) -> Html Msg
renderNormalForm title formula calculateNormalForm =
    let
        normalForm =
            calculateNormalForm formula
    in
    div [ class "box content" ]
        [ h4 [] [ text title ]
        , text (toString normalForm)

        --, p [] [ text (Debug.toString normalForm) ]
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
