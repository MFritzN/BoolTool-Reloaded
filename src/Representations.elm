module Representations exposing (..)

import ANF exposing (calculateANF, listToANF)
import BoolImpl exposing (..)
import Browser.Navigation exposing (Key, replaceUrl)
import Html exposing (Html, a, button, div, h4, i, input, label, p, span, table, td, text, th, tr)
import Html.Attributes exposing (class, placeholder, readonly, style, value)
import Html.Events exposing (onClick, onInput)
import List.Extra
import NormalForms exposing (calculateCNF, calculateDNF, calculateNNF, replaceImplXor)
import OBDD exposing (computeOBDD)
import Parser.Advanced exposing (DeadEnd, run)
import ParserError exposing (parserError)
import Ports
import Properties exposing (calculateProperties, calculateTruthTable)
import Render as R
import Render.StandardDrawers as RSD
import Render.StandardDrawers.Attributes as RSDA
import Render.StandardDrawers.Types exposing (Shape(..))
import Result.Extra
import Set exposing (Set)
import Url exposing (Url)
import ViewHelpers exposing (boolToSymbol)



-- Model


type alias Model =
    { formulaInput : String
    , list : List BoolImpl.Formula
    , formulaInputParsed : Result (List (DeadEnd Context Problem)) Formula
    , key : Key
    , url : Url
    , variableOrder : List String
    , expandedLaTeX : Set String
    }


initModel : String -> Key -> Url -> Model
initModel urlString key url =
    let
        formulaInput =
            preprocessString urlString

        formulaInputParsed =
            run formula_p formulaInput
    in
    { formulaInput = formulaInput
    , list = []
    , formulaInputParsed = formulaInputParsed
    , key = key
    , url = url
    , variableOrder = getVariableOrder formulaInputParsed
    , expandedLaTeX = Set.empty
    }


getVariableOrder : Result (List (DeadEnd Context Problem)) Formula -> List String
getVariableOrder formulaInputParsed =
    formulaInputParsed
        |> Result.map getVariables
        |> Result.withDefault Set.empty
        |> Set.toList



-- Update


type Msg
    = InputChanged String
    | VariableOrderChanged Int MoveTo
    | LaTeXClicked String
    | Copy String


type MoveTo
    = Front
    | Back


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged newInput ->
            let
                preprocessedInput =
                    preprocessString newInput

                formulaInputParsed =
                    run formula_p preprocessedInput

                oldUrl =
                    model.url

                newUrl =
                    { oldUrl | fragment = Just (reversePreprocessString preprocessedInput) }
            in
            ( { model | formulaInput = preprocessedInput, formulaInputParsed = formulaInputParsed, url = newUrl, variableOrder = getVariableOrder formulaInputParsed }, replaceUrl model.key (Url.toString newUrl) )

        VariableOrderChanged index direction ->
            let
                varibaleToMove =
                    List.Extra.getAt index model.variableOrder

                maybeVariableOrder =
                    List.map Just model.variableOrder
            in
            ( { model
                | variableOrder =
                    (case direction of
                        Front ->
                            List.Extra.updateAt index (\_ -> List.Extra.getAt (index + 1) model.variableOrder) maybeVariableOrder
                                |> List.Extra.updateAt (index + 1) (\_ -> varibaleToMove)
                                |> List.filterMap identity

                        Back ->
                            List.Extra.updateAt index (\_ -> List.Extra.getAt (index - 1) model.variableOrder) maybeVariableOrder
                                |> List.Extra.updateAt (index - 1) (\_ -> varibaleToMove)
                                |> List.filterMap identity
                    )
                        |> (\result ->
                                if List.length result /= List.length model.variableOrder then
                                    -- If some List.Extra calls were out of reach, the old list is kept. This code should not be reachable.
                                    model.variableOrder

                                else
                                    result
                           )
              }
            , Cmd.none
            )

        LaTeXClicked title ->
            let
                newSet =
                    if Set.member title model.expandedLaTeX then
                        Set.remove title model.expandedLaTeX

                    else
                        Set.insert title model.expandedLaTeX
            in
            ( { model | expandedLaTeX = newSet }, Cmd.none )

        Copy toCopy ->
            ( model, Ports.copy toCopy )



-- View


view : Model -> Html Msg
view model =
    div []
        [ div [ class "field box" ]
            [ input
                [ if Result.Extra.isOk model.formulaInputParsed then
                    class "is-success"

                  else
                    class "is-danger"
                , placeholder "Formula Input"
                , value model.formulaInput
                , onInput InputChanged
                , class "input avoid-cursor-jump"
                ]
                []
            , case model.formulaInputParsed of
                Ok formula ->
                    text (toString formula)

                Err x ->
                    p [ class "help is-danger" ] [ parserError x model.formulaInput ]
            ]
        , div []
            (case model.formulaInputParsed of
                Ok formula ->
                    [ renderProperties formula
                    , renderNormalForm "ANF" formula (\f -> listToANF (calculateANF f)) model.expandedLaTeX
                    , renderNormalForm "NNF" formula calculateNNF model.expandedLaTeX
                    , renderNormalForm "CNF" formula calculateCNF model.expandedLaTeX
                    , renderNormalForm "DNF" formula calculateDNF model.expandedLaTeX
                    , renderTruthTable formula
                    , renderOBDD formula model.variableOrder
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


renderNormalForm : String -> Formula -> (Formula -> Formula) -> Set String -> Html Msg
renderNormalForm title formula calculateNormalForm expandedLaTeX =
    let
        normalForm =
            calculateNormalForm formula
    in
    div [ class "box content" ]
        ([ h4 [] [ text title ]
         , text <| toString normalForm
         , button [ onClick <| LaTeXClicked title, class "button is-small", style "float" "right" ] [ text "LaTeX" ]
         ]
            ++ (if Set.member title expandedLaTeX then
                    [ renderLaTeX <| toString normalForm ]

                else
                    []
               )
        )


renderLaTeX : String -> Html Msg
renderLaTeX formula =
    let
        laTeX =
            prettyPrintToLaTeX formula
    in
    div [ class "field" ]
        [ label [ class "label" ] [ text "LaTeX to Copy" ]
        , input [ value laTeX, class "input copy-input", readonly Basics.True ] []
        , button [ class "button is-small", onClick <| Copy laTeX ] [ text "copy" ]
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


renderOBDD : Formula -> List String -> Html Msg
renderOBDD formula variableOrder =
    div [ class "box content" ]
        [ h4 [] [ text "OBDD" ]
        , div [ class "field is-grouped is-grouped-multiline" ]
            (List.indexedMap
                (\index variable ->
                    div [ class "control" ]
                        [ div [ class "tags has-addons" ]
                            [ span [ class "tag icon", style "cursor" "pointer", onClick (VariableOrderChanged index Back) ] [ i [ class "fas fa-solid fa-caret-left" ] [] ]
                            , span [ class "tag" ] [ text variable ]
                            , span [ class "tag icon", style "cursor" "pointer", onClick (VariableOrderChanged index Front) ] [ i [ class "fas fa-solid fa-caret-right" ] [] ]
                            ]
                        ]
                )
                variableOrder
            )
        , R.draw
            []
            [ R.nodeDrawer
                (RSD.svgDrawNode
                    [ RSDA.label (\a -> a.label)
                    , RSDA.shape
                        (\a ->
                            if a.id <= 1 then
                                Box

                            else
                                Circle
                        )
                    ]
                )
            , R.edgeDrawer
                (RSD.svgDrawEdge
                    [ RSDA.strokeDashArray
                        (\a ->
                            if a.label then
                                "0"

                            else
                                "2.5"
                        )
                    ]
                )
            , R.style "height: 50vh;"
            ]
            (computeOBDD formula variableOrder)
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
        , table [ class "table is-narrow is-striped is-hoverable is-bordered" ]
            (tr [] (List.map (\variable -> th [] [ text variable ]) truthTable.vars ++ [ th [] [ text "Result" ] ])
                :: List.map (\row -> tr [] (List.map (\value -> td [] [ prettyPrintBool value ]) (Tuple.first row) ++ [ td [] [ prettyPrintBool (Tuple.second row) ] ]))
                    truthTable.results
            )
        ]


prettyPrintBool : Basics.Bool -> Html Msg
prettyPrintBool bool =
    if bool then
        text "T"

    else
        text "F"



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
