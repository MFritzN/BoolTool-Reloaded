module Representations exposing (..)

import ANF exposing (calculateANF, listToANF)
import BoolImpl exposing (..)
import Browser.Navigation exposing (Key, replaceUrl)
import Color
import Graph exposing (Graph)
import Html exposing (Html, a, button, div, h4, h5, header, i, input, p, span, table, td, text, th, tr)
import Html.Attributes exposing (attribute, class, placeholder, readonly, style, value)
import Html.Events exposing (onClick, onInput)
import List.Extra
import NormalForms exposing (calculateCNF, calculateDNF, calculateNNF)
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
import ViewHelpers exposing (boolToSymbol, syntax)



-- Model


type alias Model =
    { formulaInput : String
    , list : List BoolImpl.Formula
    , formulaInputParsed : Result (List (DeadEnd Context Problem)) Formula
    , key : Key
    , url : Url
    , variableOrder : List String
    , expandedLaTeX : Set String
    , showUsage : Basics.Bool
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
    , showUsage = Basics.False
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
    | UsageUpdate


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

        UsageUpdate ->
            ( { model | showUsage = not model.showUsage }, Cmd.none )



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
                    p [] [ span [] [ text "Parsed Input: " ], text <| toString formula ]

                Err x ->
                    p [ class "help is-danger" ] [ parserError x model.formulaInput ]
            ]
        , div []
            (usage model.showUsage
                :: (case model.formulaInputParsed of
                        Ok formula ->
                            [ div [ class "columns" ]
                                [ div [ class "column" ]
                                    [ renderProperties formula
                                    , renderNormalForm "Negation Normal Form" formula calculateNNF model.expandedLaTeX
                                    ]
                                , div [ class "column" ]
                                    [ renderNormalForm "Conjunctive Normal Form" formula calculateCNF model.expandedLaTeX
                                    , renderNormalForm "Disjunctive Normal Form" formula calculateDNF model.expandedLaTeX
                                    , renderNormalForm "Algebraic Normal Form" formula (\f -> listToANF (calculateANF f)) model.expandedLaTeX
                                    ]
                                ]
                            , div [ class "is-hidden-mobile" ] [ renderOBDD formula model.variableOrder Basics.False ]
                            , div [ class "is-hidden-tablet" ] [ renderOBDD formula model.variableOrder Basics.True ]
                            , renderTruthTable formula
                            ]

                        _ ->
                            []
                   )
            )
        ]


usage : Basics.Bool -> Html Msg
usage showContent =
    div [ class "card mb-4" ]
        (header [ class "card-header" ]
            [ p [ class "card-header-title" ] [ text "Usage" ]
            , button [ class "card-header-icon", onClick UsageUpdate, attribute "aria-label" "more options" ]
                [ span [ class "icon" ]
                    [ i
                        [ class
                            (if showContent then
                                "fas fa-angle-up"

                             else
                                "fas fa-angle-down"
                            )
                        , attribute "aria-hidden" "true"
                        ]
                        []
                    ]
                ]
            ]
            :: (if showContent then
                    [ div [ class "card-content columns" ]
                        [ div [ class "column content" ]
                            [ h5 [ class "subtitle" ] [ text "Syntax" ]
                            , syntax
                            ]
                        , div [ class "column content" ]
                            [ h5 [ class "subtitle" ] [ text "Features" ]
                            , p [] [ text "To process a formula, enter it in the text field. The representations will automatically be updated." ]
                            , p [] [ text "You can share your input by copying the URL or using the share button in the top right corner." ]
                            , p [] [ text "It is possible to export outputs in a LaTeX format by clicking the LaTeX button and copying the text." ]
                            ]
                        ]
                    ]

                else
                    []
               )
        )


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
    div [ class "field has-addons" ]
        [ div [ class "control is-expanded" ] [ input [ value laTeX, class "input copy-input is-small", readonly Basics.True ] [] ]
        , div [ class "control" ] [ button [ class "button is-small", onClick <| Copy laTeX ] [ i [ class "fa-regular fa-clipboard" ] [] ] ]
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


renderOBDD : Formula -> List String -> Basics.Bool -> Html Msg
renderOBDD formula variableOrder isMobile =
    let
        graph =
            computeOBDD formula variableOrder
    in
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
                    , RSDA.fill
                        (\_ ->
                            Color.rgb255 105 188 252
                        )
                    , RSDA.strokeColor (\_ -> Color.rgb255 74 74 74)
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
                    , RSDA.strokeColor (\_ -> Color.rgb255 74 74 74)
                    ]
                )

            -- Calculate width depending on the amount of nodes and the display size
            , R.style <|
                "width: "
                    ++ (if isMobile then
                            String.fromInt <| Basics.min 100 <| Basics.max 40 <| 10 * (Set.size <| Set.fromList <| List.map (\node -> node.label) <| Graph.nodes graph)

                        else
                            String.fromInt <| Basics.min 80 <| Basics.max 20 <| 5 * (Set.size <| Set.fromList <| List.map (\node -> node.label) <| Graph.nodes graph)
                       )
                    ++ "%; max-height: 95vh; margin-left: auto; margin-right: auto; display: block"
            ]
            graph
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
