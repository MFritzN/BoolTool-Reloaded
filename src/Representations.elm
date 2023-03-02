module Representations exposing (..)

import BoolImpl exposing (..)
import Browser.Navigation exposing (Key, replaceUrl)
import Html exposing (Html, button, div, h3, h5, header, i, input, p, span, text)
import Html.Attributes exposing (attribute, class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Parser.Advanced exposing (DeadEnd, run)
import ParserError exposing (parserError)
import Ports
import Render.StandardDrawers.Types exposing (Shape(..))
import Representations.NormalForms as NormalForms exposing (NormalForm(..))
import Representations.OBDD as OBDD
import Representations.Properties as Properties
import Representations.TruthTable as TruthTable
import Result.Extra
import Set
import Svg.Attributes
import Url exposing (Url)
import ViewHelpers exposing (renderBox, syntax)



-- MODEL


type alias Model =
    { formulaInput : String
    , list : List BoolImpl.Formula
    , formulaInputParsed : Result (List (DeadEnd Context Problem)) Formula
    , key : Key
    , url : Url
    , expandedLaTeX : Maybe NormalForms.NormalForm
    , showUsage : Basics.Bool
    , obdd : OBDD.Model (List (DeadEnd Context Problem))
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
    , expandedLaTeX = Nothing
    , showUsage = Basics.False
    , obdd = OBDD.initModel formulaInputParsed
    }



-- UPDATE


type Msg
    = InputChanged String
    | NormalFormsMSG NormalForms.Msg
    | Copy String
    | UsageUpdate
    | OBDDMsg OBDD.Msg


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
                    { oldUrl | fragment = Just (prettyPrintToURL preprocessedInput) }
            in
            ( { model | formulaInput = preprocessedInput, formulaInputParsed = formulaInputParsed, url = newUrl, obdd = OBDD.initModel formulaInputParsed }, replaceUrl model.key (Url.toString newUrl) )

        NormalFormsMSG (NormalForms.LaTeXClicked normalForm) ->
            ( { model
                | expandedLaTeX =
                    if model.expandedLaTeX == Just normalForm then
                        Nothing

                    else
                        Just normalForm
              }
            , Cmd.none
            )

        NormalFormsMSG (NormalForms.Copy toCopy) ->
            ( model, Ports.copy toCopy )

        Copy toCopy ->
            ( model, Ports.copy toCopy )

        UsageUpdate ->
            ( { model | showUsage = not model.showUsage }, Cmd.none )

        OBDDMsg obddMsg ->
            ( { model | obdd = Tuple.first (OBDD.update obddMsg model.obdd) }, Cmd.map (\o -> OBDDMsg o) <| Tuple.second <| OBDD.update obddMsg model.obdd )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "box" ]
            [ h3 [ class "title is-4" ] [ text "Representations" ]

            -- , p [] [ text "Try to type in a formula, like 'a & b'." ]
            , div [ class "field" ]
                [ input
                    [ if Result.Extra.isOk model.formulaInputParsed then
                        class "is-success"

                      else
                        class "is-danger"
                    , placeholder "Formula Input - Try to type something like a & b"
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
            ]
        , div []
            (usage model.showUsage
                :: renderRepresentations model
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
                        [ Svg.Attributes.class
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


renderRepresentations : Model -> List (Html Msg)
renderRepresentations model =
    case model.formulaInputParsed of
        Ok formula ->
            [ div [ class "columns" ]
                [ div [ class "column" ]
                    [ renderBox <| Properties.renderProperties formula
                    , renderBox <| mapNormalForm <| NormalForms.renderNormalForm NNF formula model.expandedLaTeX
                    ]
                , div [ class "column" ]
                    [ renderBox <| mapNormalForm <| NormalForms.renderNormalForm CNF formula model.expandedLaTeX
                    , renderBox <| mapNormalForm <| NormalForms.renderNormalForm DNF formula model.expandedLaTeX
                    , renderBox <| mapNormalForm <| NormalForms.renderNormalForm ANF formula model.expandedLaTeX
                    ]
                ]
            , Html.map (\o -> OBDDMsg o) (renderBox (OBDD.view model.obdd))
            , renderBox <| TruthTable.renderTruthTable formula
            ]

        _ ->
            []


mapNormalForm : { title : String, render : Html NormalForms.Msg } -> { title : String, render : Html Msg }
mapNormalForm input =
    { title = input.title, render = Html.map (\n -> NormalFormsMSG n) input.render }



-- OTHER FUNCTIONS


getVariableOrder : Result (List (DeadEnd Context Problem)) Formula -> List String
getVariableOrder formulaInputParsed =
    formulaInputParsed
        |> Result.map getVariables
        |> Result.withDefault Set.empty
        |> Set.toList
