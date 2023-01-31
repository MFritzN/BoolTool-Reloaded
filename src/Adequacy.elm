module Adequacy exposing (..)

import Adequacy.Affinity exposing (existsIsNotAffine, isNotAffine)
import Adequacy.Monotonicity exposing (exsistsIsNotMonotone, renderMonotone)
import Adequacy.SelfDualness exposing (exsistsIsNotSelfDual, renderSelfDualness)
import BoolImpl exposing (..)
import Browser.Navigation exposing (Key)
import Dict exposing (..)
import Html exposing (Html, button, div, h4, h5, header, i, input, p, span, table, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Json exposing (string)
import List.Extra
import Parser.Advanced exposing (DeadEnd, run, variable)
import ParserError exposing (parserError)
import Representations.ANF as ANF
import Result.Extra
import Set
import Svg.Attributes
import Url exposing (Url)
import ViewHelpers exposing (boolToSymbol, syntax)



-- MODEL


type alias Model =
    { setInput : String
    , list : List BoolImpl.Formula
    , setInputParsed : Result (Html Msg) (List Formula)
    , key : Key
    , url : Url
    , showUsage : Basics.Bool
    }


initModel : String -> Key -> Url -> Model
initModel string key url =
    { setInput = preprocessString string
    , list = []
    , setInputParsed = parseInputSet (preprocessString string)
    , key = key
    , url = url
    , showUsage = Basics.False
    }



-- UPDATE


type Msg
    = InputChanged String
    | AddToSet
    | RemoveFromSet Int
    | UsageUpdate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged newInput ->
            ( { model | setInput = preprocessString newInput, setInputParsed = parseInputSet (preprocessString newInput) }, Cmd.none )

        AddToSet ->
            case model.setInputParsed of
                Ok inputList ->
                    let
                        oldUrl =
                            model.url

                        newSet =
                            model.list ++ List.filter (\inputFormula -> not (List.any (equals inputFormula) model.list)) inputList

                        newUrl =
                            { oldUrl | fragment = Just (prettyPrintToURL (functionSetToString newSet)) }
                    in
                    ( { model | list = newSet, setInput = "", setInputParsed = parseInputSet "", url = newUrl }, Browser.Navigation.replaceUrl model.key (Url.toString newUrl) )

                Err _ ->
                    ( { model | list = model.list }, Cmd.none )

        RemoveFromSet index ->
            ( { model | list = List.Extra.removeAt index model.list }, Cmd.none )

        UsageUpdate ->
            ( { model | showUsage = not model.showUsage }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "box" ]
            [ h4 [ class "title is-4" ] [ text "Adequacy" ]
            , div [ onEnter AddToSet ]
                [ div [ class "field has-addons" ]
                    [ div [ class "control is-expanded" ]
                        [ input
                            [ if Result.Extra.isOk model.setInputParsed then
                                class ""

                              else
                                class "is-danger"
                            , placeholder <|
                                "Function Input"
                                    ++ (if List.isEmpty model.list then
                                            "- Try something like a & b, ~a"

                                        else
                                            ""
                                       )
                            , value model.setInput
                            , onInput InputChanged
                            , class "input avoid-cursor-jump level"
                            ]
                            []
                        ]
                    , div [ class "control" ] [ button [ onClick AddToSet, class "button" ] [ text "Add to Set" ] ]
                    ]
                , case model.setInputParsed of
                    Ok list ->
                        p [] [ span [] [ text "Parsed Input: " ], text <| functionSetToString list ]

                    Err x ->
                        p [ class "help is-danger" ] [ x ]
                ]
            ]
        , usage model.showUsage
        , renderPostConditions model.list
        ]


renderFunctionSet : List Formula -> Html Msg
renderFunctionSet list =
    div [ class "tags are-normal box" ]
        (List.indexedMap (\index formula -> span [ class "tag" ] [ text (BoolImpl.toString formula), button [ onClick (RemoveFromSet index), class "delete" ] [] ]) list)


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
                            , p [] [ text "To add a function, enter it in the text field. Add it by clicking the button. You can add multiple functions by seperating them with a comma." ]
                            , p [] [ text "The last row of the table will become green if the set of functions is adequat." ]
                            , p [] [ text "You can share your input by copying the URL or using the share button in the top right corner." ]
                            ]
                        ]
                    ]

                else
                    []
               )
        )


renderPostConditions : List Formula -> Html Msg
renderPostConditions list =
    if List.isEmpty list then
        text ""

    else
        table [ class "table is-narrow box" ]
            (tr []
                [ th [] [ text "Function" ]
                , th [] [ text "f (0,...,0) ≠ 0: " ]
                , th [] [ text "f (1,...,1) ≠ 1: " ]
                , th [] [ text "not monotone:" ]
                , th [] [ text "not self-dual:" ]
                , th [] [ text "not affine:" ]
                , th [] [ text "adequat" ]
                ]
                :: List.indexedMap
                    (\index formula ->
                        tr []
                            [ td [] [ span [ class "tag" ] [ text (functionHeaderToString (Set.toList <| getVariables formula) ++ " = " ++ toString formula), button [ onClick (RemoveFromSet index), class "delete" ] [] ] ]
                            , td []
                                [ text (boolToSymbol (allInputNotEqInput formula Basics.False)) ]
                            , td []
                                [ text (boolToSymbol (allInputNotEqInput formula Basics.True)) ]
                            , td []
                                [ renderMonotone formula ]
                            , td []
                                [ renderSelfDualness formula ]
                            , td []
                                [ span [ attribute "data-tooltip" ("ANF: " ++ (toString <| ANF.listToANF <| ANF.calculateANF formula)) ] [ text (boolToSymbol (isNotAffine formula)) ] ]
                            , td []
                                [ text (boolToSymbol (isAdequat [ formula ])) ]
                            ]
                    )
                    list
                ++ [ tr
                        [ class
                            (if isAdequat list then
                                "has-bg-success"

                             else
                                "has-bg-warning"
                            )
                        ]
                        [ td [] [ span [ class "tag" ] [ text "exists" ] ]
                        , td []
                            [ text (boolToSymbol (existsAllInputNotEqInput list Basics.False)) ]
                        , td []
                            [ text (boolToSymbol (existsAllInputNotEqInput list Basics.True)) ]
                        , td []
                            [ text (boolToSymbol (exsistsIsNotMonotone list)) ]
                        , td []
                            [ text (boolToSymbol (exsistsIsNotSelfDual list)) ]
                        , td []
                            [ text (boolToSymbol (existsIsNotAffine list)) ]
                        , td []
                            [ text (boolToSymbol (isAdequat list)) ]
                        ]
                   ]
            )


onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)


functionSetToString : List Formula -> String
functionSetToString list =
    List.foldl (\formula string -> string ++ ", " ++ toString formula) "" list
        |> String.dropLeft 2



-- OTHER FUNCTIONS


parseInputSet : String -> Result (Html Msg) (List Formula)
parseInputSet input =
    input
        |> String.split ","
        |> (\list ->
                if List.any (\string -> String.length string == 0) list then
                    Err (text "Input contains an empty function")

                else
                    Ok list
           )
        |> (\result -> Result.map (List.map (\stringFormula -> ( run formula_p stringFormula, stringFormula ))) result)
        |> (\result -> Result.andThen (\list -> parseInputSetHelp [] 0 list) result)


parseInputSetHelp : List Formula -> Int -> List ( Result (List (DeadEnd Context Problem)) Formula, String ) -> Result (Html Msg) (List Formula)
parseInputSetHelp returnList counter inputList =
    case inputList of
        [] ->
            Ok returnList

        ( Err error, string ) :: _ ->
            Err (parserError error string)

        ( Ok a, _ ) :: tail ->
            if List.any (equals a) returnList then
                Err <| text <| "There is a duplicate in here: " ++ toString a

            else
                parseInputSetHelp (returnList ++ [ a ]) (counter + 1) tail


isAdequat : List Formula -> Basics.Bool
isAdequat list =
    List.all (\a -> a) [ existsAllInputNotEqInput list Basics.False, existsAllInputNotEqInput list Basics.True, exsistsIsNotMonotone list, existsIsNotAffine list, exsistsIsNotSelfDual list ]


{-| Check if any of the boolean functions does not result in x for all inputs x: ∃formula ∈ List such that f (x,...,x) ≠ x

    existsAllInputNotEqInput [a & b] True = (True && True /= True) = False

-}
existsAllInputNotEqInput : List Formula -> Basics.Bool -> Basics.Bool
existsAllInputNotEqInput list x =
    List.any (\formula -> allInputNotEqInput formula x) list


{-| Check if a boolean function does not result in x for all inputs x: f (x,...,x) ≠ x

    allInputNotEqInput (a & b) True = (True && True /= True) = False

-}
allInputNotEqInput : Formula -> Basics.Bool -> Basics.Bool
allInputNotEqInput formula x =
    evaluateUnsafe formula (Dict.fromList (List.map (\variable -> ( variable, x )) (Set.toList (getVariables formula)))) /= x
