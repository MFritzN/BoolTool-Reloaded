module Main exposing (..)

import BoolImpl exposing (..)
import Browser
import Html exposing (Attribute, Html, button, div, input, li, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra
import Parser exposing (DeadEnd, run)
import Set exposing (Set)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { content : String
    , list : List BoolImpl.Formula
    , formula : Result (List DeadEnd) Formula
    }


init : Model
init =
    { content = ""
    , list = []
    , formula = run formula_p ""
    }



-- UPDATE


type Msg
    = Change String
    | AddToSet
    | RemoveFromSet Int


resultOk : Result a b -> Bool
resultOk result =
    case result of
        Ok _ ->
            Basics.True

        Err _ ->
            Basics.False


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent, formula = run formula_p newContent }

        AddToSet ->
            case model.formula of
                Ok result ->
                    if List.any (\el -> BoolImpl.equals el result) model.list then
                        { model | list = model.list }

                    else
                        { model | list = result :: model.list }

                Err _ ->
                    { model | list = model.list }

        RemoveFromSet index ->
            { model | list = List.Extra.removeAt index model.list }



-- VIEW


renderFunctionSet : List Formula -> Html Msg
renderFunctionSet list =
    ul []
        (List.indexedMap (\index formula -> li [] [ text (BoolImpl.toString formula), button [ onClick (RemoveFromSet index) ] [ text "remove" ] ]) list)


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Formula Input", value model.content, onInput Change ] []
        , div []
            [ text
                (case model.formula of
                    Ok formula ->
                        BoolImpl.toString formula

                    Err err ->
                        Debug.toString err
                )
            ]
        , div []
            [ button [ onClick AddToSet ] [ text "Add to Set" ]
            , div []
                [ renderFunctionSet model.list
                ]
            ]
        ]
