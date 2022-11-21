module Main exposing (..)

import Adequacy
import BoolImpl exposing (..)
import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, input, li, table, td, text, th, tr, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra
import Parser exposing (DeadEnd, run)
import Representations



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { content : String
    , list : List BoolImpl.Formula
    , formula : Result (List DeadEnd) Formula
    , anf : Maybe Formula
    }


init : Model
init =
    { content = ""
    , list = []
    , formula = run formula_p ""
    , anf = Nothing
    }



-- UPDATE


type Msg
    = Change String
    | AddToSet
    | RemoveFromSet Int
    | ComputeANF Int


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

        ComputeANF index ->
            { model | anf = Maybe.andThen (\a -> Just (Representations.listToANF (Representations.calculateANF a))) (List.Extra.getAt index model.list) }

        RemoveFromSet index ->
            { model | list = List.Extra.removeAt index model.list }



-- VIEW


renderFunctionSet : List Formula -> Html Msg
renderFunctionSet list =
    ul []
        (List.indexedMap (\index formula -> li [] [ text (BoolImpl.toString formula), button [ onClick (RemoveFromSet index) ] [ text "remove" ], button [ onClick (ComputeANF index) ] [ text "ANF" ] ]) list)


renderPostConditions : List Formula -> Html Msg
renderPostConditions list =
    if List.isEmpty list then
        text ""

    else
        table []
            (tr []
                [ th [] [ text "Function" ]
                , th [] [ text "∃f ∈ X such that f (0,...,0) ≠ 0: " ]
                , th [] [ text "∃f ∈ X such that f (1,...,1) ≠ 1: " ]
                , th [] [ text "∃f ∈ X which is not monotone:" ]
                , th [] [ text "∃f ∈ X which is not self-dual:" ]
                , th [] [ text "∃f ∈ X which is not affine:" ]
                , th [] [ text "adequat" ]
                ]
                :: List.map
                    (\formula ->
                        tr []
                            [ td [] [ text (toString formula) ]
                            , td []
                                [ if Adequacy.allInputNotEqInput formula Basics.False then
                                    text "✓"

                                  else
                                    text "✕"
                                ]
                            , td []
                                [ if Adequacy.allInputNotEqInput formula Basics.True then
                                    text "✓"

                                  else
                                    text "✕"
                                ]
                            , td []
                                [ if Adequacy.isNotMontone formula then
                                    text "✓"

                                  else
                                    text "✕"
                                ]
                            , td []
                                [ if Adequacy.isNotSelfDual formula then
                                    text "✓"

                                  else
                                    text "✕"
                                ]
                            , td []
                                [ if Adequacy.isNotAffine formula then
                                    text "✓"

                                  else
                                    text "✕"
                                ]
                            , td []
                                [ if Adequacy.isAdequat [ formula ] then
                                    text "✓"

                                  else
                                    text "✕"
                                ]
                            ]
                    )
                    list
                ++ [ tr []
                        [ td [] [ text "exists" ]
                        , td []
                            [ if Adequacy.existsAllInputNotEqInput list Basics.False then
                                text "✓"

                              else
                                text "✕"
                            ]
                        , td []
                            [ if Adequacy.existsAllInputNotEqInput list Basics.True then
                                text "✓"

                              else
                                text "✕"
                            ]
                        , td []
                            [ if Adequacy.exsistsIsNotMonotone list then
                                text "✓"

                              else
                                text "✕"
                            ]
                        , td []
                            [ if Adequacy.exsistsIsNotSelfDual list then
                                text "✓"

                              else
                                text "✕"
                            ]
                        , td []
                            [ if Adequacy.existsIsNotAffine list then
                                text "✓"

                              else
                                text "✕"
                            ]
                        , td []
                            [ if Adequacy.isAdequat list then
                                text "✓"

                              else
                                text "✕"
                            ]
                        ]
                   ]
            )



{- ul []
   [ li []
       [ text
           ("∃f ∈ X such that f (0,...,0) ≠ 0: "
               ++ (if Adequacy.existsAllInputNotEqInput list Basics.True then
                       "forfilled"

                   else
                       "not forfilled"
                  )
           )
       ]
   , li []
       [ text
           ("∃f ∈ X such that f (1,...,1) ≠ 1: "
               ++ (if Adequacy.existsAllInputNotEqInput list Basics.False then
                       "forfilled"

                   else
                       "not forfilled"
                  )
           )
       ]
   , li []
       [ text
           ("∃f ∈ X which is not monotone: "
               ++ (if Adequacy.exsistsIsNotMonotone list then
                       "forfilled"

                   else
                       "not forfilled"
                  )
           )
       ]
   , li []
       [ text
           ("∃f ∈ X which is not self-dual: "
               ++ (if Adequacy.exsistsIsNotSelfDual list then
                       "forfilled"

                   else
                       "not forfilled"
                  )
           )
       ]
   ]
-}


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
            , text (Maybe.withDefault "" (Maybe.andThen (\a -> Just (toString a)) model.anf))
            ]
        , div []
            [ div []
                [ renderPostConditions model.list
                ]
            ]
        ]
