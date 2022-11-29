module Adequacy exposing (..)

import BoolImpl exposing (..)
import Dict exposing (..)
import Parser exposing (variable)
import Set
import Html.Attributes exposing (..)
import Html exposing (div, button, input, table, tr, td, th, span, text, Html)
import Html.Events exposing (onInput, onClick)
import Parser exposing (DeadEnd, run)
import List.Extra
import Maybe exposing (andThen)
import Html.Events exposing (keyCode, on)
import Json.Decode as Json
import Representations

-- Model
type alias Model =
    { functionInput : String
    , list : List BoolImpl.Formula
    , functionInputParsed : Result (List DeadEnd) Formula
    }

initModel : String -> Model
initModel _ = {functionInput = ""
                , list = []
                , functionInputParsed = run formula_p ""}

-- Update

type Msg
    = InputChanged String
    | AddToSet
    | RemoveFromSet Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        InputChanged newInput ->
            ( { model | functionInput = newInput, functionInputParsed = run formula_p newInput }, Cmd.none)

        AddToSet ->
            case model.functionInputParsed of
                Ok result ->
                    if List.any (\el -> BoolImpl.equals el result) model.list then
                        ({ model | list = model.list, functionInput = "" }, Cmd.none)

                    else
                        ({ model | list = result :: model.list }, Cmd.none)

                Err _ ->
                    ({ model | list = model.list }, Cmd.none)

        RemoveFromSet index ->
            ({ model | list = List.Extra.removeAt index model.list }, Cmd.none)

-- View

renderFunctionSet : List Formula -> Html Msg
renderFunctionSet list =
    div [class "tags are-normal"]
        (List.indexedMap (\index formula -> span [class "tag"] [ text (BoolImpl.toString formula), button [ onClick (RemoveFromSet index), class "delete" ] []]) list)

renderPostConditions : List Formula -> Html Msg
renderPostConditions list =
    if List.isEmpty list then
        text ""

    else
        table [class "table is-narrow"]
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
                                [ if allInputNotEqInput formula Basics.False then
                                    text "✓"

                                  else
                                    text "✕"
                                ]
                            , td []
                                [ if allInputNotEqInput formula Basics.True then
                                    text "✓"

                                  else
                                    text "✕"
                                ]
                            , td []
                                [ if isNotMontone formula then
                                    text "✓"

                                  else
                                    text "✕"
                                ]
                            , td []
                                [ if isNotSelfDual formula then
                                    text "✓"

                                  else
                                    text "✕"
                                ]
                            , td []
                                [ if isNotAffine formula then
                                    text "✓"

                                  else
                                    text "✕"
                                ]
                            , td []
                                [ if isAdequat [ formula ] then
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
                            [ if existsAllInputNotEqInput list Basics.False then
                                text "✓"

                              else
                                text "✕"
                            ]
                        , td []
                            [ if existsAllInputNotEqInput list Basics.True then
                                text "✓"

                              else
                                text "✕"
                            ]
                        , td []
                            [ if exsistsIsNotMonotone list then
                                text "✓"

                              else
                                text "✕"
                            ]
                        , td []
                            [ if exsistsIsNotSelfDual list then
                                text "✓"

                              else
                                text "✕"
                            ]
                        , td []
                            [ if existsIsNotAffine list then
                                text "✓"

                              else
                                text "✕"
                            ]
                        , td []
                            [ if isAdequat list then
                                text "✓"

                              else
                                text "✕"
                            ]
                        ]
                   ]
            )

view : Model -> Html Msg
view model = div [] [
    div [onEnter AddToSet] [
        input [ placeholder "Function Input", value model.functionInput, onInput InputChanged, class "input" ] []
        , text (case model.functionInputParsed of
            Ok formula -> toString formula
            Err x -> Debug.toString x
            )
        , button [ onClick AddToSet, class "button" ] [ text "Add to Set" ]
    ]
    , renderFunctionSet model.list
    , renderPostConditions model.list
    ]

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

-- Logic

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
    evaluate formula (Dict.fromList (List.map (\variable -> ( variable, x )) (Set.toList (getVariables formula)))) /= x



-- is not montone


exsistsIsNotMonotone : List Formula -> Basics.Bool
exsistsIsNotMonotone list =
    List.any isNotMontone list


isNotMontone : Formula -> Basics.Bool
isNotMontone formula =
    let
        variables =
            Dict.fromList (List.map (\variable -> ( variable, Basics.False )) (Set.toList (getVariables formula)))
    in
    isNotMonotoneHelp formula variables (Dict.keys variables)


isNotMonotoneHelp : Formula -> Dict String Bool -> List String -> Bool
isNotMonotoneHelp formula variables remainingVariables =
    case remainingVariables of
        [] ->
            case iterateVariables variables of
                Nothing ->
                    Basics.False

                Just newVariables ->
                    isNotMonotoneHelp formula newVariables (Dict.keys newVariables)

        currentVar :: remainingVariablesTail ->
            if not (BoolImpl.evaluate formula (Dict.insert currentVar Basics.True variables)) && BoolImpl.evaluate formula (Dict.insert currentVar Basics.False variables) then
                Basics.True

            else
                isNotMonotoneHelp formula variables remainingVariablesTail



-- is not self-dual


exsistsIsNotSelfDual : List Formula -> Basics.Bool
exsistsIsNotSelfDual list =
    List.any isNotSelfDual list


isNotSelfDual : Formula -> Basics.Bool
isNotSelfDual formula =
    let
        variables =
            Dict.fromList (List.map (\variable -> ( variable, Basics.False )) (Set.toList (getVariables formula)))
    in
    isNotSelfDualHelp formula variables


isNotSelfDualHelp : Formula -> Dict String Bool -> Bool
isNotSelfDualHelp formula variables =
    let
        inverse_variables =
            Dict.map (\_ v -> not v) variables
    in
    if evaluate formula variables == evaluate formula inverse_variables then
        Basics.True

    else
        case iterateVariables variables of
            Nothing ->
                Basics.False

            Just newVariables ->
                isNotSelfDualHelp formula newVariables



-- is not affine


existsIsNotAffine : List Formula -> Basics.Bool
existsIsNotAffine formula =
    List.any isNotAffine formula


isNotAffine : Formula -> Basics.Bool
isNotAffine formula =
    Representations.calculateANF formula
        |> List.map List.length
        |> List.maximum
        |> Maybe.andThen (\x -> Just (x > 1))
        |> Maybe.withDefault Basics.False



-- adequacy


isAdequat : List Formula -> Basics.Bool
isAdequat list =
    List.all (\a -> a) [ existsAllInputNotEqInput list Basics.False, existsAllInputNotEqInput list Basics.True, exsistsIsNotMonotone list, existsIsNotAffine list, exsistsIsNotSelfDual list ]
