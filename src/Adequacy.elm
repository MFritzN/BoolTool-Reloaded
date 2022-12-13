module Adequacy exposing (..)

import ANF
import BoolImpl exposing (..)
import Browser.Navigation exposing (Key)
import Dict exposing (..)
import Html exposing (Html, button, div, input, span, table, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Json exposing (string)
import List.Extra
import Maybe
import Parser exposing (DeadEnd, run, variable)
import Set
import Url exposing (Url)



-- Model


type InputError
    = ParserFailed Int (List DeadEnd)
    | FoundDuplicateInString Formula
    | MissingOpeningBracket
    | MissingClosingBracket


type alias Model =
    { setInput : String
    , list : List BoolImpl.Formula
    , setInputParsed : Result InputError (List Formula)
    , key : Key
    , url : Url
    }


initModel : String -> Key -> Url -> Model
initModel string key url =
    { setInput = preprocessString string
    , list = []
    , setInputParsed = parseInputSet (preprocessString string)
    , key = key
    , url = url
    }


parseInputSet : String -> Result InputError (List Formula)
parseInputSet input =
    stringToList input
        |> Result.map (List.map (\stringFormula -> run formula_p stringFormula))
        |> Result.andThen (parseInputSetHelp [] 0)


parseInputSetHelp : List Formula -> Int -> List (Result (List DeadEnd) Formula) -> Result InputError (List Formula)
parseInputSetHelp returnList counter inputList =
    case inputList of
        [] ->
            Ok returnList

        (Err a) :: _ ->
            Err (ParserFailed counter a)

        (Ok a) :: tail ->
            if List.any (equals a) returnList then
                Err (FoundDuplicateInString a)

            else
                parseInputSetHelp (returnList ++ [ a ]) (counter + 1) tail



-- Update


type Msg
    = InputChanged String
    | AddToSet
    | RemoveFromSet Int


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
                            { oldUrl | fragment = Just (reversePreprocessString (functionSetToString newSet)) }
                    in
                    ( { model | list = newSet, setInput = "", setInputParsed = parseInputSet "", url = newUrl }, Browser.Navigation.replaceUrl model.key (Url.toString newUrl) )

                Err _ ->
                    ( { model | list = model.list }, Cmd.none )

        RemoveFromSet index ->
            ( { model | list = List.Extra.removeAt index model.list }, Cmd.none )



-- View


renderFunctionSet : List Formula -> Html Msg
renderFunctionSet list =
    div [ class "tags are-normal box" ]
        (List.indexedMap (\index formula -> span [ class "tag" ] [ text (BoolImpl.toString formula), button [ onClick (RemoveFromSet index), class "delete" ] [] ]) list)


renderPostConditions : List Formula -> Html Msg
renderPostConditions list =
    if List.isEmpty list then
        text ""

    else
        table [ class "table is-narrow box" ]
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
                ++ [ tr [ class "is-selected" ]
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
view model =
    div []
        [ div [ onEnter AddToSet, class "box" ]
            [ input [ placeholder "Function Input", value model.setInput, onInput InputChanged, class "input" ] []
            , text
                (case model.setInputParsed of
                    Ok list ->
                        functionSetToString list

                    Err x ->
                        Debug.toString x
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


functionSetToString : List Formula -> String
functionSetToString list =
    List.foldl (\formula string -> string ++ ", " ++ toString formula) "" list
        |> String.dropLeft 2
        |> (\a ->
                "["
                    ++ a
                    ++ "]"
           )


stringToList : String -> Result InputError (List String)
stringToList string =
    let
        startsWithBracket =
            String.startsWith "[" string

        endsWithBracket =
            String.endsWith "]" string
    in
    case ( startsWithBracket, endsWithBracket ) of
        ( Basics.True, Basics.True ) ->
            string
                |> String.dropLeft 1
                |> String.dropRight 1
                |> String.split ","
                |> Ok

        ( Basics.False, Basics.True ) ->
            Err MissingOpeningBracket

        ( Basics.True, Basics.False ) ->
            Err MissingClosingBracket

        ( Basics.False, Basics.False ) ->
            Ok [ string ]



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
    evaluateUnsafe formula (Dict.fromList (List.map (\variable -> ( variable, x )) (Set.toList (getVariables formula)))) /= x



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
            if not (BoolImpl.evaluateUnsafe formula (Dict.insert currentVar Basics.True variables)) && BoolImpl.evaluateUnsafe formula (Dict.insert currentVar Basics.False variables) then
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
    if evaluateUnsafe formula variables == evaluateUnsafe formula inverse_variables then
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
    ANF.calculateANF formula
        |> List.map List.length
        |> List.maximum
        |> Maybe.andThen (\x -> Just (x > 1))
        |> Maybe.withDefault Basics.False



-- adequacy


isAdequat : List Formula -> Basics.Bool
isAdequat list =
    List.all (\a -> a) [ existsAllInputNotEqInput list Basics.False, existsAllInputNotEqInput list Basics.True, exsistsIsNotMonotone list, existsIsNotAffine list, exsistsIsNotSelfDual list ]
