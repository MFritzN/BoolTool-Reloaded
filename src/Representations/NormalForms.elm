module Representations.NormalForms exposing (..)

{-| This module contains the implementations for calculating NNFs, CNFs and DNFs. The ANF computation happens in the [`ANF`](ANF) module.
-}

import BoolImpl exposing (..)
import Html exposing (Html, button, div, i, input, span, text)
import Html.Attributes exposing (class, readonly, style, value)
import Html.Events exposing (onClick)
import Representations.ANF as ANF exposing (calculateANF)


type Msg
    = LaTeXClicked NormalForm
    | Copy String


type NormalForm
    = NNF
    | CNF
    | DNF
    | ANF



-- VIEW


renderNormalForm : NormalForm -> Formula -> Maybe NormalForm -> { title : String, render : Html Msg }
renderNormalForm normalForm formula expandedLaTeX =
    let
        caseResult =
            case normalForm of
                NNF ->
                    { title = "Negation Normal Form", normaForm = calculateNNF formula }

                CNF ->
                    { title = "Conjunctive Normal Form", normaForm = calculateCNF formula }

                DNF ->
                    { title = "Disjunctive Normal Form", normaForm = calculateDNF formula }

                ANF ->
                    { title = "Algebaric Normal Form", normaForm = ANF.listToANF <| calculateANF formula }
    in
    { title = caseResult.title
    , render =
        case normalForm of
            ANF ->
                div []
                    ([ text <| ANF.postProcessANF <| toString caseResult.normaForm
                     , button [ onClick <| LaTeXClicked normalForm, class "button is-small", style "float" "right" ] [ text "LaTeX" ]
                     ]
                        ++ (if expandedLaTeX == Just normalForm then
                                [ renderLaTeX <| ANF.postProcessANF <| toString caseResult.normaForm ]

                            else
                                []
                           )
                    )

            _ ->
                div []
                    ([ text <| toString caseResult.normaForm
                     , button [ onClick <| LaTeXClicked normalForm, class "button is-small", style "float" "right" ] [ text "LaTeX" ]
                     ]
                        ++ (if expandedLaTeX == Just normalForm then
                                [ renderLaTeX <| toString caseResult.normaForm ]

                            else
                                []
                           )
                    )
    }


renderLaTeX : String -> Html Msg
renderLaTeX formula =
    let
        laTeX =
            prettyPrintToLaTeX formula
    in
    div [ class "field has-addons" ]
        [ div [ class "control is-expanded" ] [ input [ value laTeX, class "input copy-input is-small", readonly Basics.True ] [] ]
        , div [ class "control" ] [ button [ class "button is-small", onClick <| Copy laTeX ] [ span [ class "icon" ] [ i [ class "far fa-clipboard" ] [] ] ] ]
        ]



-- OTHER FUNCTIONS


calculateCNF : Formula -> Formula
calculateCNF formula =
    case calculateNNF formula of
        And a b ->
            And (calculateCNF a) (calculateCNF b)

        Or a b ->
            distrCNF (calculateCNF a) (calculateCNF b)

        a ->
            a


distrCNF : Formula -> Formula -> Formula
distrCNF formula1 formula2 =
    case ( formula1, formula2 ) of
        ( And formula11 formula12, _ ) ->
            And (distrCNF formula11 formula2) (distrCNF formula12 formula2)

        ( _, And formula21 formula22 ) ->
            And (distrCNF formula1 formula21) (distrCNF formula1 formula22)

        ( a, b ) ->
            Or a b


calculateDNF : Formula -> Formula
calculateDNF formula =
    case calculateNNF formula of
        Or a b ->
            Or (calculateDNF a) (calculateDNF b)

        And a b ->
            distrDNF (calculateDNF a) (calculateDNF b)

        a ->
            a


distrDNF : Formula -> Formula -> Formula
distrDNF formula1 formula2 =
    case ( formula1, formula2 ) of
        ( Or formula11 formula12, _ ) ->
            Or (distrDNF formula11 formula2) (distrDNF formula12 formula2)

        ( _, Or formula21 formula22 ) ->
            Or (distrDNF formula1 formula21) (distrDNF formula1 formula22)

        ( a, b ) ->
            And a b


{-| Replaces Implications (Impl) and Exclusive Ors (Xor) by equal statements using And, Or and Neg.
This is needed as a preprocessing step for `calculateNNF`.
-}
replaceImplXorEqual : Formula -> Formula
replaceImplXorEqual formula =
    case replaceBotTop formula of
        Neg a ->
            Neg (replaceImplXorEqual a)

        And a b ->
            And (replaceImplXorEqual a) (replaceImplXorEqual b)

        Or a b ->
            Or (replaceImplXorEqual a) (replaceImplXorEqual b)

        Impl a b ->
            Or (Neg (replaceImplXorEqual a)) (replaceImplXorEqual b)

        Xor a b ->
            replaceImplXorEqual (Or (And a (Neg b)) (And (Neg a) b))

        Equal a b ->
            replaceImplXorEqual (And (Impl a b) (Impl b a))

        a ->
            a


replaceBotTop : Formula -> Formula
replaceBotTop formula =
    case formula of
        Neg a ->
            case replaceBotTop a of
                BoolImpl.True ->
                    BoolImpl.False

                BoolImpl.False ->
                    BoolImpl.True

                x ->
                    Neg x

        And a b ->
            case ( replaceBotTop a, replaceBotTop b ) of
                ( BoolImpl.True, y ) ->
                    y

                ( x, BoolImpl.True ) ->
                    x

                ( BoolImpl.False, _ ) ->
                    BoolImpl.False

                ( _, BoolImpl.False ) ->
                    BoolImpl.False

                ( x, y ) ->
                    And x y

        Or a b ->
            case ( replaceBotTop a, replaceBotTop b ) of
                ( BoolImpl.True, _ ) ->
                    BoolImpl.True

                ( _, BoolImpl.True ) ->
                    BoolImpl.True

                ( BoolImpl.False, y ) ->
                    y

                ( x, BoolImpl.False ) ->
                    x

                ( x, y ) ->
                    Or x y

        Xor a b ->
            case ( replaceBotTop a, replaceBotTop b ) of
                ( BoolImpl.False, y ) ->
                    y

                ( x, BoolImpl.False ) ->
                    x

                ( BoolImpl.True, y ) ->
                    replaceBotTop <| Neg y

                ( y, BoolImpl.True ) ->
                    replaceBotTop <| Neg y

                ( x, y ) ->
                    Xor x y

        Impl a b ->
            case ( replaceBotTop a, replaceBotTop b ) of
                ( BoolImpl.False, _ ) ->
                    BoolImpl.True

                ( _, BoolImpl.True ) ->
                    BoolImpl.True

                ( x, BoolImpl.False ) ->
                    replaceBotTop <| Neg x

                ( BoolImpl.True, y ) ->
                    y

                ( x, y ) ->
                    Impl x y

        Equal a b ->
            case ( replaceBotTop a, replaceBotTop b ) of
                ( BoolImpl.False, x ) ->
                    replaceBotTop (Neg x)

                ( BoolImpl.True, x ) ->
                    x

                ( x, BoolImpl.False ) ->
                    replaceBotTop <| Neg x

                ( x, BoolImpl.True ) ->
                    x

                ( x, y ) ->
                    Equal x y

        a ->
            a


calculateNNF : Formula -> Formula
calculateNNF formula =
    case replaceImplXorEqual formula of
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
