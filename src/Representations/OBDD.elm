module Representations.OBDD exposing (..)

import BoolImpl exposing (Formula, evaluateUnsafe)
import Color
import Dict exposing (Dict)
import Graph as G
import Html exposing (Html, div, h4, i, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import List.Extra
import Render as R
import Render.StandardDrawers as RSD
import Render.StandardDrawers.Attributes as RSDA
import Render.StandardDrawers.Types exposing (Shape(..))
import Set



-- MODEL


type alias Model a =
    { formula : Result a Formula
    , variableOrder : List String
    }


initModel : Result a Formula -> Model a
initModel formula =
    { variableOrder = Result.withDefault [] (Result.map (\f -> Set.toList <| BoolImpl.getVariables f) formula)
    , formula = formula
    }



-- UPDATE


type Msg
    = VariableOrderChanged Int MoveTo


type MoveTo
    = Front
    | Back


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
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



-- VIEW


view : Model a -> { title : String, render : Html Msg }
view model =
    { title = "OBDD"
    , render =
        case model.formula of
            Ok formula ->
                div []
                    [ div [ class "is-hidden-mobile" ] [ renderOBDD formula model.variableOrder Basics.False ]
                    , div [ class "is-hidden-tablet" ] [ renderOBDD formula model.variableOrder Basics.True ]
                    ]

            Err _ ->
                div [] []
    }


renderOBDD : Formula -> List String -> Basics.Bool -> Html Msg
renderOBDD formula variableOrder isMobile =
    let
        graph =
            computeOBDD formula variableOrder
    in
    div []
        [ div [ class "field is-grouped is-grouped-multiline" ]
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
                            String.fromInt <| Basics.min 100 <| Basics.max 40 <| 10 * (Set.size <| Set.fromList <| List.map (\node -> node.label) <| G.nodes graph)

                        else
                            String.fromInt <| Basics.min 80 <| Basics.max 20 <| 5 * (Set.size <| Set.fromList <| List.map (\node -> node.label) <| G.nodes graph)
                       )
                    ++ "%; max-height: 95vh; margin-left: auto; margin-right: auto; display: block"
            ]
            graph
        ]



-- OTHER FUNCTIONS


type BDD
    = VariableNode String BDD BDD
    | ValueLeaf Basics.Bool


computeBDD : Formula -> List String -> BDD
computeBDD formula variables =
    computeBDDHelp formula variables Dict.empty


computeBDDHelp : Formula -> List String -> Dict String Basics.Bool -> BDD
computeBDDHelp formula variables values =
    case variables of
        [] ->
            ValueLeaf (evaluateUnsafe formula values)

        variable :: variableTail ->
            VariableNode variable (computeBDDHelp formula variableTail (Dict.insert variable Basics.True values)) (computeBDDHelp formula variableTail (Dict.insert variable Basics.False values))


computeOBDD : Formula -> List String -> G.Graph String Basics.Bool
computeOBDD formula list =
    let
        result =
            computeOBDDHelp (computeBDD formula list) Dict.empty
    in
    G.fromNodesAndEdges result.nodes result.edges


computeOBDDHelp : BDD -> Dict ( String, Int, Int ) Int -> { myId : Int, idManagment : Dict ( String, Int, Int ) Int, nodes : List (G.Node String), edges : List (G.Edge Basics.Bool) }
computeOBDDHelp bdd idManagment =
    case bdd of
        ValueLeaf Basics.True ->
            { myId = 1, idManagment = idManagment, nodes = [ G.Node 1 "1" ], edges = [] }

        ValueLeaf Basics.False ->
            { myId = 0, idManagment = idManagment, nodes = [ G.Node 0 "0" ], edges = [] }

        VariableNode variable hi lo ->
            let
                hiResult =
                    computeOBDDHelp hi idManagment

                loResult =
                    computeOBDDHelp lo hiResult.idManagment

                myId =
                    if hiResult.myId == loResult.myId then
                        loResult.myId

                    else
                        Maybe.withDefault ((+) 1 <| Maybe.withDefault 2 <| List.maximum <| Dict.values loResult.idManagment) (Dict.get ( variable, hiResult.myId, loResult.myId ) loResult.idManagment)
            in
            { myId = myId
            , idManagment =
                if myId <= 1 then
                    loResult.idManagment

                else
                    Dict.insert ( variable, hiResult.myId, loResult.myId ) myId loResult.idManagment
            , nodes = G.Node myId variable :: (hiResult.nodes ++ loResult.nodes)
            , edges = [ G.Edge myId hiResult.myId Basics.True, G.Edge myId loResult.myId Basics.False ] ++ (hiResult.edges ++ loResult.edges)
            }
