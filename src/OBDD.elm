module OBDD exposing (..)

import BoolImpl exposing (Formula, evaluateUnsafe)
import Dict exposing (Dict)
import Graph as G


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
        falseNode =
            G.Node 0 "0"

        trueNode =
            G.Node 1 "1"

        bdd =
            computeBDD formula list

        result =
            computeOBDDHelp bdd Dict.empty
    in
    G.fromNodesAndEdges (falseNode :: (trueNode :: result.nodes)) result.edges


computeOBDDHelp : BDD -> Dict ( String, Int, Int ) Int -> { myId : Int, idManagment : Dict ( String, Int, Int ) Int, nodes : List (G.Node String), edges : List (G.Edge Basics.Bool) }
computeOBDDHelp bdd idManagment =
    case bdd of
        ValueLeaf Basics.True ->
            { myId = 1, idManagment = idManagment, nodes = [], edges = [] }

        ValueLeaf Basics.False ->
            { myId = 0, idManagment = idManagment, nodes = [], edges = [] }

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
