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


computeGraph : BDD -> G.Graph String Basics.Bool
computeGraph bdd =
    let
        falseNode =
            G.Node 0 "0"

        trueNode =
            G.Node 1 "1"

        result =
            computeGraphHelp bdd 2
    in
    G.fromNodesAndEdges (falseNode :: (trueNode :: result.nodes)) result.edges


computeGraphHelp : BDD -> Int -> { myId : Int, nextFreeId : Int, nodes : List (G.Node String), edges : List (G.Edge Basics.Bool) }
computeGraphHelp bdd nextFreeID =
    case bdd of
        ValueLeaf Basics.True ->
            { myId = 1, nextFreeId = nextFreeID, nodes = [], edges = [] }

        ValueLeaf Basics.False ->
            { myId = 0, nextFreeId = nextFreeID, nodes = [], edges = [] }

        VariableNode variable trueBDD falseBDD ->
            let
                trueBddResult =
                    computeGraphHelp trueBDD nextFreeID

                falseBddResult =
                    computeGraphHelp falseBDD trueBddResult.nextFreeId

                myId =
                    falseBddResult.nextFreeId
            in
            { myId = myId
            , nextFreeId = myId + 1
            , nodes = G.Node myId variable :: (trueBddResult.nodes ++ falseBddResult.nodes)
            , edges = [ G.Edge myId trueBddResult.myId Basics.True, G.Edge myId falseBddResult.myId Basics.False ] ++ (trueBddResult.edges ++ falseBddResult.edges)
            }
