module OBDD exposing (..)

import BoolImpl exposing (Formula, evaluateUnsafe)
import Dict exposing (Dict)
import Graph as G
import IntDict
import List.Extra


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


computeOBDD : Formula -> List String -> G.Graph String Basics.Bool
computeOBDD formula variables =
    computeBDD formula variables
        |> computeGraph
        |> computeObddHelp


computeObddHelp : G.Graph String Basics.Bool -> G.Graph String Basics.Bool
computeObddHelp graph =
    removeRedundantTests graph
        |> (\( hasChanged, g ) -> ( hasChanged || Tuple.first (removeDuplicates g), Tuple.second (removeDuplicates g) ))
        |> (\( hasChanged, g ) ->
                if hasChanged then
                    computeObddHelp g

                else
                    g
           )



--


{-| Remove non-terminals with both edges pointing to the same node.
Uses `removeRedundantTestsFindNodeId` to find such nodes. If such nodes are found their parent's edges are adjusted before they are removed, then `removeRedundantTests` calls itself until there are no more nodes found.
-}
removeRedundantTests : G.Graph String Basics.Bool -> ( Basics.Bool, G.Graph String Basics.Bool )
removeRedundantTests graph =
    case removeRedundantTestsFindNodeId graph of
        Nothing ->
            ( Basics.False, graph )

        Just ( contextToRemove, childId ) ->
            let
                parentNodes =
                    IntDict.keys contextToRemove.incoming
            in
            ( Basics.True
            , graph
                |> G.mapContexts
                    (\iteratedContext ->
                        if List.any (\id -> iteratedContext.node.id == id) parentNodes then
                            { iteratedContext | outgoing = IntDict.insert childId (Maybe.withDefault Basics.True (IntDict.get contextToRemove.node.id iteratedContext.outgoing)) iteratedContext.outgoing }

                        else
                            iteratedContext
                    )
                |> G.remove contextToRemove.node.id
                |> removeRedundantTests
                |> Tuple.second
            )


{-| Finds a Node which's edge only point to one node and returns its context. Additionally the found nodes childId is returned.
-}
removeRedundantTestsFindNodeId : G.Graph String Basics.Bool -> Maybe ( G.NodeContext String Basics.Bool, G.NodeId )
removeRedundantTestsFindNodeId graph =
    getContexts graph
        -- elm/graph only allows one edge between the same node per direction, that's why we can check for == 1 here.
        |> List.Extra.find (\context -> List.length (IntDict.values context.outgoing) == 1)
        |> Maybe.andThen (\context -> Maybe.map (\childDictEntry -> ( context, Tuple.first childDictEntry )) (IntDict.findMax context.outgoing))


getContexts : G.Graph String Basics.Bool -> List (G.NodeContext String Basics.Bool)
getContexts graph =
    G.nodes graph
        |> List.map (\a -> G.get a.id graph)
        |> List.filterMap identity


removeDuplicates : G.Graph String Basics.Bool -> ( Basics.Bool, G.Graph String Basics.Bool )
removeDuplicates graph =
    case findDuplicates (getContexts graph) of
        [] ->
            ( Basics.False, graph )

        contextToKeep :: contextsToRemove ->
            ( Basics.True
            , G.mapContexts
                (\context ->
                    { context
                        | outgoing =
                            context.outgoing
                                |> IntDict.toList
                                |> List.map
                                    (\( k, v ) ->
                                        if List.any (\contextToRemove -> contextToRemove.node.id == k) contextsToRemove then
                                            ( contextToKeep.node.id, v )

                                        else
                                            ( k, v )
                                    )
                                |> IntDict.fromList
                    }
                )
                graph
                |> (\g -> List.foldl G.remove g (List.map (\context -> context.node.id) contextsToRemove))
                |> removeDuplicates
                |> Tuple.second
            )


findDuplicates : List (G.NodeContext String Basics.Bool) -> List (G.NodeContext String Basics.Bool)
findDuplicates contexts =
    case contexts of
        [] ->
            []

        contextToCompare :: contextTail ->
            case List.filter (\context -> IntDict.toList context.outgoing == IntDict.toList contextToCompare.outgoing && context.node.id > 2) contextTail of
                [] ->
                    findDuplicates contextTail

                result ->
                    contextToCompare :: result
