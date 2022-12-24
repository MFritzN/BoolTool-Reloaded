module RepresentationsTest exposing (..)

import ANF exposing (calculateANF, listToANF)
import BoolImpl exposing (..)
import Dict exposing (Dict)
import Expect
import Graph as G
import IntDict
import List.Extra
import NormalForms exposing (calculateCNF, calculateDNF, calculateNNF)
import OBDD exposing (computeOBDD, getContexts)
import Parser exposing (variable)
import Properties exposing (calculateTruthTable)
import Set
import Test exposing (Test, describe, test)


anfTestSuite : Test
anfTestSuite =
    let
        a =
            Var "a"

        b =
            Var "b"

        c =
            Var "c"

        d =
            Var "d"

        x =
            Var "x"

        y =
            Var "y"
    in
    describe "ANF Test"
        [ anfTestHelp (Or x y) (Xor x (Xor y (And x y)))
        , anfTestHelp (Or (And (Neg d) (Or (And (Neg c) a) (And c b))) (And d (Or (And (Neg a) b) (And a c)))) (Xor a (Xor (And a c) (Xor (And a d) (Xor (And b c) (Xor (And b d) (Xor (And a (And b d)) (And b (And c d))))))))
        ]


anfTestHelp : Formula -> Formula -> Test
anfTestHelp input expected =
    test (toString input) <|
        \_ ->
            calculateANF input
                |> listToANF
                |> equals expected
                |> Expect.equal Basics.True
                |> Expect.onFail ("Expected " ++ toString expected ++ " but got " ++ toString (listToANF (calculateANF input)))


testFormulas : List Formula
testFormulas =
    let
        a =
            Var "a"

        b =
            Var "b"

        c =
            Var "c"

        d =
            Var "d"

        x =
            Var "x"

        y =
            Var "y"
    in
    [ Or x y
    , Or (And (Neg d) (Or (And (Neg c) a) (And c b))) (And d (Or (And (Neg a) b) (And a c)))
    , And a b
    , Impl a b
    , Xor a b
    , Neg (And a b)
    , Neg (Impl a b)
    , And (Or a b) (And (Or c d) (Or x y))
    ]


nnfTestSuite : Test
nnfTestSuite =
    describe "calculateNNF"
        (List.map
            (\formula -> isNNF formula (calculateNNF formula))
            testFormulas
        )


cnfTestSuite : Test
cnfTestSuite =
    describe "calculateCNF"
        (List.map
            isCNF
            testFormulas
        )


dnfTestSuite : Test
dnfTestSuite =
    describe "calculateDNF"
        (List.map
            isDNF
            testFormulas
        )


isNNF : Formula -> Formula -> Test
isNNF originalFormula nnf =
    describe (toString originalFormula)
        [ test "nnf  is equivalent to original"
            (\_ ->
                isEquivalent originalFormula nnf
            )
        , test "negations are only with atoms"
            (\_ ->
                isNNFForm nnf
            )
        ]


isNNFForm : Formula -> Expect.Expectation
isNNFForm nnf =
    case nnf of
        And a b ->
            Expect.all [ \_ -> isNNFForm a, \_ -> isNNFForm b ] a

        Or a b ->
            Expect.all [ \_ -> isNNFForm a, \_ -> isNNFForm b ] a

        Xor a b ->
            Expect.all [ \_ -> isNNFForm a, \_ -> isNNFForm b ] a

        Impl a b ->
            Expect.all [ \_ -> isNNFForm a, \_ -> isNNFForm b ] a

        Neg (Var _) ->
            Expect.pass

        Neg BoolImpl.True ->
            Expect.pass

        Neg BoolImpl.False ->
            Expect.pass

        Neg _ ->
            Expect.fail "I found a Negation that was not next to an Atom."

        _ ->
            Expect.pass


isEquivalent : Formula -> Formula -> Expect.Expectation
isEquivalent formulaA formulaB =
    Expect.equal (calculateTruthTable formulaA) (calculateTruthTable formulaB)


isCNF : Formula -> Test
isCNF originalFormula =
    let
        cnf =
            calculateCNF originalFormula
    in
    describe (toString originalFormula)
        [ test "nnf  is equivalent to original"
            (\_ ->
                isEquivalent originalFormula cnf
            )
        , test "calculated cnf is of correct form"
            (\_ ->
                isCNFForm cnf
            )
        ]


isCNFForm : Formula -> Expect.Expectation
isCNFForm formula =
    case formula of
        And a b ->
            Expect.all [ \_ -> isCNFForm a, \_ -> isCNFForm b ] a

        Or a b ->
            Expect.all [ \_ -> isDisjunction a, \_ -> isDisjunction b ] a

        Neg a ->
            isNNFForm (Neg a)

        Xor _ _ ->
            Expect.fail "I found a xor in the calculated cnf"

        Impl _ _ ->
            Expect.fail "I found an implication in the calculated cnf"

        _ ->
            Expect.pass


{-| Passes if the formula only consists of Disjunctions, Literals and Negations of literals. Is used by [`isCNFForm`](#isCNFForm)

        isDisjunction And a (Neg b) == Pass
        isDisjunction Neg (And a b) == Fail

-}
isDisjunction : Formula -> Expect.Expectation
isDisjunction formula =
    case formula of
        Or a b ->
            Expect.all [ \_ -> isDisjunction a, \_ -> isDisjunction b ] a

        Xor _ _ ->
            Expect.fail "I found a xor in what was supposed to be a conjunction"

        And _ _ ->
            Expect.fail "I found a conjunction in what was supposed to be a conjunction"

        Impl _ _ ->
            Expect.fail "I found an implication in what was supposed to be a conjunction"

        Neg a ->
            isNNFForm a

        _ ->
            Expect.pass


isDNF : Formula -> Test
isDNF originalFormula =
    let
        dnf =
            calculateDNF originalFormula
    in
    describe (toString originalFormula)
        [ test "dnf  is equivalent to original"
            (\_ ->
                isEquivalent originalFormula dnf
            )
        , test "calculated dnf is of correct form"
            (\_ ->
                isDNFForm dnf
            )
        ]


isDNFForm : Formula -> Expect.Expectation
isDNFForm formula =
    case formula of
        And a b ->
            Expect.all [ \_ -> isConjunction a, \_ -> isConjunction b ] a

        Or a b ->
            Expect.all [ \_ -> isDNFForm a, \_ -> isDNFForm b ] a

        Neg a ->
            isNNFForm (Neg a)

        Xor _ _ ->
            Expect.fail "I found a xor in the calculated cnf"

        Impl _ _ ->
            Expect.fail "I found an implication in the calculated cnf"

        _ ->
            Expect.pass


{-| Passes if the formula only consists of Conjunctions, Literals and Negations of literals. Is used by [`isDNFForm`](#isDNFForm)

        isDisjunction Or a (Neg b) == Pass
        isDisjunction Neg (Or a b) == Fail

-}
isConjunction : Formula -> Expect.Expectation
isConjunction formula =
    case formula of
        And a b ->
            Expect.all [ \_ -> isConjunction a, \_ -> isConjunction b ] a

        Xor _ _ ->
            Expect.fail "I found a xor in what was supposed to be a conjunction"

        Or _ _ ->
            Expect.fail "I found a disjunction in what was supposed to be a conjunction"

        Impl _ _ ->
            Expect.fail "I found an implication in what was supposed to be a conjunction"

        Neg a ->
            isNNFForm a

        _ ->
            Expect.pass


allVariableCombinations : List String -> List (List String)
allVariableCombinations variables =
    case allVariableCombinationsHelp [ variables ] [] variables variables of
        Err a ->
            a

        Ok a ->
            a


allVariableCombinationsHelp : List (List String) -> List String -> List String -> List String -> Result (List (List String)) (List (List String))
allVariableCombinationsHelp variablesList currentTry remainingVars originalVars =
    case remainingVars of
        [] ->
            Err variablesList

        var :: vars ->
            if List.any ((==) var) currentTry then
                allVariableCombinationsHelp variablesList currentTry vars originalVars

            else if List.length currentTry == List.length originalVars - 1 then
                if List.any (\list -> list == (var :: currentTry)) variablesList then
                    allVariableCombinationsHelp variablesList currentTry vars originalVars

                else
                    Ok ((var :: currentTry) :: variablesList)

            else
                case allVariableCombinationsHelp variablesList (var :: currentTry) originalVars originalVars of
                    Ok result ->
                        Ok result

                    Err result ->
                        allVariableCombinationsHelp result (var :: currentTry) vars originalVars


bbdSuite : Test
bbdSuite =
    describe "BDD Equivalence"
        (List.map
            (\formula ->
                describe (toString formula)
                    (let
                        varOrder =
                            allVariableCombinations (Set.toList (getVariables formula))
                     in
                     List.map (\vars -> test ("Var Order: " ++ Debug.toString vars) (\_ -> isEqualBdd formula vars)) varOrder
                    )
            )
            testFormulas
        )


isEqualBdd : Formula -> List String -> Expect.Expectation
isEqualBdd formula variables =
    let
        bdd =
            computeOBDD formula variables

        values =
            Dict.fromList (List.map (\variable -> ( variable, Basics.False )) variables)
    in
    isEqualBddHelp formula values bdd


isEqualBddHelp : Formula -> Dict String Basics.Bool -> G.Graph String Basics.Bool -> Expect.Expectation
isEqualBddHelp formula variables graph =
    case evaluateGraph graph variables of
        Ok result ->
            if evaluateUnsafe formula variables /= result then
                Expect.equal (evaluateUnsafe formula variables) result

            else
                case iterateVariables variables of
                    Just newValues ->
                        isEqualBddHelp formula newValues graph

                    Nothing ->
                        Expect.pass

        Err err ->
            Expect.fail err


evaluateGraph : G.Graph String Basics.Bool -> Dict String Basics.Bool -> Result String Basics.Bool
evaluateGraph graph variables =
    List.Extra.find (\context -> IntDict.isEmpty context.incoming) (getContexts graph)
        |> Result.fromMaybe "Could not find root node."
        |> Result.andThen (\context -> evaluateGraphHelp context graph variables)


evaluateGraphHelp : G.NodeContext String Basics.Bool -> G.Graph String Basics.Bool -> Dict String Basics.Bool -> Result String Basics.Bool
evaluateGraphHelp context graph variables =
    case ( context.node.id, Dict.get context.node.label variables ) of
        ( 0, _ ) ->
            Ok Basics.False

        ( 1, _ ) ->
            Ok Basics.True

        ( id, Just value ) ->
            IntDict.toList context.outgoing
                |> List.Extra.find (\( k, v ) -> v == value)
                |> Result.fromMaybe ("Was not able to find the correct edge from node {id: " ++ String.fromInt context.node.id ++ ", label: " ++ context.node.label ++ ".")
                |> Result.andThen (\( k, _ ) -> Result.fromMaybe "I couldn't find the end node of an edge" (G.get k graph))
                |> Result.andThen (\c -> evaluateGraphHelp c graph variables)

        ( id, Nothing ) ->
            Err ("Could not find the Variable Value for label " ++ context.node.label ++ ".")
