module RepresentationsTest exposing (..)

import BoolImpl exposing (..)
import Dict exposing (Dict)
import Expect
import Parser exposing (variable)
import Representations.ANF exposing (calculateANF, listToANF)
import Representations.NormalForms exposing (calculateCNF, calculateDNF, calculateNNF)
import Representations.TruthTable exposing (calculateTruthTable)
import Test exposing (Test, describe, test)
import TestHelp exposing (testFormulas)


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
