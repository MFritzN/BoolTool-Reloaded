module NormalForms exposing (..)

{-| This module contains the implementations for calculating NNFs, CNFs and DNFs. The ANF computation happens in the  [`ANF`](ANF) module.
-}

import BoolImpl exposing (..)

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
            And (distrDNF formula1 formula21) (distrDNF formula1 formula22)

        ( a, b ) ->
            And a b

{-| Replaces Implications (Impl) and Exclusive Ors (Xor) by equal statements using And, Or and Neg.
This is needed as a preprocessing step for `calculateNNF`.
-}
replaceImplXor : Formula -> Formula
replaceImplXor formula =
    case formula of
        Neg a ->
            Neg (replaceImplXor a)

        And a b ->
            And (replaceImplXor a) (replaceImplXor b)

        Or a b ->
            Or (replaceImplXor a) (replaceImplXor b)

        Impl a b ->
            Or (Neg (replaceImplXor a)) (replaceImplXor b)

        Xor a b ->
            replaceImplXor (Or (And a (Neg b)) (And (Neg a) b))

        a ->
            a

calculateNNF : Formula -> Formula
calculateNNF formula =
    case replaceImplXor formula of
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