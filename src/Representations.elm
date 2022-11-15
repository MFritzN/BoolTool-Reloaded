module Representations exposing (..)

import BoolImpl exposing (..)
import Set


algebraicNormalform : Formula -> Formula
algebraicNormalform formula =
    formula
        |> algebraicNormalformStep1
        |> simplify
        |> algebraicNomralFormStep2
        |> splitANF
        |> sortANFList
        |> listToANF


algebraicNormalformStep1 : Formula -> Formula
algebraicNormalformStep1 formula =
    case List.head (List.sort (Set.toList (getVariables formula))) of
        Nothing ->
            formula

        Just x1 ->
            let
                g =
                    simplify (replaceVarByValue formula x1 Basics.False)

                h =
                    Xor (simplify (replaceVarByValue formula x1 Basics.False)) (simplify (replaceVarByValue formula x1 Basics.True))
            in
            Xor (algebraicNormalformStep1 g) (And (Var x1) (algebraicNormalformStep1 h))


algebraicNomralFormStep2 : Formula -> Formula
algebraicNomralFormStep2 formula =
    case formula of
        Xor form1 form2 ->
            Xor (algebraicNomralFormStep2 form1) (algebraicNomralFormStep2 form2)

        And (And a b) c ->
            And (And a b) c

        And a (And b c) ->
            And a (And b c)

        And a b ->
            let
                leftFormula =
                    splitANF a

                rightFormula =
                    splitANF b
            in
            List.map (\leftConjunction -> List.map (\rightConjunction -> leftConjunction ++ rightConjunction) rightFormula) leftFormula
                |> List.foldr (++) []
                |> listToANF
                |> simplify

        notReachable ->
            notReachable


listToANF : List (List String) -> Formula
listToANF list =
    case list of
        [] ->
            BoolImpl.False

        x :: [] ->
            listToConjunction x

        x :: xs ->
            Xor (listToConjunction x) (listToANF xs)


listToConjunction : List String -> Formula
listToConjunction list =
    case list of
        [] ->
            BoolImpl.False

        x :: [] ->
            case String.toInt x of
                Nothing ->
                    Var x

                Just int ->
                    if int == 1 then
                        BoolImpl.True

                    else
                        BoolImpl.False

        x :: xs ->
            case String.toInt x of
                Nothing ->
                    And (Var x) (listToConjunction xs)

                Just int ->
                    And
                        (if int == 1 then
                            BoolImpl.True

                         else
                            BoolImpl.False
                        )
                        (listToConjunction xs)


splitANF : Formula -> List (List String)
splitANF formula =
    case formula of
        Xor a b ->
            splitANF a ++ splitANF b

        Var a ->
            [ [ a ] ]

        BoolImpl.True ->
            [ [ String.fromInt 1 ] ]

        BoolImpl.False ->
            [ [ String.fromInt 0 ] ]

        And a b ->
            [ splitANFAndHelp (And a b) ]

        _ ->
            []


splitANFAndHelp : Formula -> List String
splitANFAndHelp formula =
    case formula of
        And a b ->
            splitANFAndHelp a ++ splitANFAndHelp b

        Var a ->
            [ a ]

        BoolImpl.True ->
            [ String.fromInt 1 ]

        BoolImpl.False ->
            [ String.fromInt 0 ]

        _ ->
            []


sortANFList : List (List String) -> List (List String)
sortANFList list =
    List.sortBy (\a -> String.fromInt (List.length a) ++ List.foldr (++) "" a) (List.map List.sort list)


simplify : Formula -> Formula
simplify formula =
    case formula of
        BoolImpl.True ->
            BoolImpl.True

        BoolImpl.False ->
            BoolImpl.False

        BoolImpl.Var string ->
            BoolImpl.Var string

        BoolImpl.And form1 form2 ->
            case ( simplify form1, simplify form2 ) of
                ( BoolImpl.True, x ) ->
                    x

                ( x, BoolImpl.True ) ->
                    x

                ( BoolImpl.False, _ ) ->
                    BoolImpl.False

                ( _, BoolImpl.False ) ->
                    BoolImpl.False

                ( x, y ) ->
                    And x y

        BoolImpl.Xor form1 form2 ->
            case ( simplify form1, simplify form2 ) of
                ( BoolImpl.True, BoolImpl.True ) ->
                    BoolImpl.False

                ( BoolImpl.False, x ) ->
                    x

                ( x, BoolImpl.False ) ->
                    x

                ( x, y ) ->
                    Xor x y

        BoolImpl.Or form1 form2 ->
            case ( simplify form1, simplify form2 ) of
                ( BoolImpl.True, _ ) ->
                    BoolImpl.True

                ( _, BoolImpl.True ) ->
                    BoolImpl.True

                ( BoolImpl.False, x ) ->
                    x

                ( x, BoolImpl.False ) ->
                    x

                ( x, y ) ->
                    Or x y

        BoolImpl.Impl form1 form2 ->
            case ( simplify form1, simplify form2 ) of
                ( BoolImpl.False, _ ) ->
                    BoolImpl.True

                ( BoolImpl.True, x ) ->
                    x

                ( x, BoolImpl.False ) ->
                    x

                ( x, y ) ->
                    Impl x y

        BoolImpl.Neg form1 ->
            case simplify form1 of
                BoolImpl.True ->
                    BoolImpl.False

                BoolImpl.False ->
                    BoolImpl.True

                x ->
                    Neg x


replaceVarByValue : Formula -> String -> Bool -> Formula
replaceVarByValue formula var value =
    case formula of
        BoolImpl.Var string ->
            if var == string then
                if value then
                    BoolImpl.True

                else
                    BoolImpl.False

            else
                Var string

        BoolImpl.And form1 form2 ->
            And (replaceVarByValue form1 var value) (replaceVarByValue form2 var value)

        BoolImpl.Xor form1 form2 ->
            Xor (replaceVarByValue form1 var value) (replaceVarByValue form2 var value)

        BoolImpl.Or form1 form2 ->
            Or (replaceVarByValue form1 var value) (replaceVarByValue form2 var value)

        BoolImpl.Impl form1 form2 ->
            Impl (replaceVarByValue form1 var value) (replaceVarByValue form2 var value)

        BoolImpl.Neg form1 ->
            Neg (replaceVarByValue form1 var value)

        x ->
            x
