module Representations exposing (..)

import BoolImpl exposing (..)
import Set



-- ANF - Represented as a List of Lists of Strings whereas Strings represent Variables, a inner list conjunctions and the outer list Disjunctions


calculateANF : Formula -> List (List String)
calculateANF formula =
    case formula of
        Var x ->
            [ [ x ] ]

        BoolImpl.True ->
            [ [] ]

        BoolImpl.False ->
            []

        Xor x y ->
            polishANF (calculateANF x ++ calculateANF y)

        Neg x ->
            polishANF ([ [] ] ++ calculateANF x)

        Or x y ->
            let
                xANF =
                    calculateANF x

                yANF =
                    calculateANF y
            in
            polishANF (xANF ++ yANF ++ calculateANF (And x y))

        And x y ->
            let
                xANF =
                    calculateANF x

                yANF =
                    calculateANF y
            in
            List.map (\xConjunction -> List.map (\yConjunction -> yConjunction ++ xConjunction) xANF) yANF
                |> List.foldr (++) []
                |> polishANF

        Impl x y ->
            calculateANF (And (Neg x) y)


polishANF : List (List String) -> List (List String)
polishANF list =
    List.map (\conjunction -> Set.toList (Set.fromList conjunction)) list
        |> sortANFList
        |> removeDuplicatesFromANF


removeDuplicatesFromANF : List (List String) -> List (List String)
removeDuplicatesFromANF anf =
    case anf of
        x :: y :: xs ->
            if x == y then
                removeDuplicatesFromANF xs

            else
                x :: removeDuplicatesFromANF (y :: xs)

        _ ->
            anf


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
