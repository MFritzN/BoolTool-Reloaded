module Bool exposing (..)
import String exposing (indices)
import List exposing (unzip)
import Set
import Parser exposing (..)

--
-- A module for representing and handling boolean formulas and sets of functions internally
--

type Formula
    = False
    | True
    | And Formula Formula
    | Or Formula Formula
    | Neg Formula
    | Impl Formula Formula
    | Var String

formula_p : Parser Formula
formula_p =
    oneOf
        [ succeed True
            |. keyword "true"
        , succeed False
            |. keyword "false"
        , succeed Neg
            |. symbol "("
            |. spaces
            |. symbol "!"
            |. spaces
            |= lazy (\_ -> formula_p)
            |. spaces
            |. symbol ")"
        , succeed Impl
            |. symbol "("
            |. spaces
            |= lazy (\_ -> formula_p)
            |. spaces
            |. symbol "->"
            |. spaces
            |= lazy (\_ -> formula_p)
            |. spaces
            |. symbol ")"
        , succeed Or
            |. symbol "("
            |. spaces
            |= lazy (\_ -> formula_p)
            |. spaces
            |. symbol "|"
            |. spaces
            |= lazy (\_ -> formula_p)
            |. spaces
            |. symbol ")"
        , succeed And
            |. symbol "("
            |. spaces
            |= lazy (\_ -> formula_p)
            |. spaces
            |. symbol "&"
            |. spaces
            |= lazy (\_ -> formula_p)
            |. spaces
            |. symbol ")"
        , succeed Var
            |. symbol "("
            |. spaces
            |= lazy (\_ -> typeVar)
            |. spaces
            |. symbol ")"
        ]

typeVar : Parser String
typeVar =
  variable
    { start = Char.isLower
    , inner = \c -> Char.isAlphaNum c
    , reserved = Set.fromList [ "&", "|", "->", "true", "false" ]
    }

toString : Formula -> String
toString formula =
    case formula of
        True ->
            "T"
        False ->
            "F"
        Var v ->
            v
        And l_form r_form ->
            (toString(l_form)) ++ "&" ++ (toString(r_form))
        Or l_form r_form ->
            (toString(l_form)) ++ "|" ++ (toString(r_form))
        Neg r_form ->
            "!" ++ (toString(r_form))
        Impl l_form r_form ->
            (toString(l_form)) ++ "->" ++ (toString(r_form))
        

