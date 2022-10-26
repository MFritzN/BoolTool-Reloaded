module Bool exposing (..)
import String exposing (indices)
import List exposing (unzip)
import Parser exposing (..)
import Html.Events exposing (on)
import Set

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


-- The following code was adapted from
-- https://github.com/elm/parser/blob/1.1.0/examples/Math.elm (2022-10-25)
typeVar : Parser String
typeVar = 
  variable
    { start = Char.isLower
    , inner = \c -> Char.isAlphaNum c
    , reserved = Set.fromList [ "true", "false"]
    }


term : Parser Formula
term =
  oneOf
    [ succeed Var
        |= typeVar
    , succeed Neg
        |. symbol "!"
        |= lazy (\_ -> term)
    , succeed identity
        |. symbol "("
        |. spaces
        |= oneOf
        [ succeed Neg
            |. symbol "!"
            |= lazy (\_ -> formula_p)
            |. spaces
            |. symbol ")"
        , succeed identity
            |= lazy (\_ -> formula_p)
            |. spaces
            |. symbol ")"
        ]
    ]


formula_p : Parser Formula
formula_p =
  term
    |> andThen (expressionHelp [])


{-| If you want to parse operators with different precedence (like `+` and `*`)
a good strategy is to go through and create a list of all the operators. From
there, you can write separate code to sort out the grouping.
-}
expressionHelp : List (Formula, Operator) -> Formula -> Parser Formula
expressionHelp revOps expr =
  oneOf
    [ succeed Tuple.pair
        |. spaces
        |= operator
        |. spaces
        |= term
        |> andThen (\(op, newExpr) -> expressionHelp ((expr,op) :: revOps) newExpr)
    , lazy (\_ -> succeed (finalize revOps expr))
    ]


type Operator = AndOp | OrOp | ImplOp


operator : Parser Operator
operator =
  oneOf
    [ map (\_ -> AndOp) (symbol "&")
    , map (\_ -> OrOp) (symbol "|")
    , map (\_ -> ImplOp) (symbol "->")
    ]


{-| We only have `+` and `*` in this parser. If we see a `MulOp` we can
immediately group those two expressions. If we see an `AddOp` we wait to group
until all the multiplies have been taken care of.
This code is kind of tricky, but it is a baseline for what you would need if
you wanted to add `/`, `-`, `==`, `&&`, etc. which bring in more complex
associativity and precedence rules.
-}
finalize : List (Formula, Operator) -> Formula -> Formula
finalize revOps finalExpr =
  case revOps of
    [] ->
      finalExpr

    (expr, OrOp) :: otherRevOps ->
      finalize otherRevOps (Or expr finalExpr)

    (expr, AndOp) :: otherRevOps ->
      finalize otherRevOps (And expr finalExpr)

    (expr, ImplOp) :: otherRevOps ->
      finalize otherRevOps (Impl expr finalExpr)
--TODO: Respect precendence!


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
            "(" ++(toString(l_form)) ++ "&" ++ (toString(r_form)) ++ ")"
        Or l_form r_form ->
            "(" ++(toString(l_form)) ++ "|" ++ (toString(r_form)) ++ ")"
        Neg r_form ->
            "(" ++ "!" ++ (toString(r_form)) ++ ")"
        Impl l_form r_form ->
            "(" ++ (toString(l_form)) ++ "->" ++ (toString(r_form)) ++ ")"
        

