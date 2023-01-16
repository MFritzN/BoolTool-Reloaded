module ViewHelpers exposing (..)

import Html exposing (Html, div, li, p, strong, text, ul)


boolToSymbol : Basics.Bool -> String
boolToSymbol bool =
    if bool then
        "✓"

    else
        "✕"


maybeToSymbol : Maybe a -> String
maybeToSymbol maybe =
    case maybe of
        Just _ ->
            "✓"

        Nothing ->
            "✕"


syntax : Html msg
syntax =
    div []
        [ p [] [ text "The tool automatically transforms known symbols into their corresponding unicode symbols on input. Direct Unicode input is also possible. Here you can find a few of the possible transformations: " ]
        , ul []
            [ li []
                [ strong [] [ text "Negation:" ], text " ¬ | \\lnot | ~" ]
            , li []
                [ strong [] [ text "Conection:" ], text " ∧ | \\land | &" ]
            , li []
                [ strong [] [ text "Disjunction:" ], text " ∨ | \\lor | |" ]
            , li []
                [ strong [] [ text "Implication:" ], text " → | \\implies | ->" ]
            , li []
                [ strong [] [ text "Exclusive Or:" ], text " ⊕ | \\oplus | ^" ]
            ]
        , p [] [ text "Note additional the additional rules for paranthesis:" ]
        , ul []
            [ li []
                [ strong [] [ text "Binding Precedence:" ], text " ¬ > ∧, ∨ > ⊕ > →" ]
            , li [] [ text "→, ⊕, ∧, ∨ are ", strong [] [ text "right-associative" ] ]
            ]
        ]
