module ViewHelpers exposing (..)

import Html exposing (Html, br, div, h4, li, p, span, strong, text, ul)
import Html.Attributes exposing (attribute, class)


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
        [ p []
            (List.intersperse (br [] [])
                (List.map text
                    (String.lines "The grammar of our tool can be displayes in Backus-Naur-Form:\nφ ::= p | ⊥ | ⊤ | (¬φ) | (φ ∧ φ) | (φ ∨ φ) | (φ → φ) | (φ ⊕ φ)")
                )
            )
        , p [] [ text "The tool automatically transforms known symbols into their corresponding unicode symbols on input. Direct Unicode input is also possible. Here you can find a few of the possible transformations: " ]
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
            , li []
                [ strong [] [ text "Bottom:" ], text " False | \\bot" ]
            , li []
                [ strong [] [ text "Tee:" ], text " True | \\top" ]
            ]
        , p [] [ text "Note additional the additional rules for paranthesis:" ]
        , ul []
            [ li []
                [ strong [] [ text "Binding Precedence:" ], text " ¬ > ∧, ∨ > ⊕ > →" ]
            , li [] [ text "→, ⊕, ∧, ∨ are ", strong [] [ text "right-associative" ] ]
            ]
        ]


renderBox : { title : String, render : Html a } -> Html a
renderBox input =
    div [ class "box content" ]
        [ h4 [] [ text input.title ]
        , input.render
        ]


renderTooltip : Html a -> String -> Html a
renderTooltip content tooltip =
    span [ attribute "data-tooltip" tooltip ] [ content ]
