module ViewHelpers exposing (..)

import Html exposing (Html, br, div, h4, li, p, span, strong, table, td, text, th, tr, ul)
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
                    (String.lines "The grammar of our tool can be displayed in Backus-Naur-Form:\nφ ::= p | ⊥ | ⊤ | (¬φ) | (φ ∧ φ) | (φ ∨ φ) | (φ → φ) | (φ ⊕ φ) | (v ↔ φ)")
                )
            )
        , p [] [ text "The tool automatically transforms known symbols into their corresponding Unicode symbols on input. Direct Unicode input is also possible. Here you can find a few of the possible transformations: " ]
        , table [ class "table" ]
            [ tr [] [ th [] [ text "Unicode" ], th [] [ text "ASCII" ], th [] [ text "LaTeX" ] ]
            , tr []
                [ td [] [ span [] [ text "¬" ] ], td [] [ text "~" ], td [] [ text "\\lnot" ] ]
            , tr []
                [ td [] [ span [] [ text "∧" ] ], td [] [ text "&" ], td [] [ text "\\land" ] ]
            , tr []
                [ td [] [ span [] [ text "∨" ] ], td [] [ text "|" ], td [] [ text "\\lor" ] ]
            , tr []
                [ td [] [ span [] [ text "→" ] ], td [] [ text "->" ], td [] [ text "\\to" ] ]
            , tr []
                [ td [] [ span [] [ text "↔" ] ], td [] [ text "<->" ], td [] [ text "\\leftrightarrow" ] ]
            , tr []
                [ td [] [ span [] [ text "⊕" ] ], td [] [ text "^" ], td [] [ text "\\oplus" ] ]
            , tr []
                [ td [] [ span [] [ text "⊥" ] ], td [] [ text "F" ], td [] [ text "\\bot" ] ]
            , tr []
                [ td [] [ span [] [ text "⊤" ] ], td [] [ text "T" ], td [] [ text "\\top" ] ]
            ]
        , p [] [ text "Note additional the additional rules for parenthesis:" ]
        , ul []
            [ li []
                [ strong [] [ text "Binding Precedence:" ], text " ¬ > ∧, ∨ > ⊕ > → > ↔" ]
            , li [] [ text "↔, →, ⊕, ∧, ∨ are ", strong [] [ text "right-associative" ] ]
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
