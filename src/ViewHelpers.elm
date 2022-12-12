module ViewHelpers exposing (..)


boolToSymbol : Basics.Bool -> String
boolToSymbol bool =
    if bool then
        "✓"

    else
        "✕"
