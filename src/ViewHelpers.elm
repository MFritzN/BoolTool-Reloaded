module ViewHelpers exposing (..)


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
