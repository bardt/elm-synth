module Shape exposing (..)


type Shape
    = Sine
    | Square
    | Triangle
    | Sawtooth


shapes : List Shape
shapes =
    [ Sine, Square, Triangle, Sawtooth ]


toString : Shape -> String
toString shape =
    case shape of
        Sine ->
            "sine"

        Square ->
            "square"

        Triangle ->
            "triangle"

        Sawtooth ->
            "sawtooth"


fromString : String -> Result String Shape
fromString str =
    case (String.trim <| String.toLower str) of
        "sine" ->
            Ok Sine

        "square" ->
            Ok Square

        "triangle" ->
            Ok Triangle

        "sawtooth" ->
            Ok Sawtooth

        _ ->
            Err <| "Unable to parse Shape from string '" ++ str ++ "'."
