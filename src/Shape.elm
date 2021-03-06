module Shape exposing (..)


type Shape
    = Sine
    | Square
    | Triangle
    | Sawtooth


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
