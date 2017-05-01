module FormHelpers exposing (..)

import Html exposing (Attribute)
import Html.Events exposing (on, targetValue)
import Json.Decode as Json


intDecoder : String -> Json.Decoder Int
intDecoder string =
    case (String.toInt string) of
        Ok value ->
            Json.succeed value

        Err message ->
            Json.fail message


onIntInput : (Int -> msg) -> Attribute msg
onIntInput tagger =
    on "input"
        (targetValue
            |> Json.andThen intDecoder
            |> Json.map tagger
        )
