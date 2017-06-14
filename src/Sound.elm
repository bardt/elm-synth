port module Sound exposing (..)

import Json.Decode
import Json.Encode exposing (Value, object, list, string)


port startPlaying : Value -> Cmd msg


port updateAnalyzer : (Json.Decode.Value -> msg) -> Sub msg


type Sound
    = SoundNode String (List SoundProperty) (List Sound)
    | Silence


type alias SoundProperty =
    ( String, String )


output : List Sound -> Sound
output =
    SoundNode "output" []


gain : List SoundProperty -> List Sound -> Sound
gain =
    SoundNode "gain"


oscillator : List SoundProperty -> List Sound -> Sound
oscillator =
    SoundNode "oscillator"


volume : Int -> ( String, String )
volume value =
    ( "volume", (toString value) )


type alias SerializedSound =
    List ( String, ( String, String, List SoundProperty ) )


internalSerializeSound : String -> Int -> Sound -> SerializedSound
internalSerializeSound connectedTo index sound =
    case sound of
        SoundNode "output" _ connections ->
            List.indexedMap (internalSerializeSound "output") connections
                |> List.concat

        SoundNode name props connections ->
            let
                key =
                    String.join "_" [ connectedTo, name, toString index ]
            in
                ( key, ( name, connectedTo, props ) )
                    :: (connections
                            |> List.indexedMap (internalSerializeSound key)
                            |> List.concat
                       )

        Silence ->
            []


serializeSound : Sound -> SerializedSound
serializeSound sound =
    internalSerializeSound "" 0 sound


encodeSound : Sound -> Value
encodeSound sound =
    let
        encodeEach ( key, ( name, connectTo, props ) ) =
            ( key
            , list
                [ string name
                , string connectTo
                , object (List.map (\p -> ( Tuple.first p, string <| Tuple.second p )) props)
                ]
            )
    in
        sound
            |> serializeSound
            |> (object << List.map encodeEach)
