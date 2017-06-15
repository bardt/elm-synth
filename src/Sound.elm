port module Sound
    exposing
        ( Sound
        , output
        , gain
        , oscillator
        , play
        )

import Json.Encode exposing (Value, object, list, string)


port startPlaying : Value -> Cmd msg


type Sound
    = SoundNode String String (List SoundProperty) (List Sound)
    | Silence


type alias SoundProperty =
    ( String, String )


output : List Sound -> Sound
output =
    SoundNode "output" "output" []


gain : String -> List SoundProperty -> List Sound -> Sound
gain key =
    SoundNode key "gain"


oscillator : String -> List SoundProperty -> List Sound -> Sound
oscillator key =
    SoundNode key "oscillator"


type alias SerializedSound =
    ( String, ( String, String, List SoundProperty ) )


internalSerializeSound : String -> Sound -> List SerializedSound
internalSerializeSound connectedTo sound =
    case sound of
        SoundNode "output" "output" _ connections ->
            List.map (internalSerializeSound "output") connections
                |> List.concat

        SoundNode key name props connections ->
            ( key, ( name, connectedTo, props ) )
                :: (connections
                        |> List.map (internalSerializeSound key)
                        |> List.concat
                   )

        Silence ->
            []


serializeSound : Sound -> List SerializedSound
serializeSound sound =
    internalSerializeSound "" sound


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


play : Sound -> Cmd msg
play sound =
    sound
        |> encodeSound
        |> startPlaying
