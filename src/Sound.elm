port module Sound
    exposing
        ( Sound
        , output
        , gain
        , oscillator
        , play
        )

import Json.Encode exposing (Value, object, list, string)
import Sound.Properties exposing (Property)


port startPlaying : Value -> Cmd msg


type Sound
    = SoundNode String String (List Property) (List Sound)
    | Silence


output : List Sound -> Sound
output =
    SoundNode "output" "output" []


gain : String -> List Property -> List Sound -> Sound
gain key =
    SoundNode key "gain"


oscillator : String -> List Property -> List Sound -> Sound
oscillator key =
    SoundNode key "oscillator"


type alias SerializedSound =
    ( String, ( String, String, List Property ) )


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
