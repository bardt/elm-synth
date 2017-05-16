port module Sound exposing (..)

import Types exposing (..)
import Json.Decode


port startPlaying : List SoundDescriptor -> Cmd msg


port updateAnalyzer : (Json.Decode.Value -> msg) -> Sub msg
