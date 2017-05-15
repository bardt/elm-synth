port module Sound exposing (..)

import Types exposing (..)


port startPlaying : List SoundDescriptor -> Cmd msg


port updateAnalyzer : (String -> msg) -> Sub msg
