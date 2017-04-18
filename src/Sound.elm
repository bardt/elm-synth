port module Sound exposing (..)

import Types exposing (..)


port sendPlaytoJs : Bool -> Cmd msg


port startPlayingHZ : Float -> Cmd msg


port stopPlayingHZ : Float -> Cmd msg


port startPlaying : List SerializedOscillator -> Cmd msg


port stopPlaying : List SerializedOscillator -> Cmd msg
