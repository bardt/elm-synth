port module Sound exposing (..)


port sendPlaytoJs : Bool -> Cmd msg


port startPlayingHZ : Float -> Cmd msg


port stopPlayingHZ : Float -> Cmd msg
