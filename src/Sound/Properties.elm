module Sound.Properties exposing (..)


type alias Property =
    ( String, String )


gain : Float -> Property
gain gain =
    ( "gain", toString gain )


frequency : Float -> Property
frequency f =
    ( "frequency", toString f )


type_ : a -> Property
type_ a =
    ( "type", String.toLower <| toString a )
