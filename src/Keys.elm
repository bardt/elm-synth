module Keys exposing (..)

import Dict
import Keyboard exposing (KeyCode)
import Types exposing (..)


keyToNoteMapping : Dict.Dict KeyCode Note
keyToNoteMapping =
    Dict.fromList
        [ ( 65, "C" )
        , ( 87, "C#" )
        , ( 83, "D" )
        , ( 69, "Eb" )
        , ( 68, "E" )
        , ( 70, "F" )
        , ( 84, "F#" )
        , ( 71, "G" )
        , ( 89, "G#" )
        , ( 72, "A" )
        , ( 85, "Bb" )
        , ( 74, "B" )
        , ( 75, "C+" )
        , ( 79, "C#+" )
        , ( 76, "D+" )
        , ( 80, "D#+" )
        , ( 186, "E+" )
        ]


noteToFrequencyMapping : Dict.Dict Note Frequency
noteToFrequencyMapping =
    Dict.fromList
        [ ( "C", 16.35 )
        , ( "C#", 17.32 )
        , ( "D", 18.35 )
        , ( "Eb", 19.45 )
        , ( "E", 20.6 )
        , ( "F", 21.83 )
        , ( "F#", 23.12 )
        , ( "G", 24.5 )
        , ( "G#", 25.96 )
        , ( "A", 27.5 )
        , ( "Bb", 29.14 )
        , ( "B", 30.87 )
        , ( "C+", 16.35 * 2 )
        , ( "C#+", 17.32 * 2 )
        , ( "D+", 18.35 * 2 )
        , ( "D#+", 19.45 * 2 )
        , ( "E+", 20.6 * 2 )
        ]


keyToNote : KeyCode -> Maybe Note
keyToNote keyCode =
    Dict.get keyCode keyToNoteMapping


noteToFrequency : Note -> Maybe Frequency
noteToFrequency note =
    Dict.get note noteToFrequencyMapping


keyToFrequency : KeyCode -> Maybe Frequency
keyToFrequency key =
    key
        |> keyToNote
        |> Maybe.andThen noteToFrequency
