module Types exposing (..)

import Time


type Shape
    = Sine
    | Square
    | Triangle
    | Sawtooth


type alias Octave =
    Float


type alias Frequency =
    Float


type alias Oscillator =
    { shape : Shape
    , baseFrequency : Frequency
    , octave : Octave
    , fadeOutPeriod : Time.Time
    }


type alias SerializedOscillator =
    { shape : String
    , baseFrequency : Frequency
    , octave : Octave
    , fadeOutPeriod : Time.Time
    }


type alias Note =
    String


serializeShape : Shape -> String
serializeShape shape =
    case shape of
        Sine ->
            "sine"

        Square ->
            "square"

        Triangle ->
            "triangle"

        Sawtooth ->
            "sawtooth"


serialize : Oscillator -> SerializedOscillator
serialize o =
    { o
        | shape = serializeShape o.shape
    }
