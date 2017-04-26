module Types exposing (..)

import Time


type Shape
    = Sine
    | Square
    | Triangle
    | Sawtooth


type alias Octave =
    Int


type alias Frequency =
    Float


type alias Oscillator =
    { shape : Shape
    , octave : Octave
    , volume : Volume
    , fadeOutPeriod : Time.Time
    }


type alias Volume =
    Int


type alias SoundDescriptor =
    { shape : String
    , frequency : Frequency
    , octave : Octave
    , volume : Volume
    , fadeOutPeriod : Time.Time
    }


type alias Sound =
    {}


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


makeSoundDescriptor : Frequency -> Oscillator -> SoundDescriptor
makeSoundDescriptor f o =
    SoundDescriptor
        (serializeShape o.shape)
        f
        o.octave
        o.volume
        o.fadeOutPeriod
