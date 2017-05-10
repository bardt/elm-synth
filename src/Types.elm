module Types exposing (..)

import Time


type Shape
    = Sine
    | Square
    | Triangle
    | Sawtooth


type alias OctaveDelta =
    Int


type alias Frequency =
    Float


type alias Oscillator =
    { shape : Shape
    , octave : OctaveDelta
    , volume : Volume
    , fadeOutPeriod : Time.Time
    }


type alias Volume =
    Int


type alias SoundDescriptor =
    { shape : String
    , frequency : Frequency
    , volume : Volume
    , fadeOutPeriod : Time.Time
    }


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
        o.volume
        o.fadeOutPeriod
