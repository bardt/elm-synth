module Types exposing (..)

import Time
import Keys.Types as KeysTypes


type alias Model =
    { audioSupported : Bool
    , keys : KeysTypes.Model
    , oscillators : List Oscillator
    }


type Msg
    = ChangeOctaveDelta Int OctaveDelta
    | ChangeVolume Int Volume
    | ChangeShape Int Shape
    | KeysMsg KeysTypes.Msg
    | NoOp


type Shape
    = Sine
    | Square
    | Triangle
    | Sawtooth


type alias OctaveDelta =
    Int


type alias Frequency =
    KeysTypes.Frequency


type alias Note =
    KeysTypes.Note


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


makeSoundDescriptor : Frequency -> Oscillator -> SoundDescriptor
makeSoundDescriptor f o =
    SoundDescriptor
        (toString o.shape)
        f
        o.volume
        o.fadeOutPeriod
