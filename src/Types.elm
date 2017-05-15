module Types exposing (..)

import Time
import Keys.Types as KeysTypes
import Json.Encode
import Json.Decode


type alias Model =
    { audioSupported : Bool
    , keys : KeysTypes.Model
    , oscillators : List Oscillator
    , analyzerData : AnalyzerData
    }


type Msg
    = ChangeOctaveDelta Int OctaveDelta
    | ChangeVolume Int Volume
    | ChangeShape Int Shape
    | UpdateAnalyzerData AnalyzerData
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


type alias AnalyzerData =
    List Int


decodeAnalyzerData : Json.Decode.Decoder AnalyzerData
decodeAnalyzerData =
    Json.Decode.list Json.Decode.int


encodeAnalyzerData : AnalyzerData -> Json.Encode.Value
encodeAnalyzerData record =
    Json.Encode.list <| List.map Json.Encode.int <| record
