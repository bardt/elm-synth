module Types exposing (..)

import Time
import Keys.Types as KeysTypes
import Json.Encode
import Json.Decode
import Array


type alias Model =
    { audioSupported : Bool
    , keys : KeysTypes.Model
    , tracks : Array.Array Track
    , analyzerData : AnalyzerData
    , analyzerEnabled : Bool
    }


type alias Track =
    { gain :
        { volume : Volume
        }
    , oscillator : Oscillator
    }


type Msg
    = ChangeOctaveDelta Int OctaveDelta
    | ChangeVolume Int Volume
    | ChangeShape Int Shape
    | UpdateAnalyzerData AnalyzerData
    | KeysMsg KeysTypes.Msg
    | NoOp


type alias Oscillator =
    { shape : Shape
    , octaveDelta : OctaveDelta
    }


type Shape
    = Sine
    | Square
    | Triangle
    | Sawtooth


type alias AnalyzerData =
    List Int


type alias OctaveDelta =
    Int


type alias Volume =
    Int


decodeAnalyzerData : Json.Decode.Decoder AnalyzerData
decodeAnalyzerData =
    Json.Decode.list Json.Decode.int


encodeAnalyzerData : AnalyzerData -> Json.Encode.Value
encodeAnalyzerData record =
    Json.Encode.list <| List.map Json.Encode.int <| record
