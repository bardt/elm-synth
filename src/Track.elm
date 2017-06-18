module Track exposing (Track, Tracks, Msg, update, view)

import Array exposing (Array)
import Array.Extra
import FormHelpers
import Element exposing (Element, Attribute, form, row, labelBelow, select, option, text, html)
import Element.Events exposing (on, onInput)
import Html
import Html.Attributes as Attrs
import Html.Events exposing (targetValue)
import Shape exposing (Shape(..))
import Json.Decode as Json
import Styles exposing (Styles)


type alias Track =
    { gain :
        { volume : Volume
        }
    , oscillator : Oscillator
    }


type alias Tracks =
    Array Track


type alias Oscillator =
    { shape : Shape
    , octaveDelta : OctaveDelta
    }


type alias OctaveDelta =
    Int


type alias Volume =
    Int


type Msg
    = ChangeOctaveDelta Int OctaveDelta
    | ChangeVolume Int Volume
    | ChangeShape Int Shape


update : Msg -> Tracks -> Tracks
update msg =
    case msg of
        ChangeOctaveDelta trackNumber octaveDelta ->
            updateTrack trackNumber <| changeOctave octaveDelta

        ChangeVolume trackNumber volume ->
            updateTrack trackNumber <| changeVolume volume

        ChangeShape trackNumber shape ->
            updateTrack trackNumber <| changeShape shape


updateTrack : Int -> (Track -> Track) -> Tracks -> Tracks
updateTrack =
    Array.Extra.update


changeOctave : Int -> Track -> Track
changeOctave octaveDelta track =
    let
        oscillator =
            track.oscillator
    in
        { track
            | oscillator =
                { oscillator
                    | octaveDelta = octaveDelta
                }
        }


changeVolume : Int -> Track -> Track
changeVolume volume track =
    let
        gain =
            track.gain
    in
        { track
            | gain =
                { gain
                    | volume = volume
                }
        }


changeShape : Shape -> Track -> Track
changeShape shape track =
    let
        oscillator =
            track.oscillator
    in
        { track
            | oscillator =
                { oscillator
                    | shape = shape
                }
        }


view : Int -> Track -> Element Styles v Msg
view trackNumber track =
    Element.form <|
        row Styles.none
            []
            [ octaveChangeView trackNumber track.oscillator.octaveDelta
            , volumeChangeView trackNumber track.gain.volume
            , shapeSelectView trackNumber track.oscillator.shape
            ]


octaveChangeView : Int -> Int -> Element Styles v Msg
octaveChangeView index octaveDelta =
    labelBelow Styles.none [] (text "Octave") <|
        html <|
            Html.input
                [ Attrs.type_ "number"
                , Attrs.value <| toString octaveDelta
                , FormHelpers.onIntInput (ChangeOctaveDelta index)
                ]
                []


volumeChangeView : Int -> Int -> Element Styles v Msg
volumeChangeView index volume =
    labelBelow Styles.none
        []
        (text "Volume")
    <|
        row Styles.none
            []
            [ html <|
                Html.input
                    [ Attrs.type_ "range"
                    , Attrs.min "0"
                    , Attrs.max "100"
                    , Attrs.value <| toString volume
                    , FormHelpers.onIntInput (ChangeVolume index)
                    ]
                    []
            , html <|
                Html.input
                    [ Attrs.type_ "number"
                    , FormHelpers.onIntInput (ChangeVolume index)
                    , Attrs.value <| toString volume
                    ]
                    []
            ]


shapeSelectView : Int -> Shape -> Element Styles v Msg
shapeSelectView index shape =
    let
        onChange : (Shape -> Msg) -> Attribute v Msg
        onChange tagger =
            on "input"
                (targetValue |> Json.andThen shapeDecoder |> Json.map tagger)

        shapeOption : Shape -> Element.Option Styles v Msg
        shapeOption s =
            option (toString s)
                (shape == s)
                (text <| toString s)

        shapeDecoder : String -> Json.Decoder Shape
        shapeDecoder string =
            case (Shape.fromString string) of
                Ok shape ->
                    Json.succeed shape

                Err message ->
                    Json.fail message
    in
        select "Waveshape" Styles.none [ onChange (ChangeShape index) ] <|
            List.map shapeOption Shape.shapes
