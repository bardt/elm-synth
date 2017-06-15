module Track exposing (Track, Tracks, Msg, update, view)

import Array exposing (Array)
import Array.Extra
import FormHelpers
import Html exposing (Html, div, form, input, label, option, text, select)
import Html.Attributes as Attrs
import Html.Events exposing (on, targetValue)
import Shape exposing (Shape(..))
import Json.Decode as Json


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


view : Int -> Track -> Html Msg
view trackNumber track =
    form []
        [ div []
            [ octaveChangeView trackNumber track.oscillator.octaveDelta ]
        , div []
            [ volumeChangeView trackNumber track.gain.volume ]
        , div []
            [ shapeSelectView trackNumber track.oscillator.shape ]
        ]


octaveChangeView : Int -> Int -> Html Msg
octaveChangeView index octaveDelta =
    label []
        [ text "Octave: "
        , input
            [ Attrs.type_ "number"
            , Attrs.value <| toString octaveDelta
            , FormHelpers.onIntInput (ChangeOctaveDelta index)
            ]
            []
        ]


volumeChangeView : Int -> Int -> Html Msg
volumeChangeView index volume =
    label []
        [ text "Volume: "
        , input
            [ Attrs.type_ "range"
            , Attrs.min "0"
            , Attrs.max "100"
            , Attrs.value <| toString volume
            , FormHelpers.onIntInput (ChangeVolume index)
            ]
            []
        , input
            [ Attrs.type_ "number"
            , FormHelpers.onIntInput (ChangeVolume index)
            , Attrs.value <| toString volume
            ]
            []
        ]


shapeSelectView : Int -> Shape -> Html Msg
shapeSelectView index shape =
    let
        onChange : (Shape -> Msg) -> Html.Attribute Msg
        onChange tagger =
            on "input"
                (targetValue
                    |> Json.andThen shapeDecoder
                    |> Json.map tagger
                )

        shapeOption : Shape -> Html Msg
        shapeOption s =
            option [ Attrs.value (toString s), Attrs.selected (shape == s) ]
                [ text (toString s) ]

        shapeDecoder : String -> Json.Decoder Shape
        shapeDecoder string =
            case (Shape.fromString string) of
                Ok shape ->
                    Json.succeed shape

                Err message ->
                    Json.fail message
    in
        select [ onChange (ChangeShape index) ] <|
            List.map shapeOption [ Sine, Triangle, Square, Sawtooth ]
