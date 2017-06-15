module App exposing (..)

import Array
import Array.Extra
import FormHelpers
import Html exposing (Attribute, Html, div, input, label, option, select, text)
import Html.Attributes as Attrs
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode as Json
import Keys.State
import Keys.Types
import Keys.View
import Platform.Sub exposing (batch)
import Shape exposing (Shape(..))
import Sound exposing (..)


-- General

import Array
import Platform.Sub exposing (batch)


-- Keys

import Keys.State
import Keys.Types
import Keys.View


-- Types

import Shape exposing (Shape(..))


-- HTML

import Html exposing (Html, Attribute, div, text, label, input, select, option)
import Html.Attributes as Attrs
import Html.Events exposing (on, onClick, onInput, targetValue)
import FormHelpers


-- JSON

import Json.Decode as Json


type alias Model =
    { audioSupported : Bool
    , keys : Keys.Types.Model
    , tracks : Array.Array Track
    }


type alias Track =
    { gain :
        { volume : Volume
        }
    , oscillator : Oscillator
    }


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
    | KeysMsg Keys.Types.Msg
    | NoOp


init : Bool -> ( Model, Cmd Msg )
init audioSupported =
    let
        ( keysModel, keysCmd ) =
            Keys.State.init
    in
        ( { keys = keysModel
          , audioSupported = audioSupported
          , tracks =
                Array.fromList
                    [ Track { volume = 100 }
                        { shape = Square
                        , octaveDelta = 0
                        }
                    , Track { volume = 50 }
                        { shape = Triangle
                        , octaveDelta = 2
                        }
                    ]
          }
        , Cmd.map KeysMsg keysCmd
        )


renderSoundChain : Tracks -> List Keys.Types.Note -> Sound
renderSoundChain tracks notes =
    let
        renderTrackSound index track =
            gain ("gain" ++ toString index)
                [ ( "gain", toString (toFloat track.gain.volume / 100) )
                ]
            <|
                List.map (renderOscillatorSound index track) notes

        renderOscillatorSound : Int -> Track -> Keys.Types.Note -> Sound
        renderOscillatorSound trackNumber track note =
            oscillator ("oscillator" ++ toString trackNumber ++ "_" ++ toString note)
                (flatten
                    [ Keys.State.noteToFrequency note track.oscillator.octaveDelta
                        |> Maybe.map (\f -> ( "frequency", toString f ))
                    ]
                )
                []

        flatten : List (Maybe a) -> List a
        flatten list =
            List.filterMap identity list
    in
        output <|
            List.indexedMap renderTrackSound (Array.toList tracks)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ tracks } as model) =
    case msg of
        KeysMsg msg ->
            let
                ( keysModel, keysCmd ) =
                    Keys.State.update msg model.keys
            in
                { model | keys = keysModel }
                    ! [ Cmd.map KeysMsg keysCmd
                      , Keys.State.getNotes keysModel
                            |> renderSoundChain model.tracks
                            |> play
                      ]

        ChangeOctaveDelta index octaveDelta ->
            let
                newTracks =
                    Array.Extra.update index (changeOctave octaveDelta) tracks
            in
                { model
                    | tracks = newTracks
                }
                    ! [ Keys.State.getNotes model.keys
                            |> renderSoundChain newTracks
                            |> play
                      ]

        ChangeVolume index volume ->
            let
                newTracks =
                    Array.Extra.update index (changeVolume volume) tracks
            in
                { model
                    | tracks = newTracks
                }
                    ! [ Keys.State.getNotes model.keys
                            |> renderSoundChain newTracks
                            |> play
                      ]

        ChangeShape index shape ->
            let
                newTracks =
                    Array.Extra.update index (changeShape shape) tracks
            in
                { model
                    | tracks = newTracks
                }
                    ! [ Keys.State.getNotes model.keys
                            |> renderSoundChain newTracks
                            |> play
                      ]

        NoOp ->
            model ! []


type alias Tracks =
    Array.Array Track


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map KeysMsg (Keys.State.subscriptions model.keys)


view : Model -> Html Msg
view model =
    div []
        (if not model.audioSupported then
            [ Html.text "Audio NOT supported" ]
         else
            (Html.map KeysMsg <|
                Keys.View.view model.keys
            )
                :: List.indexedMap trackView (Array.toList model.tracks)
        )


trackView : Int -> Track -> Html Msg
trackView index track =
    Html.form []
        [ div []
            [ octaveChangeView index track.oscillator.octaveDelta ]
        , div []
            [ volumeChangeView index track.gain.volume ]
        , div []
            [ shapeSelectView index track.oscillator.shape ]
        ]


octaveChangeView : Int -> Int -> Html Msg
octaveChangeView index octaveDelta =
    label []
        [ Html.text "Octave: "
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
        [ Html.text "Volume: "
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
        shapeDecoder : String -> Json.Decoder Shape
        shapeDecoder string =
            case (Shape.fromString string) of
                Ok shape ->
                    Json.succeed shape

                Err message ->
                    Json.fail message

        onChange : (Shape -> Msg) -> Html.Attribute Msg
        onChange tagger =
            on "input"
                (targetValue
                    |> Json.andThen shapeDecoder
                    |> Json.map tagger
                )
    in
        select [ onChange (ChangeShape index) ]
            [ option
                [ Attrs.value (toString Sine)
                , Attrs.selected (shape == Sine)
                ]
                [ Html.text "Sine" ]
            , option [ Attrs.value (toString Triangle), Attrs.selected (shape == Triangle) ] [ Html.text "Triangle" ]
            , option [ Attrs.value (toString Square), Attrs.selected (shape == Square) ] [ Html.text "Square" ]
            , option [ Attrs.value (toString Sawtooth), Attrs.selected (shape == Sawtooth) ] [ Html.text "Sawtooth" ]
            ]
