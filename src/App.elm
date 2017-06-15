module App exposing (..)

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
                    [ { gain =
                            { volume = 100
                            }
                      , oscillator =
                            { shape = Square
                            , octaveDelta = 0
                            }
                      }
                    , { gain =
                            { volume = 50
                            }
                      , oscillator =
                            { shape = Triangle
                            , octaveDelta = 2
                            }
                      }
                    ]
          }
        , Cmd.map KeysMsg keysCmd
        )


renderSoundChain : List Track -> List Keys.Types.Note -> Sound
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
            List.indexedMap renderTrackSound tracks


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
                            |> renderSoundChain (Array.toList model.tracks)
                            |> play
                      ]

        ChangeOctaveDelta index octaveDelta ->
            let
                changeOctave track =
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

                newTracks =
                    updateTrackAtIndex index changeOctave tracks
            in
                { model
                    | tracks = newTracks
                }
                    ! [ Keys.State.getNotes model.keys
                            |> renderSoundChain (Array.toList newTracks)
                            |> play
                      ]

        ChangeVolume index volume ->
            let
                changeVolume track =
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

                newTracks =
                    updateTrackAtIndex index changeVolume tracks
            in
                { model
                    | tracks = newTracks
                }
                    ! [ Keys.State.getNotes model.keys
                            |> renderSoundChain (Array.toList newTracks)
                            |> play
                      ]

        ChangeShape index shape ->
            let
                changeShape track =
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

                newTracks =
                    updateTrackAtIndex index changeShape tracks
            in
                { model
                    | tracks = newTracks
                }
                    ! [ Keys.State.getNotes model.keys
                            |> renderSoundChain (Array.toList newTracks)
                            |> play
                      ]

        NoOp ->
            model ! []


type alias Tracks =
    Array.Array Track


updateTrackAtIndex : Int -> (Track -> Track) -> Tracks -> Tracks
updateTrackAtIndex index update tracks =
    Array.indexedMap
        (\i o ->
            if (i == index) then
                update o
            else
                o
        )
        tracks


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
