module App exposing (..)

-- General

import Array
import Platform.Sub exposing (batch)


-- Keys

import Keys.State
import Keys.Types
import Keys.View


-- Data Structures

import Shape exposing (Shape(..))
import Sound exposing (Sound, output, gain, oscillator, play)
import Sound.Properties as Props
import Track exposing (Track, Tracks)


-- HTML

import Html exposing (Html, Attribute, div)


type alias Model =
    { audioSupported : Bool
    , keys : Keys.Types.Model
    , tracks : Tracks
    }


type Msg
    = TrackMsg Track.Msg
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
                        { shape = Sine
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ keys, tracks } as model) =
    case msg of
        KeysMsg msg ->
            let
                ( keysModel, keysCmd ) =
                    Keys.State.update msg keys
            in
                { model | keys = keysModel }
                    ! [ Cmd.map KeysMsg keysCmd
                      , Keys.State.getNotes keysModel
                            |> renderSoundChain tracks
                            |> play
                      ]

        TrackMsg msg ->
            let
                newTracks =
                    Track.update msg tracks
            in
                { model
                    | tracks = newTracks
                }
                    ! [ Keys.State.getNotes keys
                            |> renderSoundChain tracks
                            |> play
                      ]

        NoOp ->
            model ! []


renderSoundChain : Tracks -> List Keys.Types.Note -> Sound
renderSoundChain tracks notes =
    let
        renderTrackSound index track =
            gain ("gain" ++ toString index)
                [ Props.gain (toFloat track.gain.volume / 100)
                ]
            <|
                List.map (renderOscillatorSound index track) notes

        renderOscillatorSound : Int -> Track -> Keys.Types.Note -> Sound
        renderOscillatorSound trackNumber track note =
            oscillator ("oscillator" ++ toString trackNumber ++ "_" ++ toString note)
                (flatten
                    [ Keys.State.noteToFrequency note track.oscillator.octaveDelta
                        |> Maybe.map Props.frequency
                    , Just (Props.type_ track.oscillator.shape)
                    ]
                )
                []

        flatten : List (Maybe a) -> List a
        flatten list =
            List.filterMap identity list
    in
        output <|
            List.indexedMap renderTrackSound (Array.toList tracks)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map KeysMsg (Keys.State.subscriptions model.keys)


view : Model -> Html Msg
view model =
    div []
        (if not model.audioSupported then
            [ Html.text "Audio NOT supported" ]
         else
            [ (Html.map KeysMsg <|
                Keys.View.view model.keys
              )
            , (Html.map TrackMsg <| tracksView model.tracks)
            ]
        )


tracksView : Tracks -> Html Track.Msg
tracksView tracks =
    let
        renderedTracks =
            List.indexedMap Track.view (Array.toList tracks)
    in
        div [] renderedTracks
