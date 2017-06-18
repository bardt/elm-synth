module App exposing (..)

-- General

import Array
import Element exposing (column, el, row, text, viewport)
import Element.Attributes exposing (alignLeft, center, padding, spacing)
import Html exposing (Html)
import Keys
import Platform.Sub exposing (batch)
import Shape exposing (Shape(..))
import Sound exposing (Sound, gain, oscillator, output, play)
import Sound.Properties as Props
import Style exposing (styleSheet)
import Track exposing (Track, Tracks)
import Styles exposing (Styles)


type alias Model =
    { audioSupported : Bool
    , keys : Keys.Model
    , tracks : Tracks
    }


type Msg
    = TrackMsg Track.Msg
    | KeysMsg Keys.Msg
    | NoOp


init : Bool -> ( Model, Cmd Msg )
init audioSupported =
    let
        ( keysModel, keysCmd ) =
            Keys.init
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
                    Keys.update msg keys
            in
                { model | keys = keysModel }
                    ! [ Cmd.map KeysMsg keysCmd
                      , Keys.getNotes keysModel
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
                    ! [ Keys.getNotes keys
                            |> renderSoundChain tracks
                            |> play
                      ]

        NoOp ->
            model ! []


renderSoundChain : Tracks -> List Keys.Note -> Sound
renderSoundChain tracks notes =
    let
        renderTrackSound index track =
            gain ("gain" ++ toString index)
                [ Props.gain (toFloat track.gain.volume / 100)
                ]
            <|
                List.map (renderOscillatorSound index track) notes

        renderOscillatorSound : Int -> Track -> Keys.Note -> Sound
        renderOscillatorSound trackNumber track note =
            oscillator ("oscillator" ++ toString trackNumber ++ "_" ++ toString note)
                (flatten
                    [ Keys.noteToFrequency note track.oscillator.octaveDelta
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
    Sub.map KeysMsg (Keys.subscriptions model.keys)


view : Model -> Html Msg
view model =
    viewport (styleSheet []) <|
        column Styles.none
            []
            (if not model.audioSupported then
                [ text "Audio is NOT supported in your browser" ]
             else
                [ (row Styles.none
                    [ center ]
                    [ Keys.view model.keys
                        |> Element.html
                        |> Element.map KeysMsg
                    ]
                  )
                , (Element.map TrackMsg <| tracksView model.tracks)
                ]
            )


tracksView : Tracks -> Element.Element Styles v Track.Msg
tracksView tracks =
    let
        renderedTracks : List (Element.Element Styles v Track.Msg)
        renderedTracks =
            List.indexedMap Track.view (Array.toList tracks)
    in
        column Styles.none [ spacing 10, padding 10, alignLeft ] renderedTracks
