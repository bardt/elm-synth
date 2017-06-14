module App exposing (..)

import Array
import Json.Decode
import Keys.State as KeysState
import Keys.Types as KeysTypes
import List.Extra
import Platform.Sub exposing (batch)
import Sound exposing (..)
import Types exposing (..)


init : Bool -> ( Model, Cmd Msg )
init audioSupported =
    let
        ( keysModel, keysCmd ) =
            KeysState.init
    in
        ( { keys = keysModel
          , audioSupported = audioSupported
          , analyzerEnabled = False
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
          , analyzerData = []
          }
        , Cmd.map KeysMsg keysCmd
        )


renderSoundChain : List Track -> List KeysTypes.Note -> Sound
renderSoundChain tracks notes =
    let
        renderTrackSound track =
            gain
                [ ( "volume", toString track.gain.volume )
                ]
            <|
                List.map (renderOscillatorSound track) notes

        renderOscillatorSound : Track -> KeysTypes.Note -> Sound
        renderOscillatorSound track note =
            oscillator
                (flatten
                    [ KeysState.noteToFrequency note track.oscillator.octaveDelta
                        |> Maybe.map (\f -> ( "frequency", toString f ))
                    ]
                )
                []

        flatten : List (Maybe a) -> List a
        flatten list =
            List.filterMap identity list
    in
        output <|
            List.map renderTrackSound tracks


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ tracks } as model) =
    case msg of
        KeysMsg msg ->
            let
                ( keysModel, keysCmd ) =
                    KeysState.update msg model.keys
            in
                { model | keys = keysModel }
                    ! [ Cmd.map KeysMsg keysCmd
                      , KeysState.getNotes keysModel
                            |> renderSoundChain (Array.toList model.tracks)
                            |> encodeSound
                            |> startPlaying
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
                    ! []

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
                    ! []

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
                    ! []

        UpdateAnalyzerData data ->
            { model | analyzerData = data } ! []

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
    let
        analyzerSubscription =
            Sound.updateAnalyzer
                (\str ->
                    case Json.Decode.decodeValue decodeAnalyzerData str of
                        Ok data ->
                            UpdateAnalyzerData data

                        Err str ->
                            NoOp
                )
    in
        Sub.batch <|
            List.filterMap identity
                [ Just <| Sub.map KeysMsg (KeysState.subscriptions model.keys)
                , (if model.analyzerEnabled then
                    Just analyzerSubscription
                   else
                    Nothing
                  )
                ]
