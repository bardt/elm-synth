module App exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Keyboard exposing (KeyCode, downs, ups)
import List.Extra exposing (unique)
import Platform.Sub exposing (batch)
import Sound
import Types exposing (..)
import Toolkit.Helpers exposing (maybeList)


type alias Model =
    { audioSupported : Bool
    , playing : Bool
    , pressed : List KeyCode
    , oscillators : List Oscillator
    }


init : Bool -> ( Model, Cmd Msg )
init audioSupported =
    ( { audioSupported = audioSupported
      , playing = False
      , pressed = []
      , oscillators =
            [ { defaultOscillator
                | volume = 50
                , octave = 2
              }
            , { shape = Triangle
              , octave = 5
              , volume = 12
              , fadeOutPeriod = 1
              }
            ]
      }
    , Cmd.none
    )


type Msg
    = PlayPause
    | Keydown KeyCode
    | Keyup KeyCode
    | NoOp


keyToNoteMapping : Dict.Dict KeyCode Note
keyToNoteMapping =
    Dict.fromList
        [ ( 65, "C" )
        , ( 87, "C#" )
        , ( 83, "D" )
        , ( 69, "Eb" )
        , ( 68, "E" )
        , ( 70, "F" )
        , ( 84, "F#" )
        , ( 71, "G" )
        , ( 89, "G#" )
        , ( 72, "A" )
        , ( 85, "Bb" )
        , ( 74, "B" )
        , ( 75, "C+" )
        , ( 79, "C#+" )
        , ( 76, "D+" )
        , ( 80, "D#+" )
        , ( 186, "E+" )
        ]


noteToFrequencyMapping : Dict.Dict Note Frequency
noteToFrequencyMapping =
    Dict.fromList
        [ ( "C", 16.35 )
        , ( "C#", 17.32 )
        , ( "D", 18.35 )
        , ( "Eb", 19.45 )
        , ( "E", 20.6 )
        , ( "F", 21.83 )
        , ( "F#", 23.12 )
        , ( "G", 24.5 )
        , ( "G#", 25.96 )
        , ( "A", 27.5 )
        , ( "Bb", 29.14 )
        , ( "B", 30.87 )
        , ( "C+", 16.35 * 2 )
        , ( "C#+", 17.32 * 2 )
        , ( "D+", 18.35 * 2 )
        , ( "D#+", 19.45 * 2 )
        , ( "E+", 20.6 * 2 )
        ]


keyToNote : KeyCode -> Maybe Note
keyToNote keyCode =
    Dict.get keyCode keyToNoteMapping


noteToFrequency : Note -> Maybe Frequency
noteToFrequency note =
    Dict.get note noteToFrequencyMapping


keyToFrequency : KeyCode -> Maybe Frequency
keyToFrequency key =
    key
        |> keyToNote
        |> Maybe.andThen noteToFrequency


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        soundDescriptors : List Oscillator -> List Frequency -> List SoundDescriptor
        soundDescriptors oscillators freqs =
            List.concatMap (\f -> List.map (makeSoundDescriptor f) oscillators) freqs
    in
        case msg of
            PlayPause ->
                ( { model | playing = not model.playing }, Sound.sendPlaytoJs (not model.playing) )

            Keydown key ->
                let
                    keysPressed =
                        unique (key :: model.pressed)
                in
                    ( { model
                        | pressed = keysPressed
                      }
                    , keysPressed
                        |> List.map keyToFrequency
                        |> maybeList
                        |> Maybe.map (soundDescriptors model.oscillators)
                        |> Maybe.map Sound.startPlaying
                        |> Maybe.withDefault Cmd.none
                    )

            Keyup key ->
                let
                    keysPressed =
                        List.filter (\k -> k /= key) model.pressed
                in
                    ( { model
                        | pressed = keysPressed
                      }
                    , keysPressed
                        |> List.map keyToFrequency
                        |> maybeList
                        |> Maybe.map (soundDescriptors model.oscillators)
                        |> Maybe.map Sound.startPlaying
                        |> Maybe.withDefault Cmd.none
                    )

            NoOp ->
                ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        supportedMessage =
            if model.audioSupported then
                "Audio supported"
            else
                "Audio NOT supported"
    in
        div []
            [ div []
                [ text supportedMessage ]
            , div [] <|
                [ button [ onClick PlayPause ]
                    [ text
                        (if model.playing then
                            "Pause"
                         else
                            "Play"
                        )
                    ]
                ]
                    ++ (List.map oscillatorView model.oscillators)
            ]


defaultOscillator : Oscillator
defaultOscillator =
    { shape = Sine, volume = 100, octave = 3, fadeOutPeriod = 0.5 }


oscillatorView : Oscillator -> Html Msg
oscillatorView o =
    div []
        [ input [ Html.Attributes.type_ "number", value <| toString o.octave ] []
          -- Shape
        , optgroup [] [ option [] [] ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    batch [ downs Keydown, ups Keyup ]
