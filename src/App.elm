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
      , oscillators = [ defaultOscillator ]
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
        ]


noteToFrequencyMapping : Dict.Dict Note Frequency
noteToFrequencyMapping =
    Dict.fromList
        [ ( "C", 130.8 / 3 )
        , ( "C#", 138.6 / 3 )
        , ( "D", 146.8 / 3 )
        , ( "Eb", 155.6 / 3 )
        , ( "E", 164.8 / 3 )
        , ( "F", 174.6 / 3 )
        , ( "F#", 185.0 / 3 )
        , ( "G", 196.0 / 3 )
        , ( "G#", 207.7 / 3 )
        , ( "A", 220.0 / 3 )
        , ( "Bb", 233.1 / 3 )
        , ( "B", 246.9 / 3 )
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
        oscillatorByKey key =
            keyToFrequency key
                |> Maybe.map (\f -> { defaultOscillator | baseFrequency = f })
    in
        case msg of
            PlayPause ->
                ( { model | playing = not model.playing }, Sound.sendPlaytoJs (not model.playing) )

            Keydown key ->
                let
                    keysPressed =
                        unique (key :: model.pressed)

                    oscillators =
                        List.filterMap oscillatorByKey keysPressed
                in
                    ( { model
                        | pressed = keysPressed
                        , oscillators = oscillators
                      }
                    , oscillators
                        |> List.map serialize
                        |> Sound.startPlaying
                    )

            Keyup key ->
                let
                    keysPressed =
                        List.filter (\k -> k /= key) model.pressed

                    oscillators =
                        List.filterMap oscillatorByKey keysPressed
                in
                    ( { model
                        | pressed = keysPressed
                        , oscillators = oscillators
                      }
                    , oscillators
                        |> List.map serialize
                        |> Sound.startPlaying
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
    { shape = Sine, octave = 3, fadeOutPeriod = 0.5, baseFrequency = 0.0 }


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
