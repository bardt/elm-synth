module App exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Sound
import Keyboard exposing (KeyCode, ups, downs)
import Platform.Sub exposing (batch)
import Dict


type alias Model =
    { message : String
    , audioSupported : Bool
    , playing : Bool
    , pressed : List KeyCode
    }


init : Bool -> ( Model, Cmd Msg )
init audioSupported =
    ( { message = "Your Elm App is working!"
      , audioSupported = audioSupported
      , playing = False
      , pressed = []
      }
    , Cmd.none
    )


type Msg
    = PlayPause
    | Keydown KeyCode
    | Keyup KeyCode
    | NoOp


type alias Note =
    String


keyToNoteMapping : Dict.Dict KeyCode Note
keyToNoteMapping =
    Dict.fromList
        [ ( 65, "C3" )
        , ( 87, "C3#" )
        , ( 83, "D3" )
        , ( 69, "E3b" )
        , ( 68, "E3" )
        , ( 70, "F3" )
        , ( 84, "F3#" )
        , ( 71, "G3" )
        , ( 89, "G3#" )
        , ( 72, "A3" )
        , ( 85, "B3b" )
        , ( 74, "B3" )
        ]


noteToFrequencyMapping : Dict.Dict Note Frequency
noteToFrequencyMapping =
    Dict.fromList
        [ ( "C3", 130.8 )
        , ( "C3#", 138.6 )
        , ( "D3", 146.8 )
        , ( "E3b", 155.6 )
        , ( "E3", 164.8 )
        , ( "F3", 174.6 )
        , ( "F3#", 185.0 )
        , ( "G3", 196.0 )
        , ( "G3#", 207.7 )
        , ( "A3", 220.0 )
        , ( "B3b", 233.1 )
        , ( "B3", 246.9 )
        ]


keyToNote : KeyCode -> Maybe Note
keyToNote keyCode =
    Dict.get keyCode keyToNoteMapping


noteToFrequency : Note -> Maybe Frequency
noteToFrequency note =
    Dict.get note noteToFrequencyMapping


type alias Frequency =
    Float


keyToFrequency : KeyCode -> Maybe Frequency
keyToFrequency key =
    key
        |> keyToNote
        |> Maybe.andThen noteToFrequency


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayPause ->
            ( { model | playing = not model.playing }, Sound.sendPlaytoJs (not model.playing) )

        Keydown key ->
            ( { model | pressed = key :: model.pressed }
            , key
                |> keyToFrequency
                |> Maybe.map
                    Sound.startPlayingHZ
                |> Maybe.withDefault Cmd.none
            )

        Keyup key ->
            ( { model | pressed = List.filter (\k -> k /= key) model.pressed }
            , key
                |> keyToFrequency
                |> Maybe.map
                    Sound.stopPlayingHZ
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
            , div []
                [ button [ onClick PlayPause ]
                    [ text
                        (if model.playing then
                            "Pause"
                         else
                            "Play"
                        )
                    ]
                ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    batch [ downs Keydown, ups Keyup ]
