module App exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Model =
    { message : String
    , audioSupported : Bool
    , playing : Bool
    }


init : Bool -> ( Model, Cmd Msg )
init audioSupported =
    ( { message = "Your Elm App is working!"
      , audioSupported = audioSupported
      , playing = False
      }
    , Cmd.none
    )


type Msg
    = PlayPause
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayPause ->
            ( { model | playing = not model.playing }, Cmd.none )

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
    Sub.none
