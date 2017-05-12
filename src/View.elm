module View exposing (..)

import Keys.View as KeysView
import Types exposing (..)
import Html exposing (Html, Attribute, div, text, label, input, select, option)
import Html.Attributes as Attrs
import Html.Events exposing (on, onClick, onInput, targetValue)
import FormHelpers
import Json.Decode as Json
import Shape


view : Model -> Html Msg
view model =
    div []
        (if not model.audioSupported then
            [ text "Audio NOT supported" ]
         else
            (Html.map KeysMsg <|
                KeysView.view model.keys
            )
                :: List.indexedMap oscillatorView model.oscillators
        )


oscillatorView : Int -> Oscillator -> Html Msg
oscillatorView index o =
    Html.form []
        [ div []
            [ octaveChangeView index o ]
        , div []
            [ volumeChangeView index o ]
        , div []
            [ shapeSelectView index o ]
        ]


octaveChangeView : Int -> Oscillator -> Html Msg
octaveChangeView index oscillator =
    label []
        [ text "Octave: "
        , input
            [ Attrs.type_ "number"
            , Attrs.value <| toString oscillator.octave
            , FormHelpers.onIntInput (ChangeOctaveDelta index)
            ]
            []
        ]


volumeChangeView : Int -> Oscillator -> Html Msg
volumeChangeView index oscillator =
    label []
        [ text "Volume: "
        , input
            [ Attrs.type_ "range"
            , Attrs.min "0"
            , Attrs.max "100"
            , Attrs.value <| toString oscillator.volume
            , FormHelpers.onIntInput (ChangeVolume index)
            ]
            []
        , input
            [ Attrs.type_ "number"
            , FormHelpers.onIntInput (ChangeVolume index)
            , Attrs.value <| toString oscillator.volume
            ]
            []
        ]


shapeSelectView : Int -> Oscillator -> Html Msg
shapeSelectView index oscillator =
    let
        shapeDecoder : String -> Json.Decoder Shape
        shapeDecoder string =
            case (Shape.fromString string) of
                Ok shape ->
                    Json.succeed shape

                Err message ->
                    Json.fail message

        onChange : (Shape -> Msg) -> Attribute Msg
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
                , Attrs.selected (oscillator.shape == Sine)
                ]
                [ text "Sine" ]
            , option [ Attrs.value (toString Triangle), Attrs.selected (oscillator.shape == Triangle) ] [ text "Triangle" ]
            , option [ Attrs.value (toString Square), Attrs.selected (oscillator.shape == Square) ] [ text "Square" ]
            , option [ Attrs.value (toString Sawtooth), Attrs.selected (oscillator.shape == Sawtooth) ] [ text "Sawtooth" ]
            ]
