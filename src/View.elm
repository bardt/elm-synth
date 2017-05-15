module View exposing (..)

import Keys.View as KeysView
import Types exposing (..)
import Html exposing (Html, Attribute, div, text, label, input, select, option)
import Html.Attributes as Attrs
import Html.Events exposing (on, onClick, onInput, targetValue)
import FormHelpers
import Json.Decode as Json
import Shape
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List.Extra


view : Model -> Html Msg
view model =
    div []
        (if not model.audioSupported then
            [ Html.text "Audio NOT supported" ]
         else
            (Html.map KeysMsg <|
                KeysView.view model.keys
            )
                :: analyzer model.analyzerData
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
        [ Html.text "Octave: "
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
        [ Html.text "Volume: "
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
                , Attrs.selected (oscillator.shape == Sine)
                ]
                [ Html.text "Sine" ]
            , option [ Attrs.value (toString Triangle), Attrs.selected (oscillator.shape == Triangle) ] [ Html.text "Triangle" ]
            , option [ Attrs.value (toString Square), Attrs.selected (oscillator.shape == Square) ] [ Html.text "Square" ]
            , option [ Attrs.value (toString Sawtooth), Attrs.selected (oscillator.shape == Sawtooth) ] [ Html.text "Sawtooth" ]
            ]


analyzer : AnalyzerData -> Html Msg
analyzer data =
    let
        dataLine =
            let
                pointsString =
                    let
                        minMaybe =
                            List.minimum data
                    in
                        case minMaybe of
                            Nothing ->
                                ""

                            Just minDatum ->
                                data
                                    |> List.Extra.dropWhile ((/=) minDatum)
                                    |> List.Extra.dropWhile ((>) 128)
                                    |> List.Extra.dropWhile ((<=) 128)
                                    |> List.indexedMap
                                        (\index datum ->
                                            toString index ++ "," ++ toString datum
                                        )
                                    |> String.join " "
            in
                polyline
                    [ stroke "black"
                    , fill "none"
                    , points pointsString
                    ]
                    []
    in
        svg
            [ width <| toString <| List.length data
            , height <| toString <| 256
            ]
            [ dataLine ]
