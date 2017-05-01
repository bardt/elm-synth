module App exposing (..)

import FormHelpers
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode as Json
import Keyboard exposing (KeyCode, downs, ups)
import Keys exposing (keyToFrequency)
import List.Extra exposing (unique)
import Platform.Sub exposing (batch)
import Shape
import Sound
import Toolkit.Helpers exposing (maybeList)
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
    = Keydown KeyCode
    | Keyup KeyCode
    | ChangeOctave Int Octave
    | ChangeVolume Int Volume
    | ChangeShape Int Shape
    | NoOp


makeSound : List KeyCode -> List Oscillator -> Cmd Msg
makeSound keys oscillators =
    let
        soundDescriptors : List Oscillator -> List Frequency -> List SoundDescriptor
        soundDescriptors oscillators freqs =
            List.concatMap (\f -> List.map (makeSoundDescriptor f) oscillators) freqs
    in
        keys
            |> List.map keyToFrequency
            |> maybeList
            |> Maybe.map (soundDescriptors oscillators)
            |> Maybe.map Sound.startPlaying
            |> Maybe.withDefault Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Keydown key ->
            let
                keysPressed =
                    unique (key :: model.pressed)
            in
                ( { model
                    | pressed = keysPressed
                  }
                , makeSound keysPressed model.oscillators
                )

        Keyup key ->
            let
                keysPressed =
                    List.filter (\k -> k /= key) model.pressed
            in
                ( { model
                    | pressed = keysPressed
                  }
                , makeSound keysPressed model.oscillators
                )

        ChangeOctave index octave ->
            let
                changeOctave oscillator =
                    { oscillator | octave = octave }

                newOscillators =
                    updateOscillatorAtIndex index changeOctave model.oscillators
            in
                ( { model
                    | oscillators = newOscillators
                  }
                , makeSound model.pressed newOscillators
                )

        ChangeVolume index volume ->
            let
                changeVolume oscillator =
                    { oscillator | volume = volume }

                newOscillators =
                    updateOscillatorAtIndex index changeVolume model.oscillators
            in
                ( { model
                    | oscillators = newOscillators
                  }
                , makeSound model.pressed newOscillators
                )

        ChangeShape index shape ->
            let
                changeShape oscillator =
                    { oscillator | shape = shape }

                newOscillators =
                    updateOscillatorAtIndex index changeShape model.oscillators
            in
                ( { model
                    | oscillators = newOscillators
                  }
                , makeSound model.pressed newOscillators
                )

        NoOp ->
            ( model, Cmd.none )


updateOscillatorAtIndex : Int -> (Oscillator -> Oscillator) -> List Oscillator -> List Oscillator
updateOscillatorAtIndex index update oscillators =
    List.indexedMap
        (\i o ->
            if (i == index) then
                update o
            else
                o
        )
        oscillators


view : Model -> Html Msg
view model =
    div []
        (if not model.audioSupported then
            [ text "Audio NOT supported" ]
         else
            Keys.view :: List.indexedMap oscillatorView model.oscillators
        )


defaultOscillator : Oscillator
defaultOscillator =
    { shape = Sine, volume = 100, octave = 3, fadeOutPeriod = 0.5 }


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
            [ Html.Attributes.type_ "number"
            , value <| toString oscillator.octave
            , FormHelpers.onIntInput (ChangeOctave index)
            ]
            []
        ]


volumeChangeView : Int -> Oscillator -> Html Msg
volumeChangeView index oscillator =
    label []
        [ text "Volume: "
        , input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max "100"
            , Html.Attributes.value <| toString oscillator.volume
            , FormHelpers.onIntInput (ChangeVolume index)
            ]
            []
        , input
            [ type_ "number"
            , FormHelpers.onIntInput (ChangeVolume index)
            , Html.Attributes.value <| toString oscillator.volume
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
            [ option [ value (toString Sine), selected (oscillator.shape == Sine) ] [ text "Sine" ]
            , option [ value (toString Triangle), selected (oscillator.shape == Triangle) ] [ text "Triangle" ]
            , option [ value (toString Square), selected (oscillator.shape == Square) ] [ text "Square" ]
            , option [ value (toString Sawtooth), selected (oscillator.shape == Sawtooth) ] [ text "Sawtooth" ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    batch [ downs Keydown, ups Keyup ]
