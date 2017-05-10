module App exposing (..)

import FormHelpers
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode as Json
import Keys
import Platform.Sub exposing (batch)
import Shape
import Sound
import Types exposing (..)


type alias Model =
    { audioSupported : Bool
    , keys : Keys.Model
    , oscillators : List Oscillator
    }


init : Bool -> ( Model, Cmd Msg )
init audioSupported =
    let
        ( keysModel, keysCmd ) =
            Keys.init
    in
        ( { keys = keysModel
          , audioSupported = audioSupported
          , oscillators =
                [ { defaultOscillator
                    | volume = 50
                    , octave = 0
                  }
                , { shape = Triangle
                  , octave = 2
                  , volume = 12
                  , fadeOutPeriod = 1
                  }
                ]
          }
        , Cmd.map KeysMsg keysCmd
        )


type Msg
    = ChangeOctaveDelta Int OctaveDelta
    | ChangeVolume Int Volume
    | ChangeShape Int Shape
    | KeysMsg Keys.Msg
    | NoOp


passToOscillator : Keys.Note -> Oscillator -> Maybe SoundDescriptor
passToOscillator note oscillator =
    Keys.noteToFrequency note oscillator.octave
        |> Maybe.map (\x -> makeSoundDescriptor x oscillator)


passToOscillators : List Oscillator -> Keys.Note -> List SoundDescriptor
passToOscillators oscillators note =
    List.filterMap (passToOscillator note) oscillators


connectChain : List Oscillator -> List Keys.Note -> Cmd Msg
connectChain oscillators notes =
    notes
        |> List.concatMap (passToOscillators oscillators)
        |> Sound.startPlaying


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeysMsg msg ->
            let
                ( keysModel, keysCmd ) =
                    Keys.update msg model.keys
            in
                ( { model | keys = keysModel }
                , Cmd.batch
                    [ Cmd.map KeysMsg keysCmd
                    , Keys.getNotes keysModel
                        |> connectChain model.oscillators
                    ]
                )

        ChangeOctaveDelta index octave ->
            let
                changeOctave oscillator =
                    { oscillator | octave = octave }

                newOscillators =
                    updateOscillatorAtIndex index changeOctave model.oscillators
            in
                ( { model
                    | oscillators = newOscillators
                  }
                , Keys.getNotes model.keys
                    |> connectChain newOscillators
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
                , Keys.getNotes model.keys
                    |> connectChain newOscillators
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
                , Keys.getNotes model.keys
                    |> connectChain newOscillators
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
            (Html.map KeysMsg <|
                Keys.view model.keys
            )
                :: List.indexedMap oscillatorView model.oscillators
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
            , FormHelpers.onIntInput (ChangeOctaveDelta index)
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
    Sub.map KeysMsg (Keys.subscriptions model.keys)
