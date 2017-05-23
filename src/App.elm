module App exposing (..)

import Json.Decode
import Keys.State as KeysState
import Platform.Sub exposing (batch)
import Sound
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
          , analyzerData = []
          }
        , Cmd.map KeysMsg keysCmd
        )


defaultOscillator : Oscillator
defaultOscillator =
    { shape = Sine, volume = 100, octave = 3, fadeOutPeriod = 0.5 }


passToOscillator : Note -> Oscillator -> Maybe SoundDescriptor
passToOscillator note oscillator =
    KeysState.noteToFrequency note oscillator.octave
        |> Maybe.map (\x -> makeSoundDescriptor x oscillator)


passToOscillators : List Oscillator -> Note -> List SoundDescriptor
passToOscillators oscillators note =
    List.filterMap (passToOscillator note) oscillators


connectChain : List Oscillator -> List Note -> Cmd Msg
connectChain oscillators notes =
    notes
        |> List.concatMap (passToOscillators oscillators)
        |> Sound.startPlaying


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ oscillators } as model) =
    case msg of
        KeysMsg msg ->
            let
                ( keysModel, keysCmd ) =
                    KeysState.update msg model.keys
            in
                { model | keys = keysModel }
                    ! [ Cmd.map KeysMsg keysCmd
                      , KeysState.getNotes keysModel
                            |> connectChain oscillators
                      ]

        ChangeOctaveDelta index octave ->
            let
                changeOctave oscillator =
                    { oscillator | octave = octave }

                newOscillators =
                    updateOscillatorAtIndex index changeOctave oscillators
            in
                ( { model
                    | oscillators = newOscillators
                  }
                , KeysState.getNotes model.keys
                    |> connectChain newOscillators
                )

        ChangeVolume index volume ->
            let
                changeVolume oscillator =
                    { oscillator | volume = volume }

                newOscillators =
                    updateOscillatorAtIndex index changeVolume oscillators
            in
                ( { model
                    | oscillators = newOscillators
                  }
                , KeysState.getNotes model.keys
                    |> connectChain newOscillators
                )

        ChangeShape index shape ->
            let
                changeShape oscillator =
                    { oscillator | shape = shape }

                newOscillators =
                    updateOscillatorAtIndex index changeShape oscillators
            in
                ( { model
                    | oscillators = newOscillators
                  }
                , KeysState.getNotes model.keys
                    |> connectChain newOscillators
                )

        UpdateAnalyzerData data ->
            { model | analyzerData = data } ! []

        NoOp ->
            model ! []


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
