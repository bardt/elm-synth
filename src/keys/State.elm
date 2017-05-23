module Keys.State exposing (..)

import Keys.Types exposing (..)
import Dict
import List.Extra exposing (find)
import Keyboard exposing (KeyCode, downs, ups)


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    let
        octave =
            4
    in
        { keys = keyboard octave
        , octave = octave
        }


keyboard : Octave -> List Key
keyboard oct =
    let
        notes =
            [ Note C Natural oct
            , Note C Sharp oct
            , Note D Natural oct
            , Note D Sharp oct
            , Note E Natural oct
            , Note F Natural oct
            , Note F Sharp oct
            , Note G Natural oct
            , Note G Sharp oct
            , Note A Natural oct
            , Note A Sharp oct
            , Note B Natural oct
            , Note C Natural (oct + 1)
            , Note C Sharp (oct + 1)
            , Note D Natural (oct + 1)
            , Note D Sharp (oct + 1)
            , Note E Natural (oct + 1)
            ]
    in
        List.map (Key False) notes


noteToFrequencyMapping : List ( ( Letter, NoteType ), Frequency )
noteToFrequencyMapping =
    [ ( ( C, Natural ), 16.35 )
    , ( ( C, Sharp ), 17.32 )
    , ( ( D, Natural ), 18.35 )
    , ( ( D, Sharp ), 19.45 )
    , ( ( E, Natural ), 20.6 )
    , ( ( F, Natural ), 21.83 )
    , ( ( F, Sharp ), 23.12 )
    , ( ( G, Natural ), 24.5 )
    , ( ( G, Sharp ), 25.96 )
    , ( ( A, Natural ), 27.5 )
    , ( ( A, Sharp ), 29.14 )
    , ( ( B, Natural ), 30.87 )
    ]


noteToFrequency : Note -> Int -> Maybe Frequency
noteToFrequency note octaveDelta =
    find (\( n, _ ) -> n == ( note.letter, note.type_ )) noteToFrequencyMapping
        |> Maybe.map (\x -> (Tuple.second x) * (toFloat (2 ^ (note.octave + octaveDelta))))


getNotes : Model -> List Note
getNotes model =
    model.keys
        |> List.filter (.pressed)
        |> List.map (.note)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ keys } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        KeyPressed key ->
            { model
                | keys =
                    List.map
                        (\k ->
                            if k.note == key.note then
                                { k | pressed = True }
                            else
                                k
                        )
                        keys
            }
                ! []

        KeyReleased key ->
            { model
                | keys =
                    List.map
                        (\k ->
                            if k.note == key.note then
                                { k | pressed = False }
                            else
                                k
                        )
                        keys
            }
                ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        codeToKey =
            List.map2 (\code key -> ( code, key )) [ 65, 87, 83, 69, 68, 70, 84, 71, 89, 72, 85, 74, 75, 79, 76, 80, 186 ] model.keys
                |> Dict.fromList

        onKeyDown : KeyCode -> Msg
        onKeyDown code =
            Dict.get code codeToKey
                |> Maybe.map KeyPressed
                |> Maybe.withDefault NoOp

        onKeyUp : KeyCode -> Msg
        onKeyUp code =
            Dict.get code codeToKey
                |> Maybe.map KeyReleased
                |> Maybe.withDefault NoOp
    in
        Sub.batch [ downs onKeyDown, ups onKeyUp ]
