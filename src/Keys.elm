module Keys exposing (..)

import Dict
import Html exposing (Html, text)
import Keyboard exposing (KeyCode)
import String exposing (join)
import Svg exposing (Svg, g, polygon, svg)
import Svg.Attributes exposing (points, fill)
import Types exposing (..)
import Html exposing (program)
import List.Extra exposing (findIndex)
import Html.Events
import Keyboard exposing (KeyCode, downs, ups)


{-| Represents phisical key on a keyboard
-}
type alias Key =
    { pressed : Bool
    , note : Note
    }


type alias Note =
    { letter : Letter
    , type_ : NoteType
    , octave : Octave
    }


type Letter
    = C
    | D
    | E
    | F
    | G
    | A
    | B


letters : List Letter
letters =
    [ C, D, E, F, G, A, B ]


type NoteType
    = Natural
    | Sharp


type alias Octave =
    Int


type alias Model =
    { keys : List Key
    , octave : Octave
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


initModel : Model
initModel =
    let
        octave =
            4
    in
        { keys = keyboard octave
        , octave = octave
        }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


type Msg
    = KeyPressed Key
    | KeyReleased Key
    | NoOp


main : Program Never Model Msg
main =
    program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        KeyPressed key ->
            ( { model
                | keys =
                    List.map
                        (\k ->
                            if k.note == key.note then
                                { k | pressed = True }
                            else
                                k
                        )
                        model.keys
              }
            , Cmd.none
            )

        KeyReleased key ->
            ( { model
                | keys =
                    List.map
                        (\k ->
                            if k.note == key.note then
                                { k | pressed = False }
                            else
                                k
                        )
                        model.keys
              }
            , Cmd.none
            )


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


view : Model -> Html Msg
view model =
    svg
        []
        [ g
            []
            (keyViews
                model
            )
        ]


keyViews : Model -> List (Svg Msg)
keyViews model =
    model.keys
        |> List.filterMap
            (\k ->
                if k.note.type_ == Natural then
                    let
                        keyViewBuilder =
                            case k.note.letter of
                                C ->
                                    whiteKeyView k Left

                                D ->
                                    whiteKeyView k Middle

                                E ->
                                    whiteKeyView k Right

                                F ->
                                    whiteKeyView k Left

                                G ->
                                    whiteKeyView k Middle

                                A ->
                                    whiteKeyView k Middle

                                B ->
                                    whiteKeyView k Right
                    in
                        findIndex ((==) k.note.letter) letters
                            |> Maybe.map (\index -> whiteWidth * index + whiteWidth * 7 * (k.note.octave - model.octave))
                            |> Maybe.map keyViewBuilder
                else
                    findIndex ((==) k.note.letter) letters
                        |> Maybe.map (\index -> whiteWidth * (index + 1) - round (toFloat blackWidth / 2) + whiteWidth * 7 * (k.note.octave - model.octave))
                        |> Maybe.map (blackKeyView k)
            )


type WhiteKeyType
    = Left
    | Right
    | Middle


whiteWidth : Int
whiteWidth =
    30


whiteHeight : Int
whiteHeight =
    150


blackWidth : Int
blackWidth =
    15


blackHeight : Int
blackHeight =
    90


gapSize : Int
gapSize =
    1


pointToString : ( Int, Int ) -> String
pointToString point =
    toString (Tuple.first point) ++ "," ++ toString (Tuple.second point)


whiteKeyView : Key -> WhiteKeyType -> Int -> Svg Msg
whiteKeyView key keyType translate =
    let
        pointsSource =
            [ ( gapSize, blackHeight + gapSize )
            , ( gapSize, whiteHeight - gapSize )
            , ( whiteWidth - gapSize, whiteHeight - gapSize )
            , ( whiteWidth - gapSize, blackHeight + gapSize )
            ]
                -- Top right
                ++
                    (case keyType of
                        Left ->
                            [ ( whiteWidth - round (toFloat blackWidth / 2) - gapSize, blackHeight + gapSize ) ]

                        Middle ->
                            [ ( whiteWidth - round (toFloat blackWidth / 2) - gapSize, blackHeight + gapSize ) ]

                        Right ->
                            [ ( whiteWidth - gapSize, 0 ) ]
                    )
                ++ [ ( whiteWidth - round (toFloat blackWidth / 2) - gapSize, 0 )
                   , ( round (toFloat blackWidth / 2) + gapSize, 0 )
                   ]
                ++ (case keyType of
                        Left ->
                            [ ( gapSize, 0 ) ]

                        Middle ->
                            [ ( round (toFloat blackWidth / 2) + gapSize, blackHeight + gapSize ) ]

                        Right ->
                            [ ( round (toFloat blackWidth / 2) + gapSize, blackHeight + gapSize ) ]
                   )
    in
        polygon
            [ Svg.Attributes.transform <| "translate(" ++ toString translate ++ ", 0)"
            , Svg.Attributes.stroke "black"
            , fill
                (if key.pressed then
                    "red"
                 else
                    "yellow"
                )
            , pointsSource
                |> List.map pointToString
                |> join " "
                |> points
            , Html.Events.onMouseDown (KeyPressed key)
            , Html.Events.onMouseUp (KeyReleased key)
            , Html.Events.onMouseLeave (KeyReleased key)
            ]
            []


blackKeyView : Key -> Int -> Svg Msg
blackKeyView key translate =
    let
        pointsSource =
            [ ( gapSize, 0 )
            , ( gapSize, blackHeight - gapSize )
            , ( blackWidth - gapSize, blackHeight - gapSize )
            , ( blackWidth - gapSize, 0 )
            ]
    in
        polygon
            [ fill
                (if key.pressed then
                    "red"
                 else
                    "grey"
                )
            , Svg.Attributes.transform <| "translate(" ++ toString translate ++ ", 0)"
            , pointsSource
                |> List.map pointToString
                |> join " "
                |> points
            , Html.Events.onMouseDown (KeyPressed key)
            , Html.Events.onMouseUp (KeyReleased key)
            , Html.Events.onMouseLeave (KeyReleased key)
            ]
            []


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
    List.Extra.find (\( n, _ ) -> n == ( note.letter, note.type_ )) noteToFrequencyMapping
        |> Maybe.map (\x -> (Tuple.second x) * (toFloat (2 ^ (note.octave + octaveDelta))))


getNotes : Model -> List Note
getNotes model =
    model.keys
        |> List.filter (.pressed)
        |> List.map (.note)
