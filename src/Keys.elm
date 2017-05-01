module Keys exposing (..)

import Dict
import Html exposing (Html, text)
import Keyboard exposing (KeyCode)
import String exposing (join)
import Svg exposing (Svg, g, polygon, svg)
import Svg.Attributes exposing (points, fill)
import Types exposing (..)


view : Html msg
view =
    let
        whiteKeys =
            List.range 0 6
                |> List.map2 (\f index -> f (whiteWidth * index))
                    [ whiteKeyView Left
                    , whiteKeyView Middle
                    , whiteKeyView Right
                    , whiteKeyView Left
                    , whiteKeyView Middle
                    , whiteKeyView Middle
                    , whiteKeyView Right
                    ]

        blackKeys =
            (List.range 0 1
                |> List.map
                    (\index ->
                        blackKeyView (whiteWidth - round (toFloat blackWidth / 2) + index * whiteWidth)
                    )
            )
                ++ (List.range 0 2
                        |> List.map
                            (\index ->
                                blackKeyView (whiteWidth - round (toFloat blackWidth / 2) + (index + 3) * whiteWidth)
                            )
                   )
    in
        svg
            []
            [ g
                []
                (whiteKeys ++ blackKeys)
            ]


type WhiteKeyType
    = Left
    | Right
    | Middle


whiteWidth : Int
whiteWidth =
    20


whiteHeight : Int
whiteHeight =
    100


blackWidth : Int
blackWidth =
    10


blackHeight : Int
blackHeight =
    70


pointToString : ( Int, Int ) -> String
pointToString point =
    toString (Tuple.first point) ++ "," ++ toString (Tuple.second point)


whiteKeyView : WhiteKeyType -> Int -> Svg msg
whiteKeyView keyType translate =
    let
        pointsSource =
            [ ( 0, blackHeight )
            , ( 0, whiteHeight )
            , ( whiteWidth, whiteHeight )
            , ( whiteWidth, blackHeight )
            ]
                -- Top right
                ++
                    (case keyType of
                        Left ->
                            [ ( whiteWidth - round (toFloat blackWidth / 2), blackHeight ) ]

                        Middle ->
                            [ ( whiteWidth - round (toFloat blackWidth / 2), blackHeight ) ]

                        Right ->
                            [ ( whiteWidth, 0 ) ]
                    )
                ++ [ ( whiteWidth - round (toFloat blackWidth / 2), 0 )
                   , ( round (toFloat blackWidth / 2), 0 )
                   ]
                ++ (case keyType of
                        Left ->
                            [ ( 0, 0 ) ]

                        Middle ->
                            [ ( round (toFloat blackWidth / 2), blackHeight ) ]

                        Right ->
                            [ ( round (toFloat blackWidth / 2), blackHeight ) ]
                   )
    in
        polygon
            [ Svg.Attributes.transform <| "translate(" ++ toString translate ++ ", 0)"
            , Svg.Attributes.stroke "black"
            , fill "yellow"
            , pointsSource
                |> List.map pointToString
                |> join " "
                |> points
            ]
            []


blackKeyView : Int -> Svg msg
blackKeyView translate =
    let
        pointsSource =
            [ ( 0, 0 )
            , ( 0, blackHeight )
            , ( blackWidth, blackHeight )
            , ( blackWidth, 0 )
            ]
    in
        polygon
            [ fill "grey"
            , Svg.Attributes.transform <| "translate(" ++ toString translate ++ ", 0)"
            , pointsSource
                |> List.map pointToString
                |> join " "
                |> points
            ]
            []


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
        , ( 75, "C+" )
        , ( 79, "C#+" )
        , ( 76, "D+" )
        , ( 80, "D#+" )
        , ( 186, "E+" )
        ]


noteToFrequencyMapping : Dict.Dict Note Frequency
noteToFrequencyMapping =
    Dict.fromList
        [ ( "C", 16.35 )
        , ( "C#", 17.32 )
        , ( "D", 18.35 )
        , ( "Eb", 19.45 )
        , ( "E", 20.6 )
        , ( "F", 21.83 )
        , ( "F#", 23.12 )
        , ( "G", 24.5 )
        , ( "G#", 25.96 )
        , ( "A", 27.5 )
        , ( "Bb", 29.14 )
        , ( "B", 30.87 )
        , ( "C+", 16.35 * 2 )
        , ( "C#+", 17.32 * 2 )
        , ( "D+", 18.35 * 2 )
        , ( "D#+", 19.45 * 2 )
        , ( "E+", 20.6 * 2 )
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
