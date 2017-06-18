module Keys.View exposing (view)

import Html exposing (Html)
import Html.Events exposing (onMouseDown, onMouseLeave, onMouseUp)
import Keys.Types exposing (..)
import List.Extra exposing (findIndex)
import String exposing (join)
import Svg exposing (Svg, g, polygon, svg, text, text_)
import Svg.Attributes exposing (fill, fontSize, points, stroke, transform, x, y)


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
    20


blackHeight : Int
blackHeight =
    90


gapSize : Int
gapSize =
    1


pointToString : ( Int, Int ) -> String
pointToString point =
    toString (Tuple.first point) ++ "," ++ toString (Tuple.second point)


noteToString : Note -> String
noteToString note =
    let
        typeToString type_ =
            case type_ of
                Sharp ->
                    "#"

                Natural ->
                    ""
    in
        toString note.letter ++ typeToString note.type_ ++ toString note.octave


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
        g [ transform <| "translate(" ++ toString translate ++ ", 0)" ]
            [ polygon
                [ stroke "black"
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
                , onMouseDown (KeyPressed key)
                , onMouseUp (KeyReleased key)
                , onMouseLeave (KeyReleased key)
                ]
                []
            , text_
                [ fill "black"
                , fontSize <| toString <| whiteWidth // 2
                , y <| toString <| whiteHeight - gapSize * 4
                , x <| toString <| gapSize * 2
                ]
                [ text <| noteToString key.note
                ]
            ]


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
        g [ transform <| "translate(" ++ toString translate ++ ", 0)" ]
            [ polygon
                [ fill
                    (if key.pressed then
                        "red"
                     else
                        "grey"
                    )
                , pointsSource
                    |> List.map pointToString
                    |> join " "
                    |> points
                , onMouseDown (KeyPressed key)
                , onMouseUp (KeyReleased key)
                , onMouseLeave (KeyReleased key)
                ]
                []
            , text_
                [ fill "white"
                , fontSize <| toString <| blackWidth // 2
                , y <| toString <| blackHeight - gapSize * 4
                , x <| toString <| gapSize * 2
                ]
                [ text <| noteToString key.note
                ]
            ]
