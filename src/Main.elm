module Main exposing (..)

import App exposing (init, update, subscriptions)
import Html exposing (programWithFlags)
import Types exposing (Model, Msg)
import View exposing (view)


main : Program Bool Model Msg
main =
    programWithFlags { view = view, init = init, update = update, subscriptions = subscriptions }
