module Main exposing (..)

import App exposing (Model, Msg, init, update, subscriptions, view)
import Html exposing (programWithFlags)


main : Program Bool Model Msg
main =
    programWithFlags { view = view, init = init, update = update, subscriptions = subscriptions }
