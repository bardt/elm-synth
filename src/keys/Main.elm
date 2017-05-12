module Keys.Main exposing (main)

import Html exposing (program)
import Keys.Types exposing (..)
import Keys.State exposing (init, update, subscriptions)
import Keys.View exposing (view)


main : Program Never Model Msg
main =
    program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
