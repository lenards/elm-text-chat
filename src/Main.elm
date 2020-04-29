module Main exposing (main)

import Browser
import State exposing (init, subscriptions, update)
import Types exposing (Flags, Model, Msg(..))
import View exposing (view)


{-| the entry point of the Elm "application"
-}
main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
