module Main exposing (..)

import Commands exposing (fetchLinks)
import Html
import Models exposing (Model, initialModel)
import Msgs exposing (Msg)
import Navigation exposing (Location)
import Routing exposing (parseLocation)
import Update exposing (commandOnRouteChange, update)
import View exposing (view)


init : Location -> ( Model, Cmd Msg )
init location =
    let
        currentRoute =
            parseLocation location
    in
    ( initialModel currentRoute, commandOnRouteChange Nothing currentRoute )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Navigation.program Msgs.OnLocationChange
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
