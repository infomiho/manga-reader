module Mangas.Common exposing (..)

import Html exposing (..)
import Models exposing (PageTitle)
import Msgs exposing (Msg)


pageTitle : Maybe PageTitle -> Html Msg
pageTitle title =
    case title of
        Just t ->
            h1 [] [ text t ]

        Nothing ->
            -- `text ""` is an empty node - no rendering
            text ""
