module Mangas.Read exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Mangas.Common exposing (pageTitle)
import Models exposing (Link, Model, PageIndex, PageTitle)
import Msgs exposing (Msg)
import Navigation exposing (newUrl)
import RemoteData exposing (WebData)


view : Model -> Maybe PageTitle -> Html Msg
view model title =
    div []
        [ pageTitle title
        , mangaReader model
        ]


mangaReader : Model -> Html Msg
mangaReader model =
    case model.activeImage of
        RemoteData.Failure e ->
            p [] [ text "There was an error while loading the image." ]

        RemoteData.Success l ->
            div [ class "manga-reader" ]
                [ div [ class "button-list" ]
                    [ generatePreviousButton model
                    , generateNextButton model
                    ]
                , img [ src l.linkHref, alt l.linkHref ] []
                , div [ class "button-list" ]
                    [ generatePreviousButton model
                    , generateNextButton model
                    ]
                ]

        _ ->
            p [] [ text "Loading..." ]



-- generateOffsetButton : Int -> String -> Model -> Html Msg
-- generateOffsetButton offset buttonText model =
--     case model.activeManga of
--         Just m ->
--             case model.activeChapter of
--                 Just c ->
--                     a [ href (readPath m.mangaUrl c.mangaUrl (model.pageIndex + offset)) ] [ text buttonText ]
--                 Nothing ->
--                     text ""
--         Nothing ->
--             text ""


generateOffsetButton : Int -> String -> Model -> Html Msg
generateOffsetButton offset buttonText model =
    button [ onClick (Msgs.NextImage (model.pageIndex + offset)) ] [ text buttonText ]


generatePreviousButton : Model -> Html Msg
generatePreviousButton model =
    if model.pageIndex > 0 then
        generateOffsetButton -1 "Previous" model
    else
        text ""


generateNextButton : Model -> Html Msg
generateNextButton =
    generateOffsetButton 1 "Next"
