module Mangas.List exposing (..)

import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Mangas.Common exposing (pageTitle)
import Maybe
import Models exposing (Link, LinkType, Manga, MangaWithLinks, PageTitle)
import Msgs exposing (Msg(..))
import RemoteData exposing (WebData)
import Routing exposing (chaptersPath, readPath)


view : WebData MangaWithLinks -> Maybe PageTitle -> LinkType -> String -> Html Msg
view mangaWithLinks title linkType search =
    div []
        [ pageTitle title
        , searchField search
        , list mangaWithLinks linkType search
        ]


list : WebData MangaWithLinks -> LinkType -> String -> Html Msg
list mangaWithLinks linkType search =
    case mangaWithLinks of
        RemoteData.Success l ->
            div []
                [ case l.links of
                    Just links ->
                        let
                            filtered =
                                links
                                    |> List.filter (\l -> String.isEmpty search || String.contains search (l.linkText |> Maybe.withDefault "" |> String.toLower))
                        in
                        if List.isEmpty filtered then
                            p [] [ text "There are no links for your search" ]
                        else
                            table []
                                [ thead []
                                    [ tr []
                                        [ th [] [ text "Link title" ]
                                        , th [] []
                                        ]
                                    ]
                                , tbody []
                                    (filtered
                                        |> List.map (linkRow linkType)
                                    )
                                ]

                    Nothing ->
                        p [] [ text "There are currently no links." ]
                ]

        RemoteData.Failure e ->
            p [] [ text "There was an error loading the links." ]

        _ ->
            p [] [ text "Loading links..." ]


linkRow : LinkType -> Link -> Html Msg
linkRow linkType link =
    tr []
        [ td [] [ text (Maybe.withDefault "" link.linkText) ]
        , td []
            [ navigationLink linkType link
            ]
        ]


navigationLink : LinkType -> Link -> Html Msg
navigationLink linkType link =
    case linkType of
        Models.GoToChapters ->
            a [ href (chaptersPath link.linkHref), onClick (SetActiveManga (Just (Manga link.linkHref link.linkText))) ] [ text "Read manga" ]

        Models.GoToPages mangaUrl ->
            a [ href (readPath mangaUrl link.linkHref 0), onClick (SetActiveChapter (Just (Manga link.linkHref link.linkText))) ] [ text "Read chapter" ]


searchField : String -> Html Msg
searchField search =
    div [ class "search" ]
        [ input [ type_ "text", placeholder "Search through the list", onInput SearchChange, value search ] []
        ]
