module View exposing (..)

import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Mangas.List
import Mangas.Read
import Maybe exposing (withDefault)
import Models exposing (Manga, Model, Route(..))
import Msgs exposing (Msg(..))
import Routing exposing (chaptersPath, linksPath)


view : Model -> Html Msg
view model =
    section []
        [ header []
            [ div [ class "logo" ] [ text "MANGA READER" ]
            , nav model
            ]
        , article []
            [ page model
            ]
        ]


nav : Model -> Html Msg
nav model =
    header []
        [ a [ href linksPath ] [ text "All mangas" ]
        , case ( model.activeManga, model.activeChapter ) of
            ( Just m, Just _ ) ->
                case m.mangaTitle of
                    Just t ->
                        a [ href (chaptersPath m.mangaUrl) ] [ text ("Go back to " ++ t) ]

                    Nothing ->
                        text ""

            _ ->
                text ""
        ]


page : Model -> Html Msg
page model =
    case model.route of
        LinksRoute ->
            Mangas.List.view model.links (Just "List of mangas") Models.GoToChapters model.search

        ChaptersRoute mangaUrl ->
            Mangas.List.view model.chapters (Just (getMangaTitle model.activeManga)) (Models.GoToPages mangaUrl) model.search

        PagesRoute mangaUrl chapterUrl pageIndex ->
            Mangas.Read.view model (Just (getMangaTitle model.activeChapter))

        NotFoundRoute ->
            div [] [ h1 [] [ text "Not found" ] ]


getMangaTitle : Maybe Manga -> String
getMangaTitle manga =
    case manga of
        Just m ->
            case m.mangaTitle of
                Just t ->
                    t

                Nothing ->
                    "Unknown manga"

        Nothing ->
            "Unknown manga"
