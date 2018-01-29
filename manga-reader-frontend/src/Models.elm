module Models exposing (..)

import RemoteData exposing (WebData)


type alias Model =
    { links : WebData MangaWithLinks
    , chapters : WebData MangaWithLinks
    , pages : WebData MangaWithLinks
    , pageIndex : PageIndex
    , activeManga : Maybe Manga
    , activeChapter : Maybe Manga
    , activeImage : WebData Link
    , route : Route
    , search : String
    }


type alias PageTitle =
    String


type alias MangaDetails =
    { title : Maybe String
    }


type alias MangaWithLinks =
    { links : Maybe (List Link)
    , manga : MangaDetails
    }


type alias Link =
    { linkHref : String
    , linkText : Maybe String
    }


type alias Manga =
    { mangaUrl : String
    , mangaTitle : Maybe String
    }


initialModel : Route -> Model
initialModel route =
    let
        pageIndex =
            case route of
                PagesRoute _ _ pageIndex ->
                    pageIndex

                _ ->
                    0

        activeManga =
            case route of
                PagesRoute mangaUrl _ _ ->
                    Just (Manga mangaUrl Nothing)

                _ ->
                    Nothing

        activeChapter =
            case route of
                PagesRoute _ chapterUrl _ ->
                    Just (Manga chapterUrl Nothing)

                _ ->
                    Nothing
    in
    { links = RemoteData.Loading
    , chapters = RemoteData.NotAsked
    , pages = RemoteData.NotAsked
    , pageIndex = pageIndex
    , activeManga = activeManga
    , activeChapter = activeChapter
    , activeImage = RemoteData.NotAsked
    , route = route
    , search = ""
    }


type alias MangaUrl =
    String


type alias ChapterUrl =
    String


type alias PageIndex =
    Int


type alias PageUrl =
    String


type Route
    = LinksRoute
    | ChaptersRoute MangaUrl
    | PagesRoute MangaUrl ChapterUrl PageIndex
    | NotFoundRoute


type LinkType
    = GoToChapters
    | GoToPages MangaUrl
