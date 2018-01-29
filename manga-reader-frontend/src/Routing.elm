module Routing exposing (..)

import Models exposing (ChapterUrl, MangaUrl, PageIndex, Route(..))
import Navigation exposing (Location)
import UrlParser exposing (..)


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map LinksRoute top
        , map ChaptersRoute (s "chapters" </> string)
        , map PagesRoute (s "read" </> string </> string </> int)
        ]


parseLocation : Location -> Route
parseLocation location =
    case parseHash matchers location of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


linksPath : String
linksPath =
    "#/"


chaptersPath : MangaUrl -> String
chaptersPath mangaUrl =
    "#/chapters/" ++ mangaUrl


readPath : MangaUrl -> ChapterUrl -> PageIndex -> String
readPath mangaUrl chaptersUrl pageIndex =
    "#/read/" ++ mangaUrl ++ "/" ++ chaptersUrl ++ "/" ++ toString pageIndex
