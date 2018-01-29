module Msgs exposing (..)

import Models exposing (Link, Manga, MangaWithLinks, PageIndex)
import Navigation exposing (Location)
import RemoteData exposing (WebData)


type Msg
    = OnFetchLinks (WebData MangaWithLinks)
    | OnFetchChapters (WebData MangaWithLinks)
    | OnFetchPages (WebData MangaWithLinks)
    | OnFetchImage (WebData Link)
    | SetActiveManga (Maybe Manga)
    | SetActiveChapter (Maybe Manga)
    | NextImage PageIndex
    | SearchChange String
    | OnLocationChange Location
