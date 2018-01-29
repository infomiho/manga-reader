module Commands exposing (..)

import Base64 as B64
import Debug exposing (log)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, optional, required)
import Models exposing (ChapterUrl, Link, MangaDetails, MangaUrl, MangaWithLinks, PageIndex, PageUrl)
import Msgs exposing (Msg)
import RemoteData


fetchLinks : Cmd Msg
fetchLinks =
    Http.get fetchLinksUrl mangaWithLinksDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.OnFetchLinks


fetchLinksUrl : String
fetchLinksUrl =
    "http://localhost:8080/links"


mangaWithLinksDecoder : Decode.Decoder MangaWithLinks
mangaWithLinksDecoder =
    decode MangaWithLinks
        |> required "links" (Decode.nullable linksDecoder)
        |> required "manga" decodeMangaDetails


decodeMangaDetails : Decode.Decoder MangaDetails
decodeMangaDetails =
    decode MangaDetails
        |> required "title" (Decode.nullable Decode.string)


linksDecoder : Decode.Decoder (List Link)
linksDecoder =
    Decode.list linkDecoder


linkDecoder : Decode.Decoder Link
linkDecoder =
    decode Link
        |> required "linkHref" Decode.string
        |> required "linkText" (Decode.nullable Decode.string)


fetchChapters : MangaUrl -> Cmd Msg
fetchChapters mangaUrl =
    case B64.decode mangaUrl of
        Ok res ->
            Http.get (fetchChaptersUrl res) mangaWithLinksDecoder
                |> RemoteData.sendRequest
                |> Cmd.map Msgs.OnFetchChapters

        Err _ ->
            Cmd.none


fetchChaptersUrl : MangaUrl -> String
fetchChaptersUrl mangaUrl =
    "http://localhost:8080/chapters?manga=" ++ mangaUrl


fetchPages : ChapterUrl -> Cmd Msg
fetchPages chapterUrl =
    case B64.decode chapterUrl of
        Ok res ->
            Http.get (fetchPagesUrl res) mangaWithLinksDecoder
                |> RemoteData.sendRequest
                |> Cmd.map Msgs.OnFetchPages

        Err _ ->
            Cmd.none


fetchPagesUrl : ChapterUrl -> String
fetchPagesUrl chapterUrl =
    "http://localhost:8080/pages?chapter=" ++ chapterUrl


fetchImage : PageUrl -> Cmd Msg
fetchImage pageUrl =
    case B64.decode pageUrl of
        Ok res ->
            Http.get (fetchImageUrl res) linkDecoder
                |> RemoteData.sendRequest
                |> Cmd.map Msgs.OnFetchImage

        Err _ ->
            Cmd.none


fetchImageUrl : PageUrl -> String
fetchImageUrl pageUrl =
    "http://localhost:8080/image?page=" ++ pageUrl
