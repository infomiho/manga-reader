module Update exposing (..)

import Array
import Commands exposing (fetchChapters, fetchImage, fetchLinks, fetchPages)
import Debug exposing (log)
import Maybe exposing (withDefault)
import Models exposing (Link, MangaWithLinks, Model, PageIndex, Route)
import Msgs exposing (Msg(..))
import Navigation exposing (newUrl)
import RemoteData exposing (WebData)
import Routing exposing (parseLocation, readPath)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetActiveManga manga ->
            ( { model | activeManga = manga }, Cmd.none )

        SetActiveChapter chapter ->
            ( { model | activeChapter = chapter }, Cmd.none )

        NextImage pageIndex ->
            ( { model | pageIndex = pageIndex }, fetchImageAtIndex model.pages pageIndex )

        SearchChange newContent ->
            ( { model | search = newContent }, Cmd.none )

        OnFetchLinks response ->
            ( { model
                -- In addition to setting the links we reset all other fields
                | links = response
                , chapters = RemoteData.NotAsked
                , pages = RemoteData.NotAsked
                , activeImage = RemoteData.NotAsked
                , activeManga = Nothing
              }
            , Cmd.none
            )

        OnFetchChapters response ->
            ( { model
                -- In addition to setting the chapters we reset all other fields
                | chapters = response
                , pages = RemoteData.NotAsked
                , activeImage = RemoteData.NotAsked
              }
            , Cmd.none
            )

        OnFetchPages response ->
            ( { model
                -- In addition to setting the chapters we reset all other fields
                | pages = response
                , activeImage = RemoteData.NotAsked
              }
            , fetchImageAtIndex response model.pageIndex
            )

        OnFetchImage response ->
            ( { model
                -- In addition to setting the chapters we reset all other fields
                | activeImage = response
              }
            , setNewUrl model
            )

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
            ( modelOnRouteChange model newRoute, commandOnRouteChange (Just model) newRoute )


modelOnRouteChange : Model -> Route -> Model
modelOnRouteChange model newRoute =
    let
        modelBase =
            { model | route = newRoute, pageIndex = setNewPageIndex newRoute, search = "" }

        modelActiveChapter =
            case ( model.route, newRoute ) of
                -- When we are coming from X to reading the manga
                -- don't remove the activeChapter from model
                ( _, Models.PagesRoute _ _ _ ) ->
                    modelBase

                _ ->
                    { modelBase | activeChapter = Nothing }
    in
    modelActiveChapter


commandOnRouteChange : Maybe Model -> Route -> Cmd Msg
commandOnRouteChange model newRoute =
    case log "new route" newRoute of
        Models.LinksRoute ->
            fetchLinks

        Models.ChaptersRoute mangaUrl ->
            fetchChapters mangaUrl

        Models.PagesRoute _ chapterUrl _ ->
            case model of
                Just m ->
                    -- m.route is the old route
                    case m.route of
                        Models.PagesRoute _ _ pageIndex ->
                            Cmd.none

                        _ ->
                            fetchPages chapterUrl

                Nothing ->
                    fetchPages chapterUrl

        _ ->
            Cmd.none


fetchImageAtIndex : WebData MangaWithLinks -> PageIndex -> Cmd Msg
fetchImageAtIndex pages pageIndex =
    case pages of
        RemoteData.Success { links, manga } ->
            let
                firstPage =
                    links
                        |> withDefault []
                        |> Array.fromList
                        |> Array.get pageIndex
            in
            case firstPage of
                Just l ->
                    fetchImage l.linkHref

                Nothing ->
                    Cmd.none

        _ ->
            Cmd.none


setNewPageIndex : Route -> PageIndex
setNewPageIndex route =
    case route of
        Models.PagesRoute _ _ pageIndex ->
            pageIndex

        _ ->
            0


setNewUrl : Model -> Cmd Msg
setNewUrl model =
    case ( model.activeManga, model.activeChapter, model.route ) of
        -- Only apply the route change if we are still at the pages route
        ( Just am, Just ac, Models.PagesRoute _ _ _ ) ->
            log "new url" newUrl (readPath am.mangaUrl ac.mangaUrl model.pageIndex)

        _ ->
            Cmd.none
