{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( startApp
    , app
    ) where

import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.TH
import Data.Maybe
import Network.Wai
import Network.Wai.Handler.Warp
import Servant hiding (Link)
import Control.Monad.IO.Class (liftIO)
import Data.List
import Servant.HTML.Blaze
import Data.ByteString.Lazy
import Network.Wai.Middleware.StaticEmbedded -- This package
import Data.FileEmbed -- file-embed

import Download
import Extract

$(deriveJSON defaultOptions ''Link)
$(deriveJSON defaultOptions ''MangaDetails)
$(deriveJSON defaultOptions ''MangaWithLinks)

type API =
  "links" :> Get '[JSON] (Maybe MangaWithLinks) :<|>
  "chapters" :> QueryParam "manga" String :> Get '[JSON] (Maybe MangaWithLinks) :<|>
  "pages" :> QueryParam "chapter" String :> Get '[JSON] (Maybe MangaWithLinks) :<|>
  "image" :> QueryParam "page" String :> Get '[JSON] (Maybe Link)

startApp :: IO ()
startApp = run 8080 (middleware $ app)

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =
  getLinks :<|>
  getChapters :<|>
  getPages :<|>
  getImage

downloadAndScrapeWith :: String -> (String -> a) -> Handler (a)
downloadAndScrapeWith url f = do
  html <- liftIO $ downloadHTML url
  return $ f html

getLinks :: Handler (Maybe MangaWithLinks)
getLinks = do
  scraped <- downloadAndScrapeWith "http://www.mangareader.net/alphabetical" scrapeAllLinks
  return $ Just scraped

getChapters :: Maybe String -> Handler (Maybe MangaWithLinks)
getChapters Nothing = return Nothing
getChapters (Just mangaUrl) = do
  scraped <- downloadAndScrapeWith ("http://www.mangareader.net" ++ mangaUrl) scrapeAllChapters
  return $ Just scraped

getPages :: Maybe String -> Handler (Maybe MangaWithLinks)
getPages Nothing = return Nothing
getPages (Just chapterUrl) = do
  scraped <- downloadAndScrapeWith ("http://www.mangareader.net" ++ chapterUrl) scrapeAllPages
  return $ Just scraped

getImage :: Maybe String -> Handler (Maybe Link)
getImage Nothing = return Nothing
getImage (Just pageUrl) = downloadAndScrapeWith ("http://www.mangareader.net" ++ pageUrl) scrapeImage

-- serveStatic :: Tagged Handler Application
-- serveStatic = serveDirectoryWebApp "./static"

middleware :: Application -> Application
middleware = static $(embedDir "./static")