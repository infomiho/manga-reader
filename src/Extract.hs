{-# LANGUAGE OverloadedStrings #-}

module Extract 
  ( scrapeAllLinks
  , scrapeAllChapters
  , scrapeAllPages
  , scrapeImage
  , Link
  , MangaDetails
  , MangaWithLinks
  ) where 
  

import Text.HTML.Scalpel.Core
import Data.ByteString.Base64 (encode)
import Data.ByteString.Char8 as Char8 hiding (concat)

data Link = Link { linkHref :: String, linkText :: Maybe String } deriving (Show)

data MangaDetails = MangaDetails { title :: Maybe String }

data MangaWithLinks = MangaWithLinks {
  manga :: MangaDetails,
  links :: Maybe [Link]
}

scrapeAllLinks :: String -> MangaWithLinks
scrapeAllLinks html =  MangaWithLinks { links = links, manga = MangaDetails { title = Nothing } }
  where links = fmap concat $ scrapeStringLike html allLinks

allLinks :: Scraper String [[Link]]
allLinks = chroots ("ul" @: [hasClass "series_alpha"]) lines
  where
    lines :: Scraper String [Link]
    lines = do
      lis <- chroots ("li") link
      return lis
    link :: Scraper String Link
    link = do
      h <- (attr "href" $ "a")
      t <- (text $ "a")
      return Link { linkHref = (encodeLink h), linkText = (Just t) }

scrapeAllChapters :: String -> MangaWithLinks
scrapeAllChapters html = MangaWithLinks { links = links, manga = MangaDetails { title = Nothing } }
   where links = scrapeStringLike html allChapters

allChapters :: Scraper String [Link]
allChapters = chroot ("table" @: ["id" @= "listing"]) rows
  where 
    rows :: Scraper String [Link]
    rows = do
      trs <- chroots ("tr") link
      return trs
    link :: Scraper String Link
    link = do
      h <- (attr "href" $ "a")
      t <- (text $ "a")
      return Link { linkHref = (encodeLink h), linkText = (Just t) } 

scrapeAllPages :: String -> MangaWithLinks
scrapeAllPages html = MangaWithLinks { links = links, manga = MangaDetails { title = Nothing } }
  where links = scrapeStringLike html allPages

allPages :: Scraper String [Link]
allPages = chroot ("select" @: ["id" @= "pageMenu"]) options
  where 
    options :: Scraper String   [Link]
    options = do
      opts <- chroots ("option") option
      return opts
    option :: Scraper String Link
    option = do
      h <- (attr "value" $ "option")
      return Link { linkHref = (encodeLink h), linkText = Nothing } 

scrapeImage :: String -> Maybe Link
scrapeImage html = scrapeStringLike html image

image :: Scraper String Link
image = do
  h <- (attr "src" $ ("img" @: ["id" @= "img"]))
  return Link { linkHref = h, linkText = Nothing }
  
scrapeMangaDetails :: String -> Maybe MangaDetails
scrapeMangaDetails html = scrapeStringLike html mangaDetails

mangaDetails :: Scraper String MangaDetails
mangaDetails = do
  return MangaDetails { title = Just "" }

encodeLink :: String -> String
encodeLink = Char8.unpack . encode .  Char8.pack