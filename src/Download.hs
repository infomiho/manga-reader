{-# LANGUAGE OverloadedStrings #-}

module Download where

import Network.Wreq
import Control.Lens
import Data.ByteString.Lazy.Char8 as Char8

downloadHTML :: String -> IO String
downloadHTML url = do
  r <- get url
  return $ Char8.unpack $ (r ^. responseBody)
