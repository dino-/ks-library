-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module Ksdl.Geocoding
   where

import Control.Applicative
import Control.Concurrent ( threadDelay )
import Control.Monad ( mzero )
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.Curl
import Network.HTTP ( urlEncode )
import Text.Printf ( printf )


data LatLng = LatLng Double Double
   deriving Show

instance FromJSON LatLng where
   parseJSON (Object v) = do
      loc <- head `fmap` (v .: "results") >>= (.: "geometry")
         >>= (.: "location")
      LatLng <$> (loc .: "lat") <*> (loc .: "lng")
   parseJSON _ = mzero


addrToCoords :: String -> IO (Maybe LatLng)
addrToCoords addr = do
   let url = mkGeocodeUrl addr
   threadDelay 500000   -- Geocoding server is touchy
   gcJSON <- snd `fmap` curlGetString url []
   return . decode . BL.pack $ gcJSON


mkGeocodeUrl :: String -> String
mkGeocodeUrl addr = printf
   "http://maps.googleapis.com/maps/api/geocode/json?address=%s"
   (urlEncode addr)
