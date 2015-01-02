-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE KindSignatures, OverloadedStrings, RankNTypes #-}

module Ksdl.Geocoding
   where

import Control.Applicative
import Control.Concurrent ( threadDelay )
import Control.Monad.Error
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.Curl
import Network.HTTP ( urlEncode )
import Text.Printf ( printf )


data LatLng = LatLng Double Double
   deriving Show

instance FromJSON LatLng where
   parseJSON (Object v) = do
      firstResult <- (v .: "results") >>= headE v
      loc <- (firstResult .: "geometry") >>= (.: "location")
      LatLng <$> (loc .: "lat") <*> (loc .: "lng")
   parseJSON o = failParse o


headE :: forall (m :: * -> *) a a1.
   (Show a1, Monad m) => a1 -> [a] -> m a
headE _ (x : _) = return x
headE v []      = failParse v


failParse :: forall (m :: * -> *) a a1.
   (Show a1, Monad m) => a1 -> m a
failParse o = fail $ printf "Geocoding results failure:\n%s" (show o)


addrToCoords :: String -> IO (Either String LatLng)
addrToCoords addr = runErrorT $ do
   let url = mkGeocodeUrl addr
   liftIO $ threadDelay 500000   -- Geocoding server is touchy
   gcJSON <- curlE =<< (liftIO $ curlGetString url [])
   either throwError return (eitherDecode . BL.pack $ gcJSON)

   where
      curlE (CurlOK, body) = return body
      curlE (code,   body) =
         throwError $ printf "%s\n%s" (show code) body


mkGeocodeUrl :: String -> String
mkGeocodeUrl addr = printf
   "http://maps.googleapis.com/maps/api/geocode/json?address=%s"
   (urlEncode addr)
