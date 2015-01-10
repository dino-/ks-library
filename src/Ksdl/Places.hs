-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts, KindSignatures, OverloadedStrings, RankNTypes #-}

module Ksdl.Places
   ( Location (..), coordsToPlaces )
   where

import Control.Applicative
import Control.Monad.Error
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.List as L
import Data.Text
import Network.HTTP.Conduit ( simpleHttp )
import Text.Printf ( printf )

import Ksdl.Geocoding ( GeoLatLng (..) )
import Ksdl.Log


-- This is in meters
placesRadius :: Int
placesRadius = 50
--placesRadius = 200

placesTypes :: String
placesTypes = L.intercalate "|"
   [ "restaurant"
   , "food"
   --, "cafe"
   --, "bar"
   ]


data PlLatLng = PlLatLng Double Double
   deriving Show


data Location = Location
   { locName :: Text
   , locVicinity :: Text
   , locLoc :: PlLatLng
   , locPlace_id :: Text
   }
   deriving Show

instance FromJSON Location where
   parseJSON (Object o) = do
      l <- (o .: "geometry") >>= (.: "location")
      Location
         <$> o .: "name"
         <*> o .: "vicinity"
         <*> (PlLatLng <$> (l .: "lat") <*> (l .: "lng"))
         <*> o .: "place_id"
   parseJSON o = failParse o


newtype Locations = Locations [Location]
   deriving Show

instance FromJSON Locations where
   parseJSON (Object v) = do
      rs <- v .: "results"
      when (L.null rs) $ failParse v
      return $ Locations rs
   parseJSON o = failParse o


failParse :: forall (m :: * -> *) a a1.
   (Show a1, Monad m) => a1 -> m a
failParse o = fail $ printf "Places results failure:\n%s" (show o)


coordsToPlaces :: (MonadError String m, MonadIO m) =>
   String -> GeoLatLng -> m [Location]
coordsToPlaces apiKey coords = do
   let url = mkPlacesUrl apiKey coords
   liftIO $ debugM lerror url

   plJSON <- liftIO $ simpleHttp url
   liftIO $ debugM lerror $ BL8.unpack plJSON

   let parseResult = eitherDecode plJSON
   liftIO $ either (infoM lerror)
      (\(Locations ls) -> mapM_ (infoM lerror . show) ls)
      parseResult
   either throwError (\(Locations ls) -> return ls) parseResult


mkPlacesUrl :: String -> GeoLatLng -> String
mkPlacesUrl apiKey (GeoLatLng lat lng) = printf
   --"https://maps.googleapis.com/maps/api/place/nearbysearch/json?key=%s&location=%f,%f&rankby=distance&types=%s" apiKey lat lng placesTypes
   "https://maps.googleapis.com/maps/api/place/nearbysearch/json?key=%s&location=%f,%f&radius=%d&types=%s" apiKey lat lng placesRadius placesTypes
