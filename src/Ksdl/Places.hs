-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts, KindSignatures, OverloadedStrings, RankNTypes #-}

module Ksdl.Places
   ( Location (..), coordsToPlaces )
   where

import Control.Applicative
import Control.Monad.Error
import Data.Aeson
import qualified Data.List as L
import Data.Text
import Network.HTTP ( urlEncode )
import Network.HTTP.Conduit ( simpleHttp )
import Text.Printf ( printf )

import Ksdl.Geocoding ( GeoLatLng (..) )
import Ksdl.Log
import Ksdl.NameWords ( toList )


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
failParse o = fail $ show o


coordsToPlaces :: (MonadError String m, MonadIO m) =>
   String -> Text -> GeoLatLng -> m [Location]
coordsToPlaces apiKey name coords = do
   let nameWords = toList name

   let url = mkPlacesUrl apiKey nameWords coords

   plJSON <- liftIO $ simpleHttp url

   let parseResult = eitherDecode plJSON
   either (err name nameWords url) (\(Locations ls) -> return ls) parseResult


err :: forall (m :: * -> *) a.
   (MonadError String m) => Text -> [Text] -> String -> String -> m a
err name nameWords url placesResultJSON =
   throwError $ printf "%s\nPlaces results failure: %s\nname words list: %s\nPlaces URL: %s\nPlaces result JSON:\n%s" line (unpack name) (show nameWords) url placesResultJSON


mkPlacesUrl :: String -> [Text] -> GeoLatLng -> String
mkPlacesUrl apiKey nameWords (GeoLatLng lat lng) = printf
   "https://maps.googleapis.com/maps/api/place/nearbysearch/json?key=%s&location=%f,%f&rankby=distance&name=%s&types=%s" apiKey lat lng nameList placesTypes
   where
      nameList = urlEncode $ unpack $ intercalate " " $ nameWords
