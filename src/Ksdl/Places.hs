-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE KindSignatures, OverloadedStrings, RankNTypes #-}

module Ksdl.Places
   ( Locations (..), Location (..), coordsToPlace )
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
   parseJSON (Object v) = Locations <$> v .: "results"
   parseJSON o = failParse o


failParse :: forall (m :: * -> *) a a1.
   (Show a1, Monad m) => a1 -> m a
failParse o = fail $ printf "Places results failure:\n%s" (show o)


coordsToPlace :: String -> GeoLatLng -> IO (Either String Locations)
coordsToPlace apiKey coords = runErrorT $ do
   let url = mkPlacesUrl apiKey coords
   liftIO $ debugM lerror url
   plJSON <- liftIO $ simpleHttp url
   liftIO $ debugM lerror $ BL8.unpack plJSON
   either throwError return (eitherDecode plJSON)


mkPlacesUrl :: String -> GeoLatLng -> String
mkPlacesUrl apiKey (GeoLatLng lat lng) = printf
   "https://maps.googleapis.com/maps/api/place/nearbysearch/json?key=%s&location=%f,%f&radius=%d&types=%s" apiKey lat lng placesRadius placesTypes
