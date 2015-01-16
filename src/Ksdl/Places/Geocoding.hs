-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE KindSignatures, OverloadedStrings, RankNTypes #-}

module Ksdl.Places.Geocoding
   ( GeoLatLng (..), forwardLookup )
   where

import Control.Applicative
import Control.Concurrent ( threadDelay )
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text
import Network.HTTP ( urlEncode )
import Network.HTTP.Conduit ( simpleHttp )
import Text.Printf ( printf )

import Ksdl
import Ksdl.Config
import Ksdl.Inspection
import Ksdl.Log


data GeoLatLng = GeoLatLng Double Double
   deriving Show

instance FromJSON GeoLatLng where
   parseJSON (Object v) = do
      status <- v .: "status"
      when (status /= "OK") $ fail status

      firstResult <- (v .: "results") >>= headE v
      loc <- (firstResult .: "geometry") >>= (.: "location")

      GeoLatLng <$> (loc .: "lat") <*> (loc .: "lng")
   parseJSON o = fail . show $ o


headE :: forall (m :: * -> *) a a1.
   (Show a1, Monad m) => a1 -> [a] -> m a
headE _ (x : _) = return x
headE v []      = fail . show $ v


forwardLookup :: Inspection -> Ksdl GeoLatLng
forwardLookup insp = do
   let url = mkGeocodeUrl . addr $ insp
   liftIO $ noticeM lname $ "Geocoding URL: " ++ url

   asks geocodingApiDelay >>= (liftIO . threadDelay)

   gcJSON <- liftIO $ simpleHttp url
   liftIO $ debugM lname $ "Geocoding result JSON: "
      ++ (BL.unpack gcJSON)

   let parseResult = eitherDecode gcJSON
   either
      (\status -> throwError $ "ERROR Geocoding: " ++ status)
      displayAndReturn parseResult


displayAndReturn :: GeoLatLng -> Ksdl GeoLatLng
displayAndReturn location = do
   liftIO $ noticeM lname $ show location
   return location


mkGeocodeUrl :: Text -> String
mkGeocodeUrl addr' = printf
   "https://maps.googleapis.com/maps/api/geocode/json?address=%s"
   (urlEncode $ unpack addr')
