-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE KindSignatures, OverloadedStrings, RankNTypes #-}

module Ksdl.Geocoding
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
import Ksdl.Facility
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
   parseJSON o = fail $ show o


headE :: forall (m :: * -> *) a a1.
   (Show a1, Monad m) => a1 -> [a] -> m a
headE _ (x : _) = return x
headE v []      = fail $ show v


forwardLookup :: Facility -> Ksdl GeoLatLng
forwardLookup fac = do
   let addr = location fac
   let url = mkGeocodeUrl addr
   liftIO $ noticeM lname $ "Geocoding URL: " ++ url

   -- Geocoding API limit: 2500/day, 5/sec
   liftIO $ threadDelay 500000

   gcJSON <- liftIO $ simpleHttp url
   liftIO $ debugM lname $ "Geocoding result JSON: "
      ++ (BL.unpack gcJSON)

   let parseResult = eitherDecode gcJSON
   either
      (\status -> throwError $ "ERROR Geocoding: " ++ status)
      displayAndReturn parseResult


displayAndReturn :: GeoLatLng -> Ksdl GeoLatLng
displayAndReturn gll = do
   liftIO $ noticeM lname $ show gll
   return gll


mkGeocodeUrl :: Text -> String
mkGeocodeUrl addr = printf
   "http://maps.googleapis.com/maps/api/geocode/json?address=%s"
   (urlEncode $ unpack addr)
