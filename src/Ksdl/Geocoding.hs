-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts, KindSignatures, OverloadedStrings, RankNTypes #-}

module Ksdl.Geocoding
   ( GeoLatLng (..), forwardLookup )
   where

import Control.Applicative
import Control.Concurrent ( threadDelay )
import Control.Monad.Error
import Data.Aeson
import Data.Text
import Network.HTTP ( urlEncode )
import Network.HTTP.Conduit ( simpleHttp )
import Text.Printf ( printf )

import Ksdl.Facility
import Ksdl.Log


data GeoLatLng = GeoLatLng Double Double
   deriving Show

instance FromJSON GeoLatLng where
   parseJSON (Object v) = do
      firstResult <- (v .: "results") >>= headE v
      loc <- (firstResult .: "geometry") >>= (.: "location")
      GeoLatLng <$> (loc .: "lat") <*> (loc .: "lng")
   parseJSON o = fail $ show o


headE :: forall (m :: * -> *) a a1.
   (Show a1, Monad m) => a1 -> [a] -> m a
headE _ (x : _) = return x
headE v []      = fail $ show v


forwardLookup :: (MonadError String m, MonadIO m) =>
   Facility -> m GeoLatLng
forwardLookup fac = do
   let addr = location fac
   let url = mkGeocodeUrl addr

   -- Geocoding API limit: 2500/day, 5/sec
   liftIO $ threadDelay 500000

   gcJSON <- liftIO $ simpleHttp url

   let parseResult = eitherDecode gcJSON
   either (err fac url) return parseResult


err :: forall (m :: * -> *) a.
   (MonadError String m) => Facility -> String -> String -> m a
err fac url gcResultJSON =
   throwError $ printf "%s\nGeocoding error:\n%s\nGeocoding URL: %s\nGeocoding result JSON:\n%s" line (show fac) url gcResultJSON


mkGeocodeUrl :: Text -> String
mkGeocodeUrl addr = printf
   "http://maps.googleapis.com/maps/api/geocode/json?address=%s"
   (urlEncode $ unpack addr)
