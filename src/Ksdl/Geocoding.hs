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
import qualified Data.ByteString.Lazy.Char8 as BL8
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
   liftIO $ debugM lerror url

   -- Geocoding API limit: 2500/day, 5/sec
   liftIO $ threadDelay 500000

   gcJSON <- liftIO $ simpleHttp url
   liftIO $ debugM lerror $ BL8.unpack gcJSON

   let parseResult = eitherDecode gcJSON
   liftIO $ either (infoM lerror) (infoM lerror . show) parseResult
   either (throwParseFailure fac) return parseResult


throwParseFailure :: forall t (m :: * -> *) a.
   (MonadError String m) => Facility -> t -> m a
throwParseFailure fac _ = throwError $
   "Geocoding problem parsing address:" ++ (show fac)


mkGeocodeUrl :: Text -> String
mkGeocodeUrl addr = printf
   "http://maps.googleapis.com/maps/api/geocode/json?address=%s"
   (urlEncode $ unpack addr)
