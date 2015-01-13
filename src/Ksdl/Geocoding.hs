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
import qualified Data.ByteString.Lazy.Char8 as BL
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
   liftIO $ noticeM lerror $ "Geocoding URL: " ++ url

   -- Geocoding API limit: 2500/day, 5/sec
   liftIO $ threadDelay 500000

   gcJSON <- liftIO $ simpleHttp url
   liftIO $ debugM lerror $ "Geocoding result JSON: "
      ++ (BL.unpack gcJSON)

   let parseResult = eitherDecode gcJSON
   liftIO $ either (noticeM lerror) (noticeM lerror . show)
      parseResult
   either (const $ throwError $ "ERROR Geocoding")
      return parseResult


mkGeocodeUrl :: Text -> String
mkGeocodeUrl addr = printf
   "http://maps.googleapis.com/maps/api/geocode/json?address=%s"
   (urlEncode $ unpack addr)
