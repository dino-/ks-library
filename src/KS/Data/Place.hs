-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module KS.Data.Place
   ( Place (..)
   , GeoPoint (..)
   )
   where

import Data.Aeson
import Data.Aeson.Types ( typeMismatch )
import Data.Text
import GHC.Generics ( Generic )


data Place = Place
   { name :: Text
   , vicinity :: Text
   , location :: GeoPoint
   , types :: [String]
   , place_id :: Text
   }
   deriving (Eq, Generic, Show)

instance FromJSON Place
instance ToJSON Place


data GeoPoint = GeoPoint
   { lat :: Double
   , lng :: Double
   }
   deriving (Eq, Show)

instance ToJSON GeoPoint where
   toJSON (GeoPoint lat' lng') = object
      [ "type" .= ("Point" :: Text)
      , "coordinates" .= [lng', lat']
      ]

instance FromJSON GeoPoint where

   parseJSON (Object o) = do
      (lng' : lat' : _) <- (o .: "coordinates")
      return $ GeoPoint lat' lng'

   parseJSON invalid = typeMismatch "location" invalid
