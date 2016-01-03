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
import Data.Bson ( (=:), at )
import Data.Bson.Generic
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

instance FromBSON Place
instance ToBSON Place


data GeoPoint = GeoPoint
   { lat :: Double
   , lng :: Double
   }
   deriving (Eq, Generic, Show)


instance ToBSON GeoPoint where
   toBSON (GeoPoint lat' lng') =
      [ "type" =: ("Point" :: Text)
      , "coordinates" =: ([lng', lat'] :: [Double])
      ]

instance FromBSON GeoPoint where
   fromBSON bson = case ("coordinates" `at` bson) of
      (lng' : lat' : _) -> Just $ GeoPoint lat' lng'
      _                 -> Nothing


instance ToJSON GeoPoint where
   toJSON (GeoPoint lat' lng') = object
      [ "type" .= ("Point" :: Text)
      , "coordinates" .= [lng', lat']
      ]

instance FromJSON GeoPoint where
   parseJSON (Object o) = do
      (lng' : lat' : _) <- (o .: "coordinates")
      return $ GeoPoint lat' lng'
   parseJSON invalid    = typeMismatch "location" invalid
