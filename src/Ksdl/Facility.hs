-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric #-}

module Ksdl.Facility
   where

import Data.Aeson ( FromJSON, ToJSON )
import GHC.Generics ( Generic )
import Text.Regex


data Facility = Facility
   { name :: String
   , score :: Double
   , location :: String
   , inspectionDate :: [Int]
   }
   deriving (Show, Generic)

instance FromJSON Facility
instance ToJSON Facility


parseDate :: String -> [Int]
parseDate dateStr =
   let mbParsed = (map read) `fmap` matchRegex re dateStr
       re = mkRegex "([0-9]{2})/([0-9]{2})/([0-9]{4})"
   in maybe [] (\(m : d : y : []) -> [y, m, d]) mbParsed
