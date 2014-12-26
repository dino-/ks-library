-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Ksdl.Facility
   where


data Facility = Facility
   { name :: String
   , score :: Double
   , location :: String
   , inspectionDate :: String
   }
   deriving Show
