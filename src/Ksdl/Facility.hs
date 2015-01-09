-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric #-}

module Ksdl.Facility
   where

import Data.Aeson ( FromJSON, ToJSON, encode )
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text hiding ( init, map, unlines )
import Data.UUID ( toString )
import Data.UUID.V4 ( nextRandom )
import GHC.Generics ( Generic )
import System.FilePath
import Text.Printf ( printf )
import Text.Regex ( matchRegex, mkRegex )


data Facility = Facility
   { _id :: String
   , name :: Text
   , score :: Double
   , location :: Text
   , inspection_date :: [Int]
   }
   deriving Generic

instance Show Facility
   where show = displayFacility

instance FromJSON Facility
instance ToJSON Facility


parseDate :: String -> [Int]
parseDate dateStr =
   let mbParsed = (map read) `fmap` matchRegex re dateStr
       re = mkRegex "([0-9]{2})/([0-9]{2})/([0-9]{4})"
   in maybe [] (\(m : d : y : []) -> [y, m, d]) mbParsed


setId :: Facility -> IO Facility
setId f = do
   u <- nextRandom
   return $ f { _id = toString u }


saveFacility :: FilePath -> Facility -> IO ()
saveFacility dir fac = BL.writeFile (dir </> (_id fac)) $ encode fac


displayFacility :: Facility -> String
displayFacility fac = printf mask (_id fac) (unpack $ name fac)
   (inspection_date fac !! 0) (inspection_date fac !! 1)
   (inspection_date fac !! 2) (score fac)
   (unpack $ location fac)

   where
      mask = init . unlines $
         [ "Facility %s"
         , "   %s | %4d-%02d-%02d %f"
         , "   %s"
         ]
