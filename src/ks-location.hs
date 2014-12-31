-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad ( mzero )
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List ( isPrefixOf )
import Data.Maybe ( catMaybes, fromJust )
import Data.String.Utils ( strip )
--import Debug.Trace ( trace )
import Network.Curl
import Network.HTTP ( urlEncode )
import System.Directory ( getDirectoryContents )
import System.Environment ( getArgs, lookupEnv )
import System.FilePath
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )
import Text.Printf ( printf )

import Ksdl.Facility


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   dir <- head `fmap` getArgs

   {-
   print =<< (loadFacility
      $ dir </> "ef23dfb3-e9d1-431c-af49-3cb98ee6d587")
   -}

   -- Paths to all files
   files <-
      ( map (dir </>)                     -- ..relative paths
      . filter (not . isPrefixOf ".") )   -- ..minus dotfiles
      `fmap` getDirectoryContents dir     -- All files
   --print $ take 5 files

   -- Loaded Facilities
   facs <- catMaybes `fmap` mapM loadFacility files
   --print facs

   let gcUrls = map mkGeocodeUrl facs
   --print gcUrls

   -- Retrieve the GeoCode results for these addresses
   gcJSONs <- map snd `fmap` mapM (flip curlGetString []) gcUrls
   --mapM_ putStrLn gcJSONs

   let locs = catMaybes $ map (decode . BL.pack) gcJSONs
   --print locs

   placesApiKey <-
      strip `fmap`         -- ..strip any trailing whitespace
      (readFile =<<        -- ..read the contents of this file
      ((</> ".gplaces") .  -- ..append the Places API key filename
      -- FIXME fromJust is bad
      fromJust) `fmap`     -- ..extracted from the Maybe
      lookupEnv "HOME")    -- Maybe $HOME directory
   --print placesApiKey

   let plUrls = map (mkPlacesUrl placesApiKey) locs
   --print plUrls

   plJSONs <- map snd `fmap` mapM (flip curlGetString []) plUrls
   --mapM_ putStrLn plJSONs

   let plLocs = map (decode . BL.pack) plJSONs :: [Maybe Locations]
   print plLocs

   putStrLn "done"


data Location = Location
   { locName :: String
   , locVicinity :: String
   , locLoc :: LatLng
   , locPlace_id :: String
   }
   deriving Show

newtype Locations = Locations [Location]
   deriving Show

instance FromJSON Locations where
   parseJSON (Object v) =
      Locations <$> catMaybes `fmap` (v .: "results")
   parseJSON _ = mzero

instance FromJSON Location where
   parseJSON (Object o) = do
      l <- (o .: "geometry") >>= (.: "location")
      Location
         <$> o .: "name"
         <*> o .: "vicinity"
         <*> (LatLng <$> (l .: "lat") <*> (l .: "lng"))
         <*> o .: "place_id"
   parseJSON _ = mzero


mkPlacesUrl :: String -> LatLng -> String
mkPlacesUrl apiKey (LatLng lat lng) = printf
   "https://maps.googleapis.com/maps/api/place/nearbysearch/json?key=%s&location=%f,%f&radius=50&types=restaurant" apiKey lat lng


data LatLng = LatLng Double Double
   deriving Show

instance FromJSON LatLng where
   parseJSON (Object v) = do
      loc <- head `fmap` (v .: "results") >>= (.: "geometry")
         >>= (.: "location")
      LatLng <$> (loc .: "lat") <*> (loc .: "lng")
   parseJSON _ = mzero


mkGeocodeUrl :: Facility -> String
mkGeocodeUrl fac = printf
   "http://maps.googleapis.com/maps/api/geocode/json?address=%s"
   (urlEncode . location $ fac)


loadFacility :: FilePath -> IO (Maybe Facility)
loadFacility path = decode `fmap` BL.readFile path
