-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad ( mzero )
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char ( toLower )
import Data.List ( intercalate, isPrefixOf )
import Data.Maybe ( catMaybes, fromJust )
import Data.String.Utils ( strip )
--import Debug.Trace ( trace )
import Network.Curl
import System.Directory ( getDirectoryContents )
import System.Environment ( getArgs, lookupEnv )
import System.FilePath
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )
import Text.EditDistance
import Text.Printf ( printf )

import Ksdl.Facility
import Ksdl.Geocoding ( LatLng (..), addrToCoords )


placesTypes :: String
placesTypes = intercalate "|"
   [ "restaurant"
   , "food"
   --, "cafe"
   --, "bar"
   ]


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

   -- FIXME Need to use real error handling and knock it off with this evil catMaybes business
   locs <- catMaybes `fmap` (mapM addrToCoords $ map location facs)
   --mapM_ print locs

   placesApiKey <-
      strip `fmap`         -- ..strip any trailing whitespace
      (readFile =<<        -- ..read the contents of this file
      ((</> ".gplaces") .  -- ..append the Places API key filename
      -- FIXME fromJust is bad
      fromJust) `fmap`     -- ..extracted from the Maybe
      lookupEnv "HOME")    -- Maybe $HOME directory
   --print placesApiKey

   let plUrls = map (mkPlacesUrl placesApiKey) locs
   --mapM_ putStrLn plUrls

   plJSONs <- map snd `fmap` mapM (flip curlGetString []) plUrls
   --mapM_ putStrLn plJSONs

   let plLocs = map (decode . BL.pack) plJSONs :: [Maybe Locations]
   --print plLocs

   let ts = zip facs plLocs
   --mapM_ log ts
   putStrLn "\"ndist\",\"vdist\",\"iname\",\"pname\",\"ivic\",\"pvic\",\"_id\""
   mapM_ csv ts

   --putStrLn "done"


csv :: (Facility, Maybe Locations) -> IO ()
csv (fac, Just (Locations locs)) = mapM_ (line fac) locs
   where
      line :: Facility -> Location -> IO ()
      line fac' loc = printf
            "%d,%d,\"%s\",\"%s\",\"%s\",\"%s\",\"%s\"\n"
            (dist (name fac') (locName loc))
            (dist (location fac') (locVicinity loc))
            (name fac')
            (locName loc)
            (location fac')
            (locVicinity loc)
            (_id fac')

      dist target input = levenshteinDistance defaultEditCosts
         (map toLower target) (map toLower input)

csv (fac, Nothing) = line fac
   where
      line :: Facility -> IO ()
      line fac' = printf
            ",,\"%s\",\"\",\"%s\",\"\",\"%s\"\n"
            (name fac')
            (location fac')
            (_id fac')


log :: (Facility, Maybe Locations) -> IO ()
log (fac, mlocs) = do
   putStr "\n"
   printf "inspection location: %s\n" (name fac)
   printf "                     %s\n" (location fac)
   putStr "places matches:\n"
   maybe (putStr "   NONE FOUND\n") (display' fac) mlocs

   where
      display' :: Facility -> Locations -> IO ()
      display' fac' (Locations ls) = mapM_ (outputLoc
         (map toLower $ name fac') (map toLower $ location fac')) ls

      outputLoc :: String -> String -> Location -> IO ()
      outputLoc fn fv l = do
         let ln = locName l
         let lv = locVicinity l
         printf "   %2d %s\n" (dist fn ln) ln
         printf "   %2d %s\n\n" (dist fv lv) lv

      dist target input = levenshteinDistance defaultEditCosts
         target (map toLower input)


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
   "https://maps.googleapis.com/maps/api/place/nearbysearch/json?key=%s&location=%f,%f&radius=50&types=%s" apiKey lat lng placesTypes


loadFacility :: FilePath -> IO (Maybe Facility)
loadFacility path = decode `fmap` BL.readFile path
