-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Char ( toLower )
import Data.Either ( partitionEithers )
import Data.List ( isPrefixOf )
import Data.Maybe ( catMaybes, fromJust )
import Data.String.Utils ( strip )
import qualified Data.Text as T
import qualified Data.Text.Format as TF
import System.Directory ( getDirectoryContents )
import System.Environment ( getArgs, lookupEnv )
import System.FilePath
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )
import Text.EditDistance

import Ksdl.Facility
import Ksdl.Geocoding ( forwardLookup )
import Ksdl.Log
import Ksdl.Places


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   -- Set up logging
   --let logLevel = DEBUG
   let logLevel = INFO
   initLogging logLevel

   dir <- head `fmap` getArgs

   -- Paths to all files
   files <-
      ( map (dir </>)                     -- ..relative paths
      . filter (not . isPrefixOf ".") )   -- ..minus dotfiles
      `fmap` getDirectoryContents dir     -- All files

   -- Loaded Facilities
   facs <- catMaybes `fmap` mapM loadFacility files
   mapM_ (debugM lerror . show) facs

   -- Geocoding results
   gcResults <- mapM forwardLookup $ map location facs
   let gcWithFacs = zipWith
         (\f e -> either (\m -> Left (f, m)) (\l -> Right (f, l)) e)
         facs gcResults
   let (gcFailures, gcLocs) = partitionEithers gcWithFacs
   mapM_ (errorM lerror . show) gcFailures
   mapM_ (debugM lerror . show) gcLocs

   -- Places API
   placesApiKey <-
      strip `fmap`         -- ..strip any trailing whitespace
      (readFile =<<        -- ..read the contents of this file
      ((</> ".gplaces") .  -- ..append the Places API key filename
      -- FIXME fromJust is bad
      fromJust) `fmap`     -- ..extracted from the Maybe
      lookupEnv "HOME")    -- Maybe $HOME directory
   --print placesApiKey

   plResults <- mapM (coordsToPlace placesApiKey) $ map snd gcLocs
   let plWithFacs = zipWith
         (\f e -> either (\m -> Left (f, m)) (\l -> Right (f, l)) e)
         (map fst gcLocs) plResults
   let (plFailures, plLocs) = partitionEithers plWithFacs
   mapM_ (errorM lerror . show) plFailures
   mapM_ (debugM lerror . show) plLocs

   -- Produce output
   putStrLn "\"ndist\",\"vdist\",\"iname\",\"pname\",\"ivic\",\"pvic\",\"_id\""
   mapM_ csv plLocs


csv :: (Facility, Locations) -> IO ()
csv (fac, Locations locs) = mapM_ (line fac) locs
   where
      line :: Facility -> Location -> IO ()
      line fac' loc = TF.print
            "{},{},\"{}\",\"{}\",\"{}\",\"{}\",\"{}\"\n"
            ( (dist (name fac') (T.unpack $ locName loc))
            , (dist (location fac') (T.unpack $ locVicinity loc))
            , (T.pack $ name fac')
            , (locName loc)
            , (T.pack $ location fac')
            , (locVicinity loc)
            , (T.pack $ _id fac')
            )

      dist target input = levenshteinDistance defaultEditCosts
         (map toLower target) (map toLower input)


{-
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
-}


loadFacility :: FilePath -> IO (Maybe Facility)
loadFacility path = decode' `fmap` BL.readFile path
