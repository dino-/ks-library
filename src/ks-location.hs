-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Error
import Data.Aeson
import qualified Data.ByteString as BS
import Data.Either ( partitionEithers )
import Data.List ( isPrefixOf )
import Data.Maybe ( catMaybes, fromJust )
import Data.String.Utils ( strip )
import System.Directory ( getDirectoryContents )
import System.Environment ( getArgs, lookupEnv )
import System.FilePath
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )

import Ksdl.Facility
import Ksdl.Geocoding ( forwardLookup )
import Ksdl.Log
import Ksdl.Match ( Match, csv, match )
import Ksdl.Places ( coordsToPlaces )


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   -- Set up logging
   --let logLevel = DEBUG
   --let logLevel = INFO
   let logLevel = NOTICE
   initLogging logLevel
   logStartMsg lerror

   dir <- head `fmap` getArgs

   -- Paths to all files
   files <-
      ( map (dir </>)                     -- ..relative paths
      . filter (not . isPrefixOf ".") )   -- ..minus dotfiles
      `fmap` getDirectoryContents dir     -- All files

   -- Loaded Facilities
   facs <- catMaybes `fmap` mapM loadFacility files

   placesApiKey <- loadPlacesKey

   -- Look up each inspection facility with Geocoding and Places
   results <- mapM (lookupFacility placesApiKey) facs
   infoM lerror $ replicate 70 '-'

   -- Separate the failures from the successes
   let (failures, matches) = partitionEithers results

   -- Report the failures in the error log
   mapM_ (errorM lerror) failures

   csv . concat $ matches

   logStopMsg lerror


lookupFacility :: String -> Facility ->
   IO (Either String [Match])
lookupFacility placesApiKey fac = runErrorT $ do
   liftIO $ do
      infoM lerror $ replicate 70 '-'
      infoM lerror $ show fac

   locations <- forwardLookup fac >>=
      coordsToPlaces placesApiKey (name fac)

   -- :: [Match]
   matches <- match fac locations

   return matches


loadFacility :: FilePath -> IO (Maybe Facility)
loadFacility path = decodeStrict' `fmap` BS.readFile path


-- Google Places API key
loadPlacesKey :: IO String
loadPlacesKey =
   strip `fmap`         -- ..strip any trailing whitespace
   (readFile =<<        -- ..read the contents of this file
   ((</> ".gplaces") .  -- ..append the Places API key filename
   -- FIXME fromJust is bad
   fromJust) `fmap`     -- ..extracted from the Maybe
   lookupEnv "HOME")    -- Maybe $HOME directory
