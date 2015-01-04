-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

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
import Ksdl.Match
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

   -- Compute the matches between inspections and Places data
   let matchDetails = concatMap match plLocs
   csv matchDetails

   return ()


loadFacility :: FilePath -> IO (Maybe Facility)
loadFacility path = decodeStrict' `fmap` BS.readFile path
