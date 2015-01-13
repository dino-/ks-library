-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString as BS
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

import Ksdl
import Ksdl.Config
import Ksdl.Facility
import Ksdl.Geocoding ( forwardLookup )
import Ksdl.Log
import Ksdl.Match ( Match, csv, match )
import Ksdl.Places ( coordsToPlaces )


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   config <- do
      c <- loadConf
      k <- loadPlacesKey
      return $ c { placesApiKey = k }

   initLogging $ logPriority config
   logStartMsg lerror

   dir <- head `fmap` getArgs

   -- Paths to all files
   files <-
      ( map (dir </>)                     -- ..relative paths
      . filter (not . isPrefixOf ".") )   -- ..minus dotfiles
      `fmap` getDirectoryContents dir     -- All files

   -- Loaded Facilities
   facs <- catMaybes `fmap` mapM loadFacility files

   -- Look up each inspection facility with Geocoding and Places
   matches <- concat `fmap` mapM (lookupFacility config) facs
   noticeM lerror line
   csv matches

   logStopMsg lerror


lookupFacility :: Config -> Facility -> IO [Match]
lookupFacility config fac = do
   r <- runKsdl config $ do
      liftIO $ do
         noticeM lerror line
         noticeM lerror $ show fac

      locations <- forwardLookup fac >>=
         coordsToPlaces fac

      -- :: [Match]
      matches <- match fac locations

      return matches

   either (\msg -> noticeM lerror msg >> return []) return r


loadFacility :: FilePath -> IO (Maybe Facility)
loadFacility path = decodeStrict' `fmap` BS.readFile path


loadConf :: IO Config
loadConf = loadConfig "ksdl.conf"


-- Google Places API key
loadPlacesKey :: IO String
loadPlacesKey =
   strip `fmap`         -- ..strip any trailing whitespace
   (readFile =<<        -- ..read the contents of this file
   ((</> ".gplaces") .  -- ..append the Places API key filename
   -- FIXME fromJust is bad
   fromJust) `fmap`     -- ..extracted from the Maybe
   lookupEnv "HOME")    -- Maybe $HOME directory
