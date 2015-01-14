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
import Ksdl.Inspection
import Ksdl.Geocoding ( forwardLookup )
import Ksdl.Log
import Ksdl.Match ( Match, csv, match )
import Ksdl.Places ( coordsToPlaces )


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   config <- do
      c <- loadConfig "ksdl.conf"
      k <- loadPlacesKey
      return $ c { placesApiKey = k }

   initLogging $ logPriority config
   logStartMsg lname

   dir <- head `fmap` getArgs

   -- Paths to all files
   files <-
      ( map (dir </>)                     -- ..relative paths
      . filter (not . isPrefixOf ".") )   -- ..minus dotfiles
      `fmap` getDirectoryContents dir     -- All files

   -- Loaded Facilities
   insps <- catMaybes `fmap` mapM loadInspection files

   -- Look up each inspection facility with Geocoding and Places
   matches <- concat `fmap` mapM (lookupInspection config) insps
   noticeM lname line
   csv matches

   logStopMsg lname


lookupInspection :: Config -> Inspection -> IO [Match]
lookupInspection config insp = do
   r <- runKsdl config $ do
      liftIO $ do
         noticeM lname line
         noticeM lname $ show insp

      locations <- forwardLookup insp >>=
         coordsToPlaces insp

      -- :: [Match]
      matches <- match insp locations

      return matches

   either (\msg -> errorM lname msg >> return []) return r


loadInspection :: FilePath -> IO (Maybe Inspection)
loadInspection path = decodeStrict' `fmap` BS.readFile path


-- Google Places API key
loadPlacesKey :: IO String
loadPlacesKey =
   strip `fmap`         -- ..strip any trailing whitespace
   (readFile =<<        -- ..read the contents of this file
   ((</> ".gplaces") .  -- ..append the Places API key filename
   -- FIXME fromJust is bad
   fromJust) `fmap`     -- ..extracted from the Maybe
   lookupEnv "HOME")    -- Maybe $HOME directory
