-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString as BS
import Data.List ( isPrefixOf )
import Data.Maybe ( catMaybes )
import Data.String.Utils ( strip )
import System.Directory ( doesFileExist, getDirectoryContents
   , getHomeDirectory )
import System.Environment ( getArgs )
import System.FilePath
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )

import Ksdl
import Ksdl.Config
import Ksdl.Database.Inspection
import Ksdl.Inspection
import Ksdl.Log
import Ksdl.Places.Geocoding ( forwardLookup )
import Ksdl.Places.Match ( Match, match )
import Ksdl.Places.Place ( coordsToPlaces )


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

   -- Paths to all files we'll be processing
   dirOrFile <- head `fmap` getArgs
   files <- do
      isFile <- doesFileExist dirOrFile
      if isFile then return [dirOrFile]
         else
            ( map (dirOrFile </>)                  -- ..relative paths
            . filter (not . isPrefixOf ".") )      -- ..minus dotfiles
            `fmap` getDirectoryContents dirOrFile  -- All files

   -- Loaded Facilities
   insps <- catMaybes `fmap` mapM loadInspection files

   -- Look up each inspection facility with Geocoding and Places
   matches <- concat `fmap` mapM (lookupInspection config) insps
   noticeM lname line

   let dbjs = map mkDoc $ filter posMatch matches
   mapM_ (saveDoc "../data/db") dbjs
   --csv matches

   logStopMsg lname


posMatch :: Match -> Bool
posMatch (True , _, _) = True
posMatch (False, _, _) = False


lookupInspection :: Config -> Inspection -> IO [Match]
lookupInspection config insp = do
   r <- runKsdl config $ do
      liftIO $ do
         noticeM lname line
         noticeM lname $ show insp

      places <- forwardLookup insp >>=
         coordsToPlaces insp

      -- :: [Match]
      matches <- match insp places

      return matches

   either (\msg -> errorM lname msg >> return []) return r


loadInspection :: FilePath -> IO (Maybe Inspection)
loadInspection path = decodeStrict' `fmap` BS.readFile path


-- Google Places API key
loadPlacesKey :: IO String
loadPlacesKey =
   strip `fmap`            -- ..strip any trailing whitespace
   (readFile =<<           -- ..read the contents of this file
   (</> ".gplaces") `fmap` -- ..append the Places API key filename
   getHomeDirectory)
