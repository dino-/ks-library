-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString as BS
import Data.List ( isPrefixOf )
import Data.Maybe ( catMaybes )
import Data.String.Utils ( strip )
import System.Directory ( copyFile, doesFileExist
   , getDirectoryContents , getHomeDirectory )
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
   (srcDirOrFile : destDir : failDir : []) <- getArgs
   files <- do
      isFile <- doesFileExist srcDirOrFile
      if isFile then return [srcDirOrFile]
         else
            ( map (srcDirOrFile </>)                  -- ..relative paths
            . filter (not . isPrefixOf ".") )         -- ..minus dotfiles
            `fmap` getDirectoryContents srcDirOrFile  -- All files

   -- Loaded Facilities
   insps <- catMaybes `fmap` mapM loadInspection files

   -- Look up each inspection facility with Geocoding and Places
   matches <- catMaybes `fmap` mapM (lookupInspection config failDir) insps
   noticeM lname line

   let docs = map mkDoc matches
   mapM_ (saveDoc destDir) docs

   logStopMsg lname


lookupInspection :: Config -> FilePath -> (FilePath, Inspection)
   -> IO (Maybe Match)
lookupInspection config failDir (srcPath, insp) = do
   r <- runKsdl (Env config insp) $ do
      liftIO $ do
         noticeM lname line
         noticeM lname $ show insp

      places <- forwardLookup >>=
         coordsToPlaces

      Just `fmap` match places

   either handleFailure return r

   where
      handleFailure msg = do
         copyFile srcPath $ failDir </> takeFileName srcPath
         errorM lname msg
         return Nothing


loadInspection :: FilePath -> IO (Maybe (FilePath, Inspection))
loadInspection path =
   (maybe Nothing (\i -> Just (path, i)) . decodeStrict')
      `fmap` BS.readFile path


-- Google Places API key
loadPlacesKey :: IO String
loadPlacesKey =
   strip `fmap`            -- ..strip any trailing whitespace
   (readFile =<<           -- ..read the contents of this file
   (</> ".gplaces") `fmap` -- ..append the Places API key filename
   getHomeDirectory)
