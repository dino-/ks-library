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
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )
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
import Ksdl.Places.Match ( match )
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

   (srcDirOrFile, outputSpec) <- getArgs >>= parseArgs

   -- Don't start with this until we know we're not bailing out
   -- because of args
   logStartMsg lname

   -- Paths to all files we'll be processing
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
   mapM_ (lookupInspection config outputSpec) insps

   noticeM lname line

   logStopMsg lname


parseArgs :: [String] -> IO (FilePath, Output)
parseArgs ("-h"     : _)     = usage
parseArgs ("--help" : _)     = usage
parseArgs [src, dest, fail'] = return (src, ToDirs dest fail')
parseArgs [src]              = return (src, ToStdout        )
parseArgs _                  = usage


usage :: IO a
usage = do
   appName <- getProgName
   putStrLn $ unlines
      [ "Look up inspections with Google Geocoding and Places"
      , ""
      , "Usage: " ++ appName ++ " FILE|DIR [SUCCDIR FAILDIR]"
      , "       " ++ appName ++ " FILE|DIR"
      , "       " ++ appName ++ " [OPTIONS]"
      , ""
      , "Options:"
      , "  -h, --help  This usage information"
      , ""
      , "Looks up the file or dir full of files specified"
      , "Writes successful lookups to SUCCDIR or stdout if omitted"
      , "Writes failed lookup input files to FAILDIR"
      , "Expects to find a ./ksdl.conf file."
      , "Logging is written to stdout."
      , ""
      , "Dino Morelli <dino@ui3.info>"
      ]
   exitFailure


lookupInspection :: Config -> Output -> (FilePath, Inspection)
   -> IO ()
lookupInspection config outputSpec (srcPath, insp) = do
   r <- runKsdl (Env config insp) $ do
      liftIO $ do
         noticeM lname line
         noticeM lname $ show insp

      geo <- forwardLookup
      places <- coordsToPlaces geo
      match places

   either (handleFailure outputSpec) (saveDoc outputSpec . mkDoc) r

   where
      handleFailure (ToDirs _ failDir) msg = do
         copyFile srcPath $ failDir </> takeFileName srcPath
         errorM lname msg

      handleFailure (ToStdout) msg =
         errorM lname msg


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
