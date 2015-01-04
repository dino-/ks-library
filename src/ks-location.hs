-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Char ( toLower )
import Data.Either ( partitionEithers )
import Data.Function ( on )
import Data.List ( foldl', isPrefixOf, maximumBy, tails )
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

   mapM_ csv plLocs


{- Longest Common Substring function borrowed from Wikibooks:
   https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Longest_common_substring
-}
longestCommonSubstring :: (Eq a) => [a] -> [a] -> [a]
longestCommonSubstring xs ys = maximumBy (compare `on` length) . concat
   $ [f xs' ys | xs' <- tails xs] ++ [f xs ys' | ys' <- drop 1 $ tails ys]

   where f xs' ys' = scanl g [] $ zip xs' ys'
         g z (x, y) = if x == y then z ++ [x] else []


{- Produce development output, to be imported into spreadsheet
-}
csv :: (Facility, Locations) -> IO ()
csv (fac, Locations locs) = do
   putStrLn "\"ncsl\",\"vcsl\",\"iname\",\"pname\",\"ivic\",\"pvic\",\"_id\""
   mapM_ (line fac) locs

   where
      line :: Facility -> Location -> IO ()
      line fac' loc = TF.print
         "{},{},\"{}\",\"{}\",\"{}\",\"{}\",\"{}\"\n"
         ( (commonSubLength (name fac') (T.unpack $ locName loc))
         , (commonSubLength (location fac') (T.unpack $ locVicinity loc))
         , (T.pack $ name fac')
         , (locName loc)
         , (T.pack $ location fac')
         , (locVicinity loc)
         , (T.pack $ _id fac')
         )

      commonSubLength target input = length $ longestCommonSubstring
         (clean target) (clean input)

      clean s = foldl' (flip id) s
         [ takeWhile (/= ',')
         , map toLower
         , filter (\c -> elem c "abcdefghijklmnopqrstuvwxyz0123456789")
         ]


loadFacility :: FilePath -> IO (Maybe Facility)
loadFacility path = decode' `fmap` BL.readFile path
