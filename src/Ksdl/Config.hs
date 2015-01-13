-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Ksdl.Config
   ( Config (..)
   , loadConfig )
   where

import Data.List ( isPrefixOf )
import qualified Data.Map as Map
import qualified Data.Text as T
import System.Log


data Config = Config
   { logPriority :: Priority
   , geocodingApiDelay :: Int
   , namewordsStopwords :: [T.Text]
   , namewordsSpecialCases :: Map.Map T.Text [T.Text]
   , placesApiKey :: String
   }
   deriving (Read, Show)


loadConfig :: FilePath -> IO Config
loadConfig path = (read . removeComments) `fmap` readFile path


{- Auto-derived Read instancing has no idea how to handle Haskell source
   code commenting. This will strip out very simple -- style comments.
-}
removeComments :: String -> String
removeComments = unlines . map removeComment . lines
   where
      removeComment =
         unwords . (takeWhile (not . isPrefixOf "--")) . words
