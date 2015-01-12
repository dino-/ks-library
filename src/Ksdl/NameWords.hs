-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module Ksdl.NameWords
   ( toList
   )
   where

import qualified Data.List as L
import qualified Data.Map as Map
import Data.Text
import Prelude hiding ( filter, map )


toList :: Text -> [Text]
toList name = Map.findWithDefault
   (mkList name)  -- Or make a list for a normal name
   name           -- Find this name..
   specialCases   -- ..in these special cases


mkList :: Text -> [Text]
mkList = L.filter (not . isPrefixOf "#")
   . L.filter (\w -> not $ L.elem w stopwords)
   . L.take 2
   . split (== ' ')
   . filter (not . (== '.'))
   . map hyphenToSpace
   . toLower


hyphenToSpace :: Char -> Char
hyphenToSpace '-' = ' '
hyphenToSpace c   = c


stopwords :: [Text]
stopwords =
   [ ""
   , "&"
   , "@"
   , "a"
   , "and"
   , "bar"
   , "cafe"
   , "cafeteria"
   , "grill"
   , "grille"
   , "in"
   , "of"
   , "on"
   , "rest"
   , "rest."
   , "restaurant"
   , "the"
   ]


specialCases :: Map.Map Text [Text]
specialCases = Map.fromList
   [ ("Belle at The Jones House", ["belle"])
   , ("Flights Restaurant-Raleigh Renaissance", ["flights"])
   , ("Piccola Italia", ["piccola"])
   , ("Tonys Bourbon Street Oyster Bar", ["tonys"])
   ]
