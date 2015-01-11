-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module Ksdl.NameWords
   ( toList
   )
   where

import qualified Data.List as L
import Data.Text
import Prelude hiding ( map )


toList :: Text -> [Text]
toList = L.filter (not . isPrefixOf "#")
   . L.filter (\w -> not $ L.elem w stopwords)
   . L.take 2
   . L.takeWhile (/= "at")
   . split (== ' ')
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
   , "italia"        -- special case
   , "of"
   , "on"
   , "raleigh"       -- special case
   , "rest"
   , "rest."
   , "restaurant"
   , "the"
   ]
