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

import Ksdl
import Ksdl.Config


toList :: Text -> Ksdl [Text]
toList name = do
   specialCases <- asks namewordsSpecialCases
   list <- mkList name

   return $ Map.findWithDefault
      list           -- Or make a list for a normal name
      name           -- Find this name..
      specialCases   -- ..in these special cases


mkList :: Text -> Ksdl [Text]
mkList name = do
   stopwords <- asks namewordsStopwords
   return $ L.filter (not . isPrefixOf "#")
      . L.filter (\w -> not $ L.elem w stopwords)
      . L.take 2
      . split (== ' ')
      . filter (not . (== '.'))
      . map hyphenToSpace
      . toLower
      $ name


hyphenToSpace :: Char -> Char
hyphenToSpace '-' = ' '
hyphenToSpace c   = c
