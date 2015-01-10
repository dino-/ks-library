-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module Ksdl.NameWords
   ( toList
   )
   where

--import qualified Data.List as L
import Data.Text


toList :: Text -> [Text]
toList orig = ["truffle", "cat"]
