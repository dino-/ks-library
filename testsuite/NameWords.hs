-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module NameWords
   ( tests )
   where

import Data.Text hiding ( map )
import Test.HUnit
import Text.Printf ( printf )

import Ksdl.NameWords ( toList )


tests :: Test
tests = TestList $ map testNameWords testData


testData :: [(Text, [Text])]
testData =
   [ ("Ruby Tuesday`s #3182", ["ruby", "tuesday"])
   , ("Ruby Tuesday", ["ruby", "tuesday"])
   , ("Panda King", ["panda", "king"])
   , ("Panda King Chinese Restaurant", ["panda", "king"])
   , ("Il Bacio Italian Grill and Pizzeria", ["il", "bacio"])
   , ("Sweet CE CE's-Heritage LLC", ["sweet", "ce"])
   ]


testNameWords :: (Text, [Text]) -> Test
testNameWords (input, output) = TestCase $ do
   assertEqual label output $ toList input

   where
      label = printf "name words for \"%s\"" (unpack input)
