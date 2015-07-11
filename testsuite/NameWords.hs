-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module NameWords
   ( tests )
   where

import Data.Text hiding ( map )
import Test.HUnit
import Text.Printf ( printf )

import KS.Data.Inspection
import KS.Locate.Config
import KS.Locate.Locate
import KS.Locate.Opts
import KS.Locate.Places.NameWords ( toList )


tests :: Test
tests = TestList $ map testNameWords testData


testNameWords :: (Text, [Text]) -> Test
testNameWords (input, output) = TestCase $ do
   conf <- loadConfig defaultOptions
   let insp = fakeInspection input
   actual <- runKSDL (Env conf insp) toList
   let label = printf "name words for \"%s\"" (unpack input)
   assertEqual label (Right output) actual


testData :: [(Text, [Text])]
testData =
   [ ("Belle at The Jones House", ["belle"])
   , ("BOJANGLES #15", ["bojangles"])
   , ("Cafe Tiramisu/North Ridge Pub", ["tiramisu"])
   , ("Flights Restaurant-Raleigh Renaissance", ["flights"])
   , ("INTER-FAITH FOOD SHUTTLE @ INTERACT", ["interact"])
   , ("K&W CAFETERIA", ["k&w"])
   , ("Kadhai-The Indian Wok", ["kadhai"])
   , ("NC Farm Bureau Cafeteria", ["farm"])
   , ("New Wangs Kitchen", ["wangs"])
   , ("Piccola Italia", ["piccola"])
   , ("Quiznos Sub", ["quiznos"])
   , ("R.J.`S PLACE", ["rjs"])
   , ("SAMI'S SUBS, PIZZA & MORE", ["samis"])
   , ("Tonys Bourbon Street Oyster Bar", ["tonys"])
   ]


fakeInspection :: Text -> Inspection
fakeInspection name' = nullInspection { name = name' }
