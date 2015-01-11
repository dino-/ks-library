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


testNameWords :: (Text, [Text]) -> Test
testNameWords (input, output) = TestCase $ do
   assertEqual label output $ toList input

   where
      label = printf "name words for \"%s\"" (unpack input)


testData :: [(Text, [Text])]
testData =
   [ ("Belle at The Jones House", ["belle"])
   , ("BOJANGLES #15", ["bojangles"])
   , ("Flights Restaurant-Raleigh Renaissance", ["flights"])
   , ("K&W CAFETERIA", ["k&w"])
   , ("Kadhai-The Indian Wok", ["kadhai"])
   , ("Piccola Italia", ["piccola"])
   , ("Tonys Bourbon Street Oyster Bar", ["tonys"])
   ]
