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
   [ ("BOJANGLES #15", ["bojangles"])
   , ("K&W CAFETERIA", ["k&w"])
   --, ("Flights Restaurant-Raleigh Renaissance", ["flights","raleigh"])
   , ("Flights Restaurant-Raleigh Renaissance", ["flights"])
   , ("Belle at The Jones House", ["belle"])
   , ("Piccola Italia", ["piccola"])
   ]
