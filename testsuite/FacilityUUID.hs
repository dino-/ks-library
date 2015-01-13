-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module FacilityUUID
   ( tests )
   where

import Test.HUnit
import Text.Printf ( printf )

import Ksdl.Facility


tests :: Test
tests = TestList $ map testFacUUID testData


testFacUUID :: Facility -> Test
testFacUUID expected = TestCase $ do
   let label = printf "construct UUID for: %s" (show expected)
   let actual = setId expected
   assertEqual label (_id expected) (_id actual)


testData :: [Facility]
testData =
   [ Facility
      { _id = "e33231ae-b075-520e-9a29-44d3fe90a37a"
      , name = "Panda King"
      , score = 92.0
      , location = "3626 Rogers RD WAKE FOREST, NC 27587"
      , inspection_date = [2014,12,10]
      }
   ]
