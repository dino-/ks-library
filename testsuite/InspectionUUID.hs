-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module InspectionUUID
   ( tests )
   where

import Test.HUnit
import Text.Printf ( printf )

import Ksdl.Inspection


tests :: Test
tests = TestList $ map testFacUUID testData


testFacUUID :: Inspection -> Test
testFacUUID expected = TestCase $ do
   let label = printf "construct UUID for: %s" (show expected)
   let actual = setId expected
   assertEqual label (_id expected) (_id actual)


testData :: [Inspection]
testData =
   [ Inspection
      { _id = "e33231ae-b075-520e-9a29-44d3fe90a37a"
      , insp_name = "Panda King"
      , insp_addr = "3626 Rogers RD WAKE FOREST, NC 27587"
      , insp_date = [2014,12,10]
      , insp_score = 92.0
      }
   ]
