-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module Inspection
   ( tests )
   where

import System.Environment ( setEnv )
import Test.HUnit

import qualified KS.Data.Inspection as I


tests :: Test
tests = TestList
   [ TestLabel "testParseDateGoodEDT" testParseDateGoodEDT
   , TestLabel "testParseDateGoodEST" testParseDateGoodEST
   , TestLabel "testParseDateBad" testParseDateBad
   ]


testParseDate :: Either String Int -> String -> Test
testParseDate expected input = TestCase $ do
   setEnv "TZ" "America/New_York"
   actual <- I.parseDate input
   expected @=? actual


testParseDateGoodEDT :: Test
testParseDateGoodEDT = testParseDate (Right 1427774400) "03/31/2015"

testParseDateGoodEST :: Test
testParseDateGoodEST = testParseDate (Right 1452920400) "01/16/2016"

testParseDateBad :: Test
testParseDateBad =
   testParseDate (Left "Unable to parse date: foo bar baz") "foo bar baz"
