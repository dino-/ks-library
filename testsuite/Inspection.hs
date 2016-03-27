-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module Inspection
   ( tests )
   where

import Test.HUnit

import qualified KS.Data.Inspection as I


tests :: Test
tests = TestList
   [ TestLabel "testParseDateGoodEDT" testParseDateGoodEDT
   , TestLabel "testParseDateGoodEST" testParseDateGoodEST
   , TestLabel "testParseDateBad" testParseDateBad
   , TestLabel "testParseDateShort" testParseDateShort
   ]


testParseDate :: Either String Int -> String -> Test
testParseDate expected input = TestCase $ do
   let actual = I.parseDate input
   expected @=? actual


testParseDateGoodEDT :: Test
testParseDateGoodEDT = testParseDate (Right 20150331) "03/31/2015"

testParseDateGoodEST :: Test
testParseDateGoodEST = testParseDate (Right 20160116) "01/16/2016"

testParseDateBad :: Test
testParseDateBad =
   testParseDate (Left "Unable to parse date: foo bar baz") "foo bar baz"

testParseDateShort :: Test
testParseDateShort = testParseDate (Right 20160504) "5/4/2016"
