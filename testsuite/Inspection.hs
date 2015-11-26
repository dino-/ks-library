-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module Inspection
   ( tests )
   where

import Data.Time
import System.Environment ( setEnv )
import Test.HUnit
import Text.Printf ( printf )

import qualified KS.Data.Inspection as I


tests :: Test
tests = TestList
   [ TestLabel "testParseDateGood" testParseDateGood
   , TestLabel "testParseDateBad" testParseDateBad
   ]


testParseDateGood :: Test
testParseDateGood = TestCase $ do
   let expY = 2015
   let expM = 7
   let expD = 7

   setEnv "TZ" "America/New_York"
   tz <- getCurrentTimeZone
   let actual = toGregorian . utctDay <$>
         (I.parseDate tz $ printf "%02d/%02d/%4d" expM expD expY)

   Right (expY, expM, expD) @=? actual


testParseDateBad :: Test
testParseDateBad = TestCase $ do
   let expected = Left "Unable to parse date: foo bar baz"

   setEnv "TZ" "America/New_York"
   tz <- getCurrentTimeZone
   let actual = I.parseDate tz "foo bar baz"
   print actual

   expected @=? actual
