-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module Inspection
   ( tests )
   where

import Data.Time
import Data.Time.Clock.POSIX
import Test.HUnit

import qualified KS.Data.Inspection as I


tests :: Test
tests = TestList
   [ TestLabel "testParseDate" testParseDate
   ]


testParseDate :: Test
testParseDate = TestCase $ do
   let expected = 1436241600

   tz <- getCurrentTimeZone
   let actual = utcTimeToPOSIXSeconds . I.parseDate tz $ "07/07/2015"

   assertEqual "parsing a date" expected actual
