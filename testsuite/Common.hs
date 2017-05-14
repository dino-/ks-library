-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module Common
   ( tests )
   where

import Test.HUnit

import KS.Data.Common ( scrubName )


tests :: Test
tests = TestList
   [ TestLabel "scrub simple name" $
      TestCase $ "FooBar" @=? scrubName "FOO BAR"
   , TestLabel "scrub with punctuation" $
      TestCase $ "FoosBar" @=? scrubName "FOO`S BAR"
   , TestLabel "scrub much lowercase" $
      TestCase $ "PhoPhoPhoBarAndKitchen" @=? scrubName "Pho pho pho bar and kitchen"
   ]
