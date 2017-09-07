-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module Common
   ( tests )
   where

import Test.Hspec

import KS.Data.Common ( scrubName )


tests :: SpecWith ()
tests = describe "Scrubbing names" $ do
   it "simple name" $ scrubName "FOO BAR" `shouldBe` "FooBar"
   it "with punctuation" $ scrubName "FOO`S BAR" `shouldBe` "FoosBar"
   it "much lowercase" $ scrubName "Pho pho pho bar and kitchen"
      `shouldBe` "PhoPhoPhoBarAndKitchen"
