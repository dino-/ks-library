-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module Inspection
   ( tests )
   where

import Test.Hspec

import qualified KS.Data.Inspection as I


tests :: SpecWith ()
tests = describe "Inspection.parseDate" $ do
   it "good EDT" $ I.parseDate "03/31/2015" `shouldBe` Right 20150331
   it "good EST" $ I.parseDate "01/16/2016" `shouldBe` Right 20160116
   it "bad date" $ I.parseDate "foo bar baz" `shouldBe`
      Left "Unable to parse date: foo bar baz"
   it "short date" $ I.parseDate "5/4/2016" `shouldBe` Right 20160504
