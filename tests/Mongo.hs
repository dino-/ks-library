-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module Mongo
   ( tests )
   where

import Data.Bson
import Data.Text hiding ( map )
import Test.Hspec

import Database.Mongo.Util


tests :: SpecWith ()
tests = describe "parsing MongoDB errors" $ mapM_ testLastError testData


testLastError :: (String, Document, Either String String) -> SpecWith ()
testLastError (label', doc, expected) =
   it label' $ parseLastError doc `shouldBe` expected


testData :: [(String, Document, Either String String)]
testData =
   [ ( "getLastError ok, db operation ok"
     , [ "connectionId" =: (652 :: Int)
       , "n" =: (0 :: Int)
       , "syncMillis" =: (0 :: Int)
       , "writtenTo" =: (Nothing :: Maybe Text)
       , "err" =: (Nothing :: Maybe Text)
       , "ok" =: (1.0 :: Double)
       ]
     , Right "operation successful"
     )
   , ( "getLastError ok, db operation NOT ok"
     , [ "connectionId" =: (652 :: Int)
       , "n" =: (0 :: Int)
       , "syncMillis" =: (0 :: Int)
       , "writtenTo" =: (Nothing :: Maybe Text)
       , "err" =: ((Just "Some error occurred with insertion!") :: Maybe Text)
       , "ok" =: (1.0 :: Double)
       ]
     , Left "operation FAILED: Some error occurred with insertion!"
     )
   , ( "getLastError NOT ok"
     , [ "connectionId" =: (652 :: Int)
       , "n" =: (0 :: Int)
       , "syncMillis" =: (0 :: Int)
       , "writtenTo" =: (Nothing :: Maybe Text)
       , "err" =: ((Just "Should never see this") :: Maybe Text)
       , "ok" =: (2.0 :: Double)
       ]
     , Left "getLastError FAILED: ok: 2.0"
     )
   ]
