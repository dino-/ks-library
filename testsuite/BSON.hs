-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module BSON
   ( tests )
   where

import Data.Bson ( (=:) )
import Data.Bson.Generic
import qualified Data.Text as T
import System.FilePath ( (</>) )
import Test.HUnit

import qualified KS.Data.Document as D


tests :: Test
tests = TestList
   [ TestLabel "Converting a Document to BSON" testDocToBSON
   , TestLabel "Converting BSON to a Document" testBSONToDoc
   ]


testDocToBSON :: Test
testDocToBSON = TestCase $ do
   let expected =
         [ "doctype" =: ("inspection" :: T.Text)
         , "inspection" =:
            [ "inspection_source" =: ("nc_wake" :: T.Text)
            , "name" =: ("Babymoon Cafe" :: T.Text)
            , "addr" =: ("100 Jerusalem DR MORRISVILLE, NC 27560" :: T.Text)
            , "date" =: (1436328000 :: Integer)
            , "score" =: (96.5 :: Double)
            , "violations" =: (7 :: Int)
            , "crit_violations" =: (1 :: Int)
            , "reinspection" =: True
            , "detail" =: ("http://wake-nc.healthinspections.us/_templates/NC/Restaurant/_report_full.cfm?reportID=5A901D13-5056-A20B-FAD300D021B99CB1&facilityID=9C2F5425-154F-13B8-8BB698F8E851909E&rtype=Restaurant&domainID=15&ps=ps" :: T.Text)
            ]
         , "place" =:
            [ "name" =: ("Babymoon Cafe" :: T.Text)
            , "vicinity" =: ("100 Jerusalem Drive #106, Morrisville" :: T.Text)
            , "location" =:
               [ "type" =: ("Point" :: String)
               , "coordinates" =: ([-78.827118,35.851295] :: [Double])
               ]
            , "types" =: (["cafe","restaurant","food","point_of_interest","establishment"] :: [T.Text])
            , "place_id" =: ("ChIJM47UNN3xrIkRQoSk4tao5x4" :: T.Text)
            ]
         ]

   actual <- toBSON <$>
      (D.loadDocument $ "testsuite" </> "ks_2015-07-08_BabymoonCafe.json")

   expected @=? actual


testBSONToDoc :: Test
testBSONToDoc = TestCase $ do
   loadedDocument <- D.loadDocument $ "testsuite" </> "ks_2015-07-08_BabymoonCafe.json"
   (Just loadedDocument) @=? (fromBSON . toBSON $ loadedDocument)
