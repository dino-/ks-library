-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

{- |
   This module is for working with data in MongoDB
-}

module KS.Data.BSON
   ( bsonToDoc
   , docToBSON
   )
   where

import Data.Bson
import Data.Geospatial ( GeoPoint (..) )
import qualified Data.Text as T

import qualified KS.Data.Document as D
import qualified KS.Data.Inspection as I
import qualified KS.Data.Place as P


{- |
   Convert a BSON Document into a KitchenSnitch inspection Document
-}
bsonToDoc :: Document -> D.Document
bsonToDoc bson = D.Document
   { D.doctype = at "doctype" bson
   , D.inspection = I.Inspection
      { I.inspection_source = at "inspection_source" bsonInsp
      , I.name = at "name" bsonInsp
      , I.addr = at "addr" bsonInsp
      , I.date = at "date" bsonInsp
      , I.score = at "score" bsonInsp
      , I.violations = at "violations" bsonInsp
      , I.crit_violations = at "crit_violations" bsonInsp
      , I.reinspection = at "reinspection" bsonInsp
      , I.detail = at "detail" bsonInsp
      }
   , D.place = P.Place
      { P.name = at "name" bsonPlace
      , P.vicinity = at "vicinity" bsonPlace
      , P.location = GeoPoint $ "coordinates" `at` ("location" `at` bsonPlace)
      , P.types = at "types" bsonPlace
      , P.place_id = at "place_id" bsonPlace
      }
   }

   where
      bsonInsp = at "inspection" bson
      bsonPlace = at "place" bson


{- |
   Convert a KitchenSnitch inspection Document into a BSON Document
-}
docToBSON :: D.Document -> Document
docToBSON doc = bdoc
   where
      insp = D.inspection doc
      pl = D.place doc
      (GeoPoint coords) = P.location pl
      bdoc =
         [ "doctype" =: D.doctype doc
         , "inspection" =:
            [ "inspection_source" =: I.inspection_source insp
            , "name" =: I.name insp
            , "addr" =: I.addr insp
            , "date" =: I.date insp
            , "score" =: I.score insp
            , "violations" =: I.violations insp
            , "crit_violations" =: I.crit_violations insp
            , "reinspection" =: I.reinspection insp
            , "detail" =: I.detail insp
            ]
         , "place" =:
            [ "name" =: P.name pl
            , "vicinity" =: P.vicinity pl
            , "location" =:
               [ "type" =: ("Point" :: T.Text)
               , "coordinates" =: coords
               ]
            , "types" =: P.types pl
            , "place_id" =: P.place_id pl
            ]
         ]
