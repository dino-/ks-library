-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module KS.Data.Feedback
   ( Status (..)
   , IssueType (..)
   , Feedback (..)
   )
   where

import Data.Aeson ( FromJSON, ToJSON )
import Data.Bson
import Data.Bson.Generic ( FromBSON ,ToBSON, fromBSON, toBSON )
import qualified Data.Text as T
import GHC.Generics ( Generic )


data Status = New | Duplicate | Resolved
   deriving (Eq, Generic, Read, Show)

instance FromJSON Status
instance ToJSON Status

instance FromBSON Status
instance ToBSON Status


data IssueType
   = Closed | WrongNameAddr | InspectionNotCurrent | Other | NoInspection
   deriving (Eq, Generic, Read, Show)

instance FromJSON IssueType
instance ToJSON IssueType

instance FromBSON IssueType
instance ToBSON IssueType


data Feedback = Feedback
   { status :: Status
   , device_id :: T.Text
   , place_id :: Maybe T.Text
   , name :: Maybe T.Text
   , date :: Int
   , issue_type :: IssueType
   , comment :: Maybe T.Text
   }
   deriving (Eq, Generic, Show)

instance FromJSON Feedback
instance ToJSON Feedback

instance FromBSON Feedback where
   fromBSON doc = Just $ Feedback
      (read ("status" `at` doc))
      ("device_id" `at` doc)
      ("place_id" `at` doc)
      ("name" `at` doc)
      (read ("date" `at` doc))
      (read ("issue_type" `at` doc))
      ("comment" `at` doc)

instance ToBSON Feedback where
   toBSON adt =
      [ "status" =: (show (status adt))
      , "device_id" =: (device_id adt)
      , "place_id" =: (place_id adt)
      , "name" =: (name adt)
      , "date" =: (show (date adt))
      , "issue_type" =: (show (issue_type adt))
      , "comment" =: (comment adt)
      ]
