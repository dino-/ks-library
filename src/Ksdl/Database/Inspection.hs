-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Ksdl.Database.Inspection
   ( mkDoc, saveDoc )
   where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics ( Generic )
import System.FilePath

import qualified Ksdl.Inspection as I
import Ksdl.Places.Match
import qualified Ksdl.Places.Place as P


data Document = Document
   { doctype :: String  -- Maybe doctype or doc_type if necessary
   , inspection :: I.Inspection
   , place :: P.Place
   }
   deriving Generic

instance ToJSON Document where
   toJSON (Document ty insp pl) = object
      [ "_id" .= I._id insp
      , "type" .= ty
      , "place_id" .= P.place_id pl
      , "inspection_source" .= I.inspection_source insp
      , "inspection" .= object
         [ "name" .= I.name insp
         , "addr" .= I.addr insp
         , "date" .= (toJSON . I.date $ insp)
         , "score" .= I.score insp
         ]
      , "place" .= object
         [ "name" .= P.name pl
         , "vicinity" .= P.vicinity pl
         , "location" .= (toJSON . P.location $ pl)
         ]
      ]


--mkJson :: Inspection -> Place -> Document
mkDoc :: Match -> Document
mkDoc (_, i, p) = Document "inspection" i p


saveDoc :: FilePath -> Document -> IO ()
saveDoc dir doc = BL.writeFile (dir </> "ks_" ++ (I._id . inspection $ doc)) $ encode doc
