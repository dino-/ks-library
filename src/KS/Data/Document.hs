-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module KS.Data.Document
   ( Document (..)
   , loadDocument, saveDocument
   )
   where

import           Data.Aeson
   ( FromJSON, ToJSON, eitherDecodeStrict', encode )
import           Data.Bson.Generic ( FromBSON, ToBSON )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import           GHC.Generics ( Generic )
import           System.Directory ( doesFileExist )
import           System.FilePath ( (</>), (<.>) )
import           Text.Printf ( printf )

import           KS.Data.Common ( formatDay, scrubName )
import qualified KS.Data.Inspection as I
import qualified KS.Data.Place as P


data Document = Document
   { doctype :: String
   , inspection :: I.Inspection
   , place :: P.Place
   }
   deriving (Eq, Generic, Show)

instance ToJSON Document
instance FromJSON Document

instance ToBSON Document
instance FromBSON Document


saveDocument :: FilePath -> Document -> IO String
saveDocument = saveDocNumbered Nothing


{- The reason this is complicated is that we sometimes have
   establishment name collision on a given day (think a city with
   lots of fast food restaurants). This function will append numbers
   starting with 2 to the end of the namePart until it has a name
   it can safely save with.
-}
saveDocNumbered :: Maybe Int -> FilePath -> Document -> IO String
saveDocNumbered mnum dir doc = do
   let datePart = formatDay . I.date . inspection $ doc
   let namePart = T.unpack . scrubName . P.name . place $ doc
   let numberPart = maybe "" show mnum

   let filename = printf "ks_%s_%s%s" datePart namePart numberPart
   let path = dir </> filename <.> "json"

   exists <- doesFileExist path
   if exists
      then saveDocNumbered (incrNum mnum) dir doc
      else do
         BL.writeFile path $ encode doc
         return path

   where
      incrNum Nothing  = Just 2
      incrNum (Just n) = Just $ n + 1


loadDocument :: FilePath -> IO Document
loadDocument path = do
   bytes <- BS.readFile path
   case eitherDecodeStrict' bytes of
      Left msg -> ioError $ userError msg
      Right doc -> return doc
