-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric #-}

module KS.Data.Document
   ( Document (..), loadDoc, saveDoc )
   where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Data.Time ( defaultTimeLocale, formatTime )
import GHC.Generics ( Generic )
import System.Directory ( doesFileExist )
import System.FilePath
import Text.Printf

import qualified KS.Data.Inspection as I
import qualified KS.Data.Place as P


data Document = Document
   { doctype :: String
   , inspection :: I.Inspection
   , place :: P.Place
   }
   deriving (Generic, Show)

instance ToJSON Document
instance FromJSON Document


saveDoc :: FilePath -> Document -> IO String
saveDoc = saveDocNumbered 1


{- The reason this is complicated is that we sometimes have
   establishment name collision on a given day (think a city with
   lots of fast food restaurants). This function will append numbers
   starting with 2 to the end of the namePart until it has a name
   it can safely save with.
-}
saveDocNumbered :: Int -> FilePath -> Document -> IO String
saveDocNumbered num dir doc = do
   let datePart = (formatTime defaultTimeLocale "%Y-%m-%d")
         . I.date . inspection $ doc
   let namePart = T.unpack . I.scrubName . P.name . place $ doc
   let numberPart = if num < 2 then "" else show num

   let filename = printf "ks_%s_%s%s" datePart namePart numberPart
   let path = dir </> filename <.> "json"

   exists <- doesFileExist path
   if exists
      then saveDocNumbered (num + 1) dir doc
      else do
         BL.writeFile path $ encode doc
         return path


loadDoc :: FilePath -> IO Document
loadDoc path = do
   bytes <- BS.readFile path
   case eitherDecodeStrict' bytes of
      Left msg -> ioError $ userError msg
      Right doc -> return doc
