-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module KS.Data.Inspection
   ( Inspection (..)
   , nullInspection
   , parseDate
   , saveInspection
   , loadInspection
   , scrubName
   )
   where

import Data.Aeson ( FromJSON, ToJSON, eitherDecodeStrict', encode )
import Data.Bson.Generic
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import GHC.Generics ( Generic )
import System.Directory ( doesFileExist )
import System.FilePath
import Text.Printf ( printf )
import Text.Regex ( matchRegex, mkRegex )

import KS.Data.Common ( formatDay, scrubName )


data Inspection = Inspection
   { inspection_source :: String
   , name :: T.Text
   , addr :: T.Text
   , date :: Int
   , score :: Double
   , violations :: Int
   , crit_violations :: Int
   , reinspection :: Bool
   , detail :: String
   }
   deriving (Eq, Generic)

instance Show Inspection
   where show = formatForDisplay

instance FromJSON Inspection
instance ToJSON Inspection

instance FromBSON Inspection
instance ToBSON Inspection


nullInspection :: Inspection
nullInspection = Inspection "" "" "" 20140101 0.0 0 0 False ""


parseDate :: String -> IO (Either String Int)
parseDate dateStr =
   maybe
      (return $ Left $ "Unable to parse date: " ++ dateStr)
      (return . Right . ymdToInt)
      (extractParts =<< match)

   where
      match = matchRegex (mkRegex "([0-9]{1,2})/([0-9]{1,2})/([0-9]{4})") dateStr

      extractParts :: [String] -> Maybe (Int, Int, Int)
      extractParts (m : d : y : _) = Just $ ((read y), (read m), (read d))
      extractParts _               = Nothing

      ymdToInt :: (Int, Int, Int) -> Int
      ymdToInt (y, m, d) = read $ printf "%d%02d%02d" y m d


saveInspection :: FilePath -> Inspection -> IO FilePath
saveInspection = saveInspNumbered Nothing


{- The reason this is complicated is that we sometimes have
   establishment name collision on a given day (think a city with
   lots of fast food restaurants). This function will append numbers
   starting with 2 to the end of the namePart until it has a name
   it can safely save with.
-}
saveInspNumbered :: Maybe Int -> FilePath -> Inspection -> IO String
saveInspNumbered mnum dir insp = do
   let datePart = formatDay . date $ insp
   let namePart = T.unpack . scrubName . name $ insp
   let numberPart = maybe "" show mnum

   let filename = printf "insp_%s_%s%s" datePart namePart numberPart
   let path = dir </> filename <.> "json"

   exists <- doesFileExist path
   if exists
      then saveInspNumbered (incrNum mnum) dir insp
      else do
         BL.writeFile path $ encode insp
         return path

   where
      incrNum Nothing  = Just 2
      incrNum (Just n) = Just $ n + 1


loadInspection :: FilePath -> IO Inspection
loadInspection path = do
   bytes <- BS.readFile path
   case eitherDecodeStrict' bytes of
      Left msg -> ioError $ userError msg
      Right insp -> return insp


formatForDisplay :: Inspection -> String
formatForDisplay
   (Inspection src n a ut sc viol crit r _) =

   printf mask (T.unpack n) (show ut) sc viol crit (showRe r) src (T.unpack a)

   where
      mask = init . unlines $
         [ "Inspection"
         , "   %s | %s %f %d/%d %s | %s"
         , "   %s"
         ]

      showRe :: Bool -> String
      showRe True  = "Re"
      showRe False = "Ins"
