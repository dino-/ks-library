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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Data.Time ( TimeZone, UTCTime (..), defaultTimeLocale, formatTime
   , localTimeToUTC )
import Data.Time.Calendar ( Day, fromGregorian )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import Data.Time.LocalTime ( LocalTime (..), midnight )
import GHC.Generics ( Generic )
import System.Directory ( doesFileExist )
import System.FilePath
import Text.Printf ( printf )
import Text.Regex ( matchRegex, mkRegex )

import KS.Data.Common ( scrubName )


data Inspection = Inspection
   { inspection_source :: String
   , name :: T.Text
   , addr :: T.Text
   , date :: UTCTime
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


nullInspection :: Inspection
nullInspection = Inspection "" "" "" (posixSecondsToUTCTime 0) 0.0 0 0 False ""


parseDate :: TimeZone -> String -> Either String UTCTime
parseDate tz dateStr =
   maybe (Left $ "Unable to parse date: " ++ dateStr) (Right . localTimeToUTC tz) localTime

   where
      pat = mkRegex "([0-9]{2})/([0-9]{2})/([0-9]{4})"
      match = matchRegex pat dateStr

      localTime = (\d -> LocalTime d midnight) <$> (mkDay =<< match)

      mkDay :: [String] -> Maybe Day
      mkDay (m : d : y : _) = Just $ fromGregorian (read y) (read m) (read d)
      mkDay _               = Nothing


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
   let datePart = (formatTime defaultTimeLocale "%Y-%m-%d")
         . date $ insp
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
