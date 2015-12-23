-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Data.Common
   ( epochToUTCTime
   , formatDay
   , scrubName
   , utcTimeToEpoch
   )
   where

import Data.Char ( isAlphaNum )
import qualified Data.Text as T
import Data.Time ( TimeZone, UTCTime, defaultTimeLocale, formatTime
   , utcToLocalTime )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime, utcTimeToPOSIXSeconds )


scrubName :: T.Text -> T.Text
scrubName = T.filter isAlphaNum


formatDay :: TimeZone -> UTCTime -> String
formatDay tz = formatTime defaultTimeLocale "%Y-%m-%d" . utcToLocalTime tz


epochToUTCTime :: Int -> UTCTime
epochToUTCTime = posixSecondsToUTCTime . realToFrac


utcTimeToEpoch :: UTCTime -> Int
utcTimeToEpoch = round . utcTimeToPOSIXSeconds
