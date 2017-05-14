-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Data.Common
   ( formatDay
   , scrubName
   )
   where

import Data.Char ( isAlphaNum, isSpace )
import qualified Data.Text as T
import Text.Printf ( printf )


scrubName :: T.Text -> T.Text
scrubName = T.filter isAlphaNum .  T.toTitle .  T.filter isAlphaNumOrSpace
   where isAlphaNumOrSpace c = isAlphaNum c || isSpace c


formatDay :: Int -> String
formatDay dateInt = printf "%s-%s-%s" y m d
   where
      dateStr = show dateInt
      y = take 4 dateStr
      m = take 2 . drop 4 $ dateStr
      d = drop 6 dateStr
