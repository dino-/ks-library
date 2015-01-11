-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module Ksdl.Match
   ( csv, match )
   where

import Data.Char ( isDigit )
import qualified Data.Text as T
import qualified Data.Text.Format as TF
--import Debug.Trace ( trace )

import Ksdl.Facility
import Ksdl.Places


match :: (Facility, [Location]) -> [(Bool, Facility, Location)]
match (fac, locs) = map combine locs
   where
      combine loc = ((isMatch (location fac) (locVicinity loc)),
         fac, loc)


isMatch :: T.Text -> T.Text -> Bool
isMatch ivic pvic = prefix ivic == prefix pvic
   where prefix = T.takeWhile isDigit


csv :: [(Bool, Facility, Location)] -> IO ()
csv xs = do
   putStrLn "\"pl\",\"iname\",\"pname\",\"ivic\",\"pvic\",\"_id\""
   mapM_ csvOne xs


csvOne :: (Bool, Facility, Location) -> IO ()
csvOne (isPlace, fac, loc) = do
   TF.print "\"{}\",\"{}\",\"{}\",\"{}\",\"{}\",\"{}\"\n"
      ( toStar isPlace
      , name fac
      , locName loc
      , location fac
      , locVicinity loc
      , _id fac
      )

   where
      toStar :: Bool -> T.Text
      toStar True  = "*"
      toStar False = ""
