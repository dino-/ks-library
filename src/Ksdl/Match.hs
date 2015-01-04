-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module Ksdl.Match
   ( csv, match )
   where

import Data.Char ( isAlphaNum, toLower )
import Data.Function ( on )
import Data.List ( foldl', maximumBy, sortBy, tails )
import qualified Data.Text as T
import qualified Data.Text.Format as TF
import Text.Regex

import Ksdl.Facility
--import Ksdl.Log
import Ksdl.Places


--match :: (Facility, Locations) -> [(Bool, Int, Int, Facility, Location)]
match :: (Facility, Locations) -> [(Int, Int, Facility, Location)]
match (fac, Locations locs) = reverse . (sortBy cmpMatchValues)
   . (map (computeMatchValues fac)) $ locs


cmpMatchValues :: (Int, Int, a, b) -> (Int, Int, a, b) -> Ordering
cmpMatchValues (ncsl1, vcsl1, _, _) (ncsl2, vcsl2, _, _) =
   compare (ncsl1, vcsl1) (ncsl2, vcsl2)


computeMatchValues :: Facility -> Location -> (Int, Int, Facility, Location)
computeMatchValues fac loc = (ncsl, vcsl, fac, loc)
   where
      ncsl = commonSubLength (name fac) (T.unpack $ locName loc)
      vcsl = commonSubLength (location fac) (T.unpack $ locVicinity loc)

      commonSubLength target input = length $ longestCommonSubstring
         (clean target) (clean input)

      -- Patterns for some "stop" words
      reRestaurant = mkRegex "restaurant"
      reShop = mkRegex "shop"

      remove pat s = subRegex pat s ""

      clean s = foldl' (flip id) s
         [ takeWhile (/= ',')
         , map toLower
         , remove reRestaurant
         , remove reShop
         , filter isAlphaNum
         ]


csv :: [(Int, Int, Facility, Location)] -> IO ()
csv xs = do
   putStrLn "\"ncsl\",\"vcsl\",\"iname\",\"pname\",\"ivic\",\"pvic\",\"_id\""
   mapM_ csvOne xs


csvOne :: (Int, Int, Facility, Location) -> IO ()
csvOne (ncsl, vcsl, fac, loc) = do
   TF.print
      "{},{},\"{}\",\"{}\",\"{}\",\"{}\",\"{}\"\n"
      ( ncsl
      , vcsl
      , (T.pack $ name fac)
      , (locName loc)
      , (T.pack $ location fac)
      , (locVicinity loc)
      , (T.pack $ _id fac)
      )


{- Longest Common Substring function borrowed from Wikibooks:
   https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Longest_common_substring
-}
longestCommonSubstring :: (Eq a) => [a] -> [a] -> [a]
longestCommonSubstring xs ys = maximumBy (compare `on` length) . concat
   $ [f xs' ys | xs' <- tails xs] ++ [f xs ys' | ys' <- drop 1 $ tails ys]

   where f xs' ys' = scanl g [] $ zip xs' ys'
         g z (x, y) = if x == y then z ++ [x] else []
