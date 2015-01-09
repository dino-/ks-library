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
--import Debug.Trace ( trace )
import Text.Regex

import Ksdl.Facility
import Ksdl.Places


match :: (Facility, Locations) -> [(Bool, Int, Int, Facility, Location)]
match (fac, Locations locs) = markBest orderedByMatch
   where
      orderedByMatch = reverse . (sortBy cmpMatchValues)
         . (map (computeMatchValues fac)) $ locs


markBest :: [(Bool, (Int, a, b, c))] -> [(Bool, Int, a, b, c)]
markBest [] = []
markBest (best : rest) = markMatch best : map markNot rest
   where
      -- First elem is the one, put the Bool inside the final tuple
      markMatch (nameMatch, (ncsl, vcsl, fac, loc)) =
         ((isMatch ncsl nameMatch), ncsl, vcsl, fac, loc)
      -- All others must be marked False, they are not the one
      markNot (_, (ncsl, vcsl, fac, loc)) =
         (False, ncsl, vcsl, fac, loc)


cmpMatchValues :: (a, (Int, Int, b, c)) -> (a, (Int, Int, b, c))
   -> Ordering
cmpMatchValues (_, (ncsl1, vcsl1, _, _)) (_, (ncsl2, vcsl2, _, _)) =
   compare (ncsl1, vcsl1) (ncsl2, vcsl2)


-- Determine if this is really a match between the inspection and
-- Places data
isMatch :: Int -> Bool -> Bool
isMatch ncsl nameMatch
   -- If the common substring is greater than 5, it's a match (we hope)
   | ncsl > 4  = True
   -- If the common substring is <= 4, are the establishment names equal?
   | nameMatch = True
   -- None of the above, not a match
   | otherwise = False


computeMatchValues :: Facility -> Location
   -> (Bool, (Int, Int, Facility, Location))
computeMatchValues fac loc = (clNfac == clNloc, (ncsl, vcsl, fac, loc))
   where
      clNfac = clean nameFilters $ T.unpack $ name fac
      clNloc = clean nameFilters $ T.unpack $ locName loc
      ncsl = commonSubLength clNfac clNloc
      vcsl = commonSubLength (clean addrFilters $ T.unpack $ location fac)
         (clean addrFilters $ T.unpack $ locVicinity loc)

      commonSubLength target input =
         length $ longestCommonSubstring target input

      -- Patterns for some "stop" words
      reRestaurant = mkRegex "restaurant"
      reShop = mkRegex "shop"

      remove pat s = subRegex pat s ""

      clean filters s = foldl' (flip id) s filters

      nameFilters =
         [ map toLower
         , remove reRestaurant
         , remove reShop
         , filter isAlphaNum
         ]

      addrFilters = takeWhile (/= ',') : nameFilters


csv :: [(Bool, Int, Int, Facility, Location)] -> IO ()
csv xs = do
   putStrLn "\"pl\",\"ncsl\",\"vcsl\",\"iname\",\"pname\",\"ivic\",\"pvic\",\"_id\""
   mapM_ csvOne xs


csvOne :: (Bool, Int, Int, Facility, Location) -> IO ()
csvOne (isPlace, ncsl, vcsl, fac, loc) = do
   TF.print "\"{}\",{},{},\"{}\",\"{}\",\"{}\",\"{}\",\"{}\"\n"
      ( toStar isPlace
      , ncsl
      , vcsl
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


{- Longest Common Substring function borrowed from Wikibooks:
   https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Longest_common_substring
-}
longestCommonSubstring :: (Eq a) => [a] -> [a] -> [a]
longestCommonSubstring xs ys = maximumBy (compare `on` length) . concat
   $ [f xs' ys | xs' <- tails xs] ++ [f xs ys' | ys' <- drop 1 $ tails ys]

   where f xs' ys' = scanl g [] $ zip xs' ys'
         g z (x, y) = if x == y then z ++ [x] else []
