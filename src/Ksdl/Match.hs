-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts, KindSignatures, OverloadedStrings, RankNTypes #-}

module Ksdl.Match
   ( Match, csv, match )
   where

import Data.Char ( isDigit )
import Data.Maybe ( catMaybes )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format as TF

import Ksdl
import Ksdl.Facility
import Ksdl.Log
import Ksdl.Places ( Location (..) )


type Match = (Bool, Facility, Location)


match :: Facility -> [Location] -> Ksdl [Match]
match fac locs = do
   let ts = map combine locs
   let count = (sum . map bToI $ ts) :: Int

   when (count == 0) $ do
      throwError "ERROR Match: No Places result matches"

   liftIO $ do
      noticeM lerror "Matches:"
      mapM_ (noticeM lerror) $ catMaybes $ map matched ts

   when (count > 1) $ liftIO $ do
      warningM lerror "WARNING Match: More than one Places result matched"

   return ts

   where
      combine loc = ((isMatch (location fac) (locVicinity loc)),
         fac, loc)

      bToI (True,  _, _) = 1
      bToI (False, _, _) = 0

      matched :: Match -> Maybe String
      matched (True , _, loc) = Just . T.unpack . TL.toStrict $
         TF.format "{} | {}" ((locName loc), (locVicinity loc))
      matched (False, _, _  ) = Nothing


isMatch :: T.Text -> T.Text -> Bool
isMatch ivic pvic = prefix ivic == prefix pvic
   where prefix = T.takeWhile isDigit


csv :: [Match] -> IO ()
csv xs = do
   putStrLn "\"pl\",\"iname\",\"pname\",\"ivic\",\"pvic\",\"_id\""
   mapM_ csvOne xs


csvOne :: Match -> IO ()
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
