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
import Ksdl.Inspection
import Ksdl.Log
import Ksdl.Places ( Location (..) )


type Match = (Bool, Inspection, Location)


match :: Inspection -> [Location] -> Ksdl [Match]
match insp locs = do
   let ts = map combine locs
   let count = (sum . map bToI $ ts) :: Int

   when (count == 0) $ do
      throwError "ERROR Match: No Places result matches"

   liftIO $ do
      noticeM lname "Matches:"
      mapM_ (noticeM lname) $ catMaybes $ map matched ts

   when (count > 1) $ liftIO $ do
      warningM lname "WARNING Match: More than one Places result matched"

   return ts

   where
      combine loc = ((isMatch (location insp) (locVicinity loc)),
         insp, loc)

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
csvOne (isPlace, insp, loc) = do
   TF.print "\"{}\",\"{}\",\"{}\",\"{}\",\"{}\",\"{}\"\n"
      ( toStar isPlace
      , name insp
      , locName loc
      , location insp
      , locVicinity loc
      , _id insp
      )

   where
      toStar :: Bool -> T.Text
      toStar True  = "*"
      toStar False = ""
