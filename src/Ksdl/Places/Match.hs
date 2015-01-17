-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module Ksdl.Places.Match
   ( Match, match )
   where

import Data.Char ( isDigit )
import Data.Maybe ( catMaybes )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format as TF

import Ksdl
import qualified Ksdl.Inspection as I
import Ksdl.Log
import qualified Ksdl.Places.Place as P


type Match = (I.Inspection, P.Place)

type MatchInternal = (Bool, Match)


match :: I.Inspection -> [P.Place] -> Ksdl Match
match insp ps = do
   let mis = map combine ps
   let count = (sum . map bToI $ mis) :: Int

   when (count == 0) $ do
      throwError "ERROR Match: No Places result matches"

   liftIO $ do
      noticeM lname "Matches:"
      mapM_ (noticeM lname) $ catMaybes $ map matched mis

   when (count > 1) $ liftIO $ do
      warningM lname "WARNING Match: More than one Places result matched"

   return . head . catMaybes . map positiveMatch $ mis

   where
      combine :: P.Place -> MatchInternal
      combine pl = ((isMatch (I.addr insp) (P.vicinity pl)),
         (insp, pl))

      bToI :: MatchInternal -> Int
      bToI (True,  (_, _)) = 1
      bToI (False, (_, _)) = 0

      matched :: MatchInternal -> Maybe String
      matched (True , (_, pl)) = Just . T.unpack . TL.toStrict $
         TF.format "{} | {}" ((P.name pl), (P.vicinity pl))
      matched (False, (_, _ )) = Nothing

      positiveMatch :: MatchInternal -> Maybe Match
      positiveMatch (True , m) = Just m
      positiveMatch (False, _) = Nothing


isMatch :: T.Text -> T.Text -> Bool
isMatch ivic pvic = prefix ivic == prefix pvic
   where prefix = T.takeWhile isDigit
