-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts, KindSignatures, OverloadedStrings, RankNTypes #-}

module Ksdl.Match
   ( Match, csv, match )
   where

import Control.Monad.Error
import Data.Char ( isDigit )
import Data.List ( intercalate )
import qualified Data.Text as T
import qualified Data.Text.Format as TF
--import Debug.Trace ( trace )
import Text.Printf ( printf )

import Ksdl.Facility
import Ksdl.Log ( line )
import Ksdl.Places ( Location (..) )


type Match = (Bool, Facility, Location)


match :: (MonadError String m, MonadIO m) =>
   Facility -> [Location] -> m [Match]
match fac locs = do
   let ts = map combine locs
   let count = (sum . map bToI $ ts) :: Int

   when (count == 0) $ err "No Places result matches" fac ts
   when (count > 1) $ err "More than one Places result matched" fac ts

   return ts

   where
      combine loc = ((isMatch (location fac) (locVicinity loc)),
         fac, loc)

      bToI (True,  _, _) = 1
      bToI (False, _, _) = 0


err :: forall (m :: * -> *) a.
   (MonadError String m) => String -> Facility -> [Match] -> m a
err msg fac ts = throwError $ printf "%s\nMatch error: %s\n%s\n%s"
   line msg (show fac) (intercalate "\n" . map extractMatch $ ts)

   where extractMatch (_, _, loc) = show loc


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
