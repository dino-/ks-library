-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module Database.Mongo.Util
   ( lastStatus
   , parseLastError
   )
   where

import Control.Monad.Trans ( MonadIO )
import Data.Bson ( Document, (=:), at )
import Database.MongoDB ( Action, runCommand )
import qualified Data.Text as T
import Text.Printf ( printf )


{- This needs to be called immediately after the MongoDB operation
   you want status for
-}
lastStatus :: (MonadIO m) => Action m (Either String String)
lastStatus = parseLastError <$> runCommand [ "getLastError" =: (1::Int) ]


parseLastError :: Document -> Either String String
parseLastError errdoc = lastErrStatus >> atE "err"
   where
      lastErrStatus :: Either String String
      lastErrStatus = case at "ok" errdoc of
         1.0   -> Right "getLastError successful"
         c     -> Left $ printf "getLastError FAILED: ok: %f" (c :: Double)

      atE :: T.Text -> Either String String
      atE key = case at key errdoc of
         Just msg -> Left $ printf "operation FAILED: %s" (T.unpack msg)
         Nothing  -> Right "operation successful"
