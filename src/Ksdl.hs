-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Ksdl
   ( Ksdl, runKsdl

   -- Re-exporting
   , asks, liftIO, throwError, when
   )
   where

import Control.Monad.Reader
import Control.Monad.Error

import Ksdl.Config


type Ksdl a = ReaderT Config (ErrorT String IO) a

runKsdl :: Config -> Ksdl a -> IO (Either String a)
runKsdl env ev = runErrorT (runReaderT ev env)
