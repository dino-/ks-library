-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Ksdl
   ( Env (..), Ksdl, runKsdl

   -- Re-exporting
   , asks, liftIO, throwError, when
   )
   where

import Control.Monad.Reader
import Control.Monad.Error

import Ksdl.Config
import Ksdl.Inspection


data Env = Env
   { getConfig :: Config
   , getInspection :: Inspection
   }

type Ksdl a = ReaderT Env (ErrorT String IO) a

runKsdl :: Env -> Ksdl a -> IO (Either String a)
runKsdl env ev = runErrorT (runReaderT ev env)
