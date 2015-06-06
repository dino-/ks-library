-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Locate.Locate
   ( Env (..), KSDL, runKSDL

   -- Re-exporting
   , asks, liftIO, local, throwError, when
   )
   where

import Control.Monad.Reader
import Control.Monad.Except

import KS.Inspection
import KS.Locate.Config


data Env = Env
   { getConfig :: Config
   , getIdInspection :: IdInspection
   }

type KSDL a = ReaderT Env (ExceptT String IO) a

runKSDL :: Env -> KSDL a -> IO (Either String a)
runKSDL env ev = runExceptT (runReaderT ev env)
