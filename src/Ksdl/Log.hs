-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Ksdl.Log
   ( initLogging, lerror
   -- Re-exported from System.Log
   , Priority (..), debugM, infoM, noticeM, warningM, errorM
   , criticalM, alertM, emergencyM
   )
   where

import System.IO ( stderr )
import System.Log.Formatter
import System.Log.Handler ( setFormatter )
import System.Log.Handler.Simple ( streamHandler )
import System.Log.Logger


lerror :: String
lerror = "error"


{- Set up logging
-}
initLogging :: Priority -> IO ()
initLogging logPriority = do
   h <- streamHandler stderr logPriority >>= \lh -> return $
      setFormatter lh (simpleLogFormatter "[$time : $prio] $msg")
   updateGlobalLogger lerror $ setHandlers [h]
   updateGlobalLogger lerror $ setLevel logPriority
