-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Data.Maybe ( listToMaybe )
import System.Environment ( getArgs )
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )

import Ksdl.Source.Common
import qualified Ksdl.Source.NcWake as NcWake


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   (source : dir : limitL) <- getArgs

   let pageLimit = read `fmap` (listToMaybe limitL)

   (downloader source) dir pageLimit


downloader :: String -> Downloader
downloader "NcWake" = NcWake.download
downloader _ = undefined
