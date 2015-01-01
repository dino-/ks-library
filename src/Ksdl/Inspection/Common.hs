-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Ksdl.Inspection.Common
   where

import System.FilePath ()


type Downloader = FilePath -> Maybe Int -> IO ()
