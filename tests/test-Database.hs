-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import System.Exit
import Test.Hspec

import qualified Mongo


main :: IO ()
main = hspec Mongo.tests
