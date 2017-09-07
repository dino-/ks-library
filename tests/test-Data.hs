-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Test.Hspec

import qualified BSON
import qualified Common
import qualified Inspection


main :: IO ()
main = hspec $ do
   BSON.tests
   Common.tests
   Inspection.tests
