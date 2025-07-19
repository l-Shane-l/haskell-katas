import Test.Hspec

-- Import all test modules

import qualified Katas.Layer1_Fundamentals.PatternMatchingSpec
import qualified Katas.Layer2_Workhorse.ListLibraryDrillsSpec
import qualified Katas.Layer3_Toolkit.DataStructuresSpec
import qualified Katas.Layer4_Apex.AbstractionsSpec

main :: IO ()
main = hspec $ do
  -- Comment / uncomment to control which tests run
  Katas.Layer1_Fundamentals.PatternMatchingSpec.spec

-- Katas.Layer2_Workhorse.ListLibraryDrillsSpec.spec
-- Katas.Layer3_Toolkit.DataStructuresSpec.spec
-- Katas.Layer4_Apex.AbstractionsSpec.spec
