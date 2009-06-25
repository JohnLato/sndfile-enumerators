import Prelude as P

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import QCUtils
import Sound.Iteratee
import Data.Int
import Data.Int.Int24
import Data.Word
import Data.Word.Word24

-- ----------------------------------------

-- ----------------------------------------
-- tests
tests = [
  ]

-- ----------------------------------------
-- entry point

main = defaultMain tests
