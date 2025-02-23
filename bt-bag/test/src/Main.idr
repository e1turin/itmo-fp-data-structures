module Main

import Hedgehog
import Util

import PropTests
import BinTreeUnitTests
import BagUnitTests


main : IO ()
main = testAll
  [ binTreeUnitTests
  , bagUnitTests
  , propTests
  ]
