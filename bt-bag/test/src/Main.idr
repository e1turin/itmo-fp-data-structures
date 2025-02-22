module Main

import Hedgehog
import Util

import BinTreeBag

import PropTests
import BinTreeUnitTests
import BagUnitTests


main : IO ()
main = testAll [
    binTreeUnitTests
  , bagUnitTests
  , propTests
  ]
