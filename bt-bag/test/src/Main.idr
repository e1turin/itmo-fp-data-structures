module Main

import Hedgehog
import Util

import BinTreeBag

import PropTests
import UnitTests


main : IO ()
main = testAll [
    unitTests
  , propTests
  ]
