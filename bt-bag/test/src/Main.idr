module Main

import Hedgehog

import BinTreeBag

import PropTests
import UnitTests

unit : PropertyT () -> Property
unit = withTests 1 . MkProperty defaultConfig

unitInsert : Property
unitInsert = unit $ do
  let t1 = binTreeBagFromList [1, 2, 3, 4, 5, 6, 7]
  let t2 = put 1 t1
  let t3 = drop 1 t2
  t1 === t3

unitTests : List Group
unitTests = [ MkGroup "unit" [
                ("unit insert", unitInsert)
              ]
            ]

main : IO ()
main = test $ unitTests ++ propTests
