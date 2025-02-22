module UnitTests

import Hedgehog
import Util

import BinTreeBag

unitInsert : UnitTest
unitInsert = unitTest $ do
  let t1 = binTreeBagFromList [1, 2, 3, 4, 5, 6, 7]
  let t2 = put 1 t1
  let t3 = drop 1 t2
  t1 === t3

export
unitTests : List Group
unitTests =
  [ MkGroup "Unit tests of BinTree Bag" [
      ("unit insert", unitInsert)
    ]
  ]

