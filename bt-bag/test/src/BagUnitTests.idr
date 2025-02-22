module BagUnitTests

import Hedgehog
import Util

import BinTreeBag


testBinTreeBagFromList : UnitTest
testBinTreeBagFromList = unitTest $ do
  let orig = [1, 1, 1, 1, 2, 2, 2, 3, 3, 4]
  let shuf = [1, 2, 3, 4, 1, 1, 2, 3, 2, 1]
  let tree = binTreeBagFromList orig
  let tree' = binTreeBagFromList shuf

  tree === Node (MkBag 3 2) (Node (MkBag 2 3) (Leaf (MkBag 1 4))
                                               Empty)
                            (Leaf (MkBag 4 1))
  tree === tree'


testFillEmpty : UnitTest
testFillEmpty = unitTest $ do
  let tree = the (BinTree (Bag Int)) Empty

  let tree1 = put 1 tree
  tree1 === Leaf (MkBag 1 1)
  BinTreeBag.count 1 tree1 === 1

  let tree1' = move (MkBag 1 42) tree
  tree1' === Leaf (MkBag 1 42)
  BinTreeBag.count 1 tree1' === 42

  let tree10 = move (MkBag 1 0) tree
  tree10 === Empty
  BinTreeBag.count 1 tree10 === 0


testFillNonEmpty : UnitTest
testFillNonEmpty = unitTest $ do
  let orig = [1, 1, 1, 1, 2, 2, 2, 3, 3, 4]
  let tree = binTreeBagFromList orig

  -- existing value in bag
  let tree4 = put 4 tree
  tree4 === binTreeBagFromList (orig ++ [4])
  tree4 === binTreeBagFromList (4::orig)

  let tree44 = move (MkBag 4 4) tree
  tree44 === binTreeBagFromList (orig ++ [4, 4, 4, 4])

  -- non existing value in bag
  let tree5 = put 5 tree
  tree5 === binTreeBagFromList (orig ++ [5])
  tree5 === binTreeBagFromList (5::orig)

  let tree53 = move (MkBag 5 3) tree
  tree53 === binTreeBagFromList (orig ++ [5, 5, 5])


export
bagUnitTests : List Group
bagUnitTests =
  [ MkGroup "Unit tests of BinTree Bag" [
      ("BinTree Bag from list", testBinTreeBagFromList)
    , ("Fill empty tree with put & move", testFillEmpty)
    , ("Fill non empty tree with put & move", testFillNonEmpty)
    ]
  ]
