module BinTreeUnitTests

import Hedgehog
import Util

import BinTree


testEquality : UnitTest
testEquality = unitTest $ do
  let tree = Node 3 (Node 2 (Leaf 1) Empty)
                (Node 5 (Leaf 4) (Leaf 6))
  let tree' = Node 5 (Node 3 (Node 2 (Leaf 1) Empty)
                             (Leaf 4))
                     (Leaf 6)
  let orig = [1, 2, 3, 4, 5, 6]
  let shuf = [5, 3, 4, 1, 6, 2]

  tree === tree'
  tree === binTreeFromList orig
  tree === binTreeFromList shuf


testToList : UnitTest
testToList = unitTest $ do
  let tree = Node 3 (Node 2 (Leaf 1) Empty)
                (Node 5 (Leaf 4) (Leaf 6))
  let tree' = Node 5 (Node 3 (Node 2 (Leaf 1) Empty)
                             (Leaf 4))
                     (Leaf 6)
  let list = BinTree.toList tree
  let list' = BinTree.toList tree'
  list === [1, 2, 3, 4, 5, 6]
  list === list'


testBinTreeFromList : UnitTest
testBinTreeFromList = unitTest $ do
  let orig = [1, 2, 3, 4, 5, 6]
  let shuf = [4, 6, 1, 2, 5, 3]

  let fromList = binTreeFromList orig
  let fromList' = binTreeFromList shuf

  fromList === Node 3 (Node 2 (Leaf 1) Empty)
                      (Node 5 (Leaf 4) (Leaf 6))
  fromList === fromList'


testInsertEmpty : UnitTest
testInsertEmpty = unitTest $ do
  let tree = the (BinTree Int) Empty
  let tree' = insert 1 tree
  tree' === Node 1 Empty Empty
  tree' === Leaf 1


testInsertNonEmpty : UnitTest
testInsertNonEmpty = unitTest $ do
  let tree = Node 3 (Leaf 2)
                    (Leaf 4)

  let tree1 = insert 1 tree
  tree1 === Node 3 (Node 2 (Leaf 1)
                            Empty)
                   (Leaf 4)

  let tree2 = insert 2 tree
  tree2 === tree

  let tree3 = insert 3 tree
  tree3 === tree

  let tree4 = insert 4 tree
  tree4 === tree

  let tree5 = insert 5 tree
  tree5 === Node 3 (Leaf 2)
                   (Node 4 Empty
                          (Leaf 5))


testRemoveEmpty : UnitTest
testRemoveEmpty = unitTest $ do
  let tree = the (BinTree Int) Empty
  let tree' = remove 42 tree
  tree' === tree


testRemoveNonEmpty : UnitTest
testRemoveNonEmpty = unitTest $ do
  let tree = Node 3 (Leaf 2)
                    (Leaf 4)

  let tree2 = remove 2 tree
  tree2 === Node 3 Empty (Leaf 4)
  tree2 === binTreeFromList [3, 4]

  let tree3 = remove 3 tree
  tree3 === Node 4 (Leaf 2) Empty
  tree3 === binTreeFromList [2, 4]


export
binTreeUnitTests : List Group
binTreeUnitTests =
  [ MkGroup "Unit tests of BinTree" [
      ("BinTree equality", testEquality)
    , ("BinTree to list", testToList)
    , ("BinTree from list", testBinTreeFromList)
    , ("Insert to empty tree", testInsertEmpty)
    , ("Insert to non empty tree", testInsertNonEmpty)
    , ("Remove from empty tree", testRemoveEmpty)
    , ("Remove from non empty tree", testRemoveNonEmpty)
    ]
  ]

