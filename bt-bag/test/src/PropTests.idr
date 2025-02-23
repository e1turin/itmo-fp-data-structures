module PropTests

import Hedgehog

import BinTree
import Bag

binTrees : Ord a => Hedgehog.Range Nat -> Gen a -> Gen (BinTree a)
binTrees range gen = list range gen |> map binTreeFromList

smallBinTreesOf : Ord a=> Gen a -> Gen (BinTree a)
smallBinTreesOf = binTrees (linear 1 30)

binTreesOfInt : Gen (BinTree Int)
binTreesOfInt = smallBinTreesOf (int (linear 0 10))

binTreeBags : Ord a => Hedgehog.Range Nat -> Gen a -> Gen (BinTree (Bag a))
binTreeBags range gen = list range gen |> map binTreeBagFromList

binTreeBagsOfInt : Gen (BinTree (Bag Int))
binTreeBagsOfInt = binTreeBags (linear 1 30) (int (linear 0 10))

(++) : Ord t => (t1, t2 : BinTree (Bag t)) -> BinTree (Bag t)
(++) = (<+>) @{BinTreeBagSemi}

propEquality : Property
propEquality = property $ do
  tree <- forAll binTreeBagsOfInt
  let orig = BinTree.toList tree
  tree === binTreeFromList (reverse orig)


propOpIsAssoc : Property
propOpIsAssoc = property $ do
  t1 <- forAll binTreeBagsOfInt
  t2 <- forAll binTreeBagsOfInt
  t3 <- forAll binTreeBagsOfInt
  t1 ++ (t2 ++ t3) === (t1 ++ t2) ++ t3


propHasNeutral : Monoid (BinTree (Bag Int)) => Property
propHasNeutral = property $ do
  t <- forAll binTreeBagsOfInt
  let n := neutral
  t ++ n === t
  n ++ t === t


propOpIsCommut : Property
propOpIsCommut = property $ do
  t1 <- forAll binTreeBagsOfInt
  t2 <- forAll binTreeBagsOfInt
  t1 ++ t2 === t2 ++ t1


propFoldOrder : Property
propFoldOrder = property $ do
  tree <- forAll binTreesOfInt

  let rightSum = foldr (+) 0 tree
  let leftSum = foldl (+) 0 tree
  rightSum === leftSum

  let rightList = foldr (::) (the (List Int) []) tree
  let leftList = foldl (flip (::)) (the (List Int) []) tree

  rightList === reverse leftList


export
propTests : List Group
propTests =
  [ MkGroup "Properties of BinTree"
    [ ("Equality", propEquality)
    , ("Fold left and right order", propFoldOrder)
    ]
  , MkGroup "Monoid properties of BinTree Bag"
    [ ("Associativity", propOpIsAssoc)
    , ("Neutral element", propHasNeutral)
    ]
  , MkGroup "Additional properties of BinTree Bag"
    [ ("Commutativity", propOpIsCommut)
    ]
  ]


