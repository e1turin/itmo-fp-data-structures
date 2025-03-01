module PropTests

import Hedgehog

import BinTree
import Bag

binTrees : {auto _ : Ord a}
        -> Hedgehog.Range Nat
        -> Gen a
        -> Gen (BinTree a)
binTrees range gen = list range gen |> map binTreeFromList

smallBinTreesOf : {auto _ : Ord a} -> Gen a -> Gen (BinTree a)
smallBinTreesOf = binTrees (linear 1 30)

binTreesOfInt : Gen (BinTree Int)
binTreesOfInt = smallBinTreesOf (int (linear 0 10))

binTreeBags : {auto _ : Ord a}
          -> Hedgehog.Range Nat
          -> Gen a
          -> Gen (BinTree (Bag a))
binTreeBags range gen = list range gen |> map binTreeBagFromList

binTreeBagsOfInt : Gen (BinTree (Bag Int))
binTreeBagsOfInt = binTreeBags (linear 1 30) (int (linear 0 10))

-- alias
(++) : {auto _ : Ord t}
    -> (t1, t2 : BinTree (Bag t))
    -> BinTree (Bag t)
(++) = (<+>) @{BinTreeBagSemi}

--
-- BinTree Properties
--

propEqualityTree : Property
propEqualityTree = property $ do
  tree <- forAll binTreesOfInt
  let orig = BinTree.toList tree
  tree === binTreeFromList (reverse orig)


propFoldOrderTree : Property
propFoldOrderTree = property $ do
  tree <- forAll binTreesOfInt

  let rightSum = foldr (+) 0 tree
  let leftSum = foldl (+) 0 tree
  rightSum === leftSum

  let rightList = foldr (::) (the (List Int) []) tree
  let leftList = foldl (flip (::)) (the (List Int) []) tree

  rightList === reverse leftList


propOpIsAssocTree : Property
propOpIsAssocTree = property $ do
  t1 <- forAll binTreesOfInt
  t2 <- forAll binTreesOfInt
  t3 <- forAll binTreesOfInt
  t1 <+> (t2 <+> t3) === (t1 <+> t2) <+> t3


propHasNeutralTree : {auto _ : Monoid (BinTree Int)}
                  -> Property
propHasNeutralTree = property $ do
  t <- forAll binTreesOfInt
  let n := neutral
  t <+> n === t
  n <+> t === t


propOpIsCommutTree : Property
propOpIsCommutTree = property $ do
  t1 <- forAll binTreesOfInt
  t2 <- forAll binTreesOfInt
  t1 <+> t2 === t2 <+> t1

--
-- BinTree Bag Properties
--

propEqualityBag : Property
propEqualityBag = property $ do
  tree <- forAll binTreeBagsOfInt
  let orig = BinTree.toList tree
  tree === binTreeFromList (reverse orig)


propOpIsAssocBag : Property
propOpIsAssocBag = property $ do
  t1 <- forAll binTreeBagsOfInt
  t2 <- forAll binTreeBagsOfInt
  t3 <- forAll binTreeBagsOfInt
  t1 ++ (t2 ++ t3) === (t1 ++ t2) ++ t3


propHasNeutralBag : {auto _ : Monoid (BinTree (Bag Int))}
                 -> Property
propHasNeutralBag = property $ do
  t <- forAll binTreeBagsOfInt
  let n := neutral
  t ++ n === t
  n ++ t === t


propOpIsCommutBag : Property
propOpIsCommutBag = property $ do
  t1 <- forAll binTreeBagsOfInt
  t2 <- forAll binTreeBagsOfInt
  t1 ++ t2 === t2 ++ t1


export
propTests : List Group
propTests =
  [ MkGroup "Properties of BinTree"
    [ ("Equality", propEqualityTree)
    , ("Fold left and right order", propFoldOrderTree)
    ]
  , MkGroup "Commutative Monoid properties of BinTree"
    [ ("Associativity", propOpIsAssocTree)
    , ("Neutral element", propHasNeutralTree)
    , ("Commutativity", propOpIsCommutTree)
    ]
  , MkGroup "Properties of BinTreeBag"
    [ ("Equality", propEqualityBag)
    ]
  , MkGroup "Commutative Monoid properties of BinTree Bag"
    [ ("Associativity", propOpIsAssocBag)
    , ("Neutral element", propHasNeutralBag)
    , ("Commutativity", propOpIsCommutBag)
    ]
  ]


