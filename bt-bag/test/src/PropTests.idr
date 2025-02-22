module PropTests

import Hedgehog

import BinTreeBag

treeBag : Ord a => Hedgehog.Range Nat -> Gen a -> Gen (BinTree (Bag a))
treeBag range gen = list range gen |> map binTreeBagFromList

treeBagInt : Gen (BinTree (Bag Int))
treeBagInt = treeBag (linear 1 30) (int (linear 0 10))

(++) : Ord t => (t1, t2 : BinTree (Bag t)) -> BinTree (Bag t)
(++) = (<+>) @{BinTreeBagSemi}

propOpIsAssoc : Property
propOpIsAssoc = property $ do
  t1 <- forAll treeBagInt
  t2 <- forAll treeBagInt
  t3 <- forAll treeBagInt
  t1 ++ (t2 ++ t3) === (t1 ++ t2) ++ t3

propHasNeutral : Monoid (BinTree (Bag Int)) => Property
propHasNeutral = property $ do
  t <- forAll treeBagInt
  let n := neutral
  t ++ n === t
  n ++ t === t

propOpIsCommut : Property
propOpIsCommut = property $ do
  t1 <- forAll treeBagInt
  t2 <- forAll treeBagInt
  t1 ++ t2 === t2 ++ t1

export
propTests : List Group
propTests =
  [ MkGroup "Monoid properties of BinTree Bag" [
      ("Associativity", propOpIsAssoc)
    , ("Neutral element", propHasNeutral)
    ]
  , MkGroup "Additional properties of BinTree Bag" [
      ("Commutativity", propOpIsCommut)
    ]
  ]


