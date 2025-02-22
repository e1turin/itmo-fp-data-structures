module PropTests

import Hedgehog

import BinTreeBag



treeBagGen : Ord a => Hedgehog.Range Nat -> Gen a -> Gen (BinTree (Bag a))
treeBagGen range gen = list range gen |> map binTreeBagFromList

treeIntBagGen : Gen (BinTree (Bag Int))
treeIntBagGen = treeBagGen (linear 1 30) (int (linear 0 10))

propOpIsAssoc : Property
propOpIsAssoc = property $ do
  t1 <- forAll treeIntBagGen
  t2 <- forAll treeIntBagGen
  t3 <- forAll treeIntBagGen
  t1 <+> (t2 <+> t3) === (t1 <+> t2) <+> t3

export
propTests : List Group
propTests = [ MkGroup "Monoid properties" [
                ("associativity", propOpIsAssoc)
              ]
            ]


