module Main

import BinTreeBag

tree1 : BinTree (Bag Int)
tree1 = binTreeBagFromList [3, 2, 4, 5]

tree2 : BinTree (Bag Int)
tree2 = binTreeBagFromList [3, 4, 1, 5, 9]

kek : Monoid (BinTree (Bag Int)) => BinTree (Bag Int)
kek = neutral

main : IO ()
main = do
  putStrLn (show tree1)
  putStrLn (show tree2)
  let tree3 = ((<+>) @{BinTreeBagSemi} tree1 tree2)
  putStrLn (show tree3)
  putStrLn (show kek)
  putStrLn (show (binTreeToList tree3))
  putStrLn (show $ size tree3)
