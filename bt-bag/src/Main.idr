module Main

import BinTreeBag

tree1 : BinTree (Bag Int)
tree1 = binTreeBagFromList [3, 2, 4, 5]

tree2 : BinTree (Bag Int)
tree2 = binTreeBagFromList [3, 4, 1, 5, 9]

tree3 : BinTree (Bag Int)
tree3 = binTreeBagFromList [1, 5, 9]

kek : Monoid (BinTree (Bag Int)) => BinTree (Bag Int)
kek = neutral

(++) : Ord t => (t1, t2 : BinTree (Bag t)) -> BinTree (Bag t)
(++) = (<+>) @{BinTreeBagSemi}

list : Ord t => BinTree t -> List t
list = BinTreeBag.toList

main : IO ()
main = do
  printLn ("1: " ++ show tree1)
  printLn ("2: " ++ show tree2)
  printLn ("3: " ++ show tree3)
  printLn ()
  let tree12 = ((++) tree1 tree2)
  printLn ("12: " ++ show tree12)
  printLn ()
  let tree23 = ((++) tree2 tree3)
  printLn ("23: " ++ show tree23)
  printLn ()
  let tree123 = ((++) tree12 tree3)
  printLn ("123: " ++ show tree123)
  printLn ()
  let tree123' = ((++) tree1 tree23)
  printLn ("123': " ++ show tree123')

