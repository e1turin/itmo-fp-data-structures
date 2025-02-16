module Main

import BinTreeBag
import Hedgehog

prop : Property
prop = property $ do
    let t1 = insert 1 Empty
    let t2 = insert 1 t1
    let t3 = remove 1 t2
    assert (t1 == t3)

main : IO ()
main = test
  [ MkGroup "basic test" 
    [ ("prop", prop)
    ]
  ]