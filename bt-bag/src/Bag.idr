module Bag

import BinTree

-- TODO: add constraint to Bag.count
-- Positive : {v: Nat} -> NonZero v => Type
-- Positive = Nat

public export
record Bag t where
  constructor MkBag
  value : t
  count : Nat

%name Bag b

--
-- Interfaces for data structures
--

export
Functor Bag where
  map f b = { value $= f } b

--
-- Ordering
--

export
Eq t => Eq (Bag t) where
  (MkBag v1 c1) == (MkBag v2 c2) = v1 == v2 && c1 == c2


export
(Ord t) => Ord (Bag t) where
  -- how we can order pairs properly?
  -- b1 < b2 = b1.value < b2.value
  -- b1 <= b2 = b1.value <= b2.value
  -- b1 > b2 = b1.value > b2.value
  -- b1 >= b2 = b1.value >= b2.value
  compare b1 b2 =
    case compare b1.value b2.value of
      LT => LT
      EQ => compare b1.count b2.count
      GT => GT

--
-- Algebraic structures
--

||| Adds all values from given bag to multiset.
export
move : Ord t => Bag t -> BinTree (Bag t) -> BinTree (Bag t)
move (MkBag _ 0) tree = tree
move bag Empty = Leaf bag
move x tree@(Node b left right) =
  case compare x.value b.value of
    LT => Node b (move x left) right
    EQ => Node ({ count $= (+ x.count) } b) left right
    GT => Node b left (move x right)

||| Add value to multiset.
export
put : Ord t => t -> BinTree (Bag t) -> BinTree (Bag t)
put x tree = move (MkBag x 1) tree

||| Implementation of Semigroup for TreeBag (Multiset).
||| Operation on it acts not like on common sets.
export
[BinTreeBagSemi]
Ord t => Semigroup (BinTree (Bag t)) where
  Empty <+> Empty = Empty
  Empty <+> node = node
  node <+> Empty = node
  t1 <+> t2 = foldr move t1 t2

--
-- Show implementation
--

export
Show t => Show (Bag t) where
  show (MkBag value count) = "{\{show value}: \{show count}}"

--
-- Other methods
--

export
drop : Ord t => t -> BinTree (Bag t) -> BinTree (Bag t)
drop x Empty = Empty
drop x (Node b tl tr) =
  case compare x b.value of
    LT => Node b (drop x tl) tr
    EQ => case b.count of
            0 => shiftLeft tl tr
            (S k) => Node ({ count := k } b) tl tr
    GT => Node b tl (drop x tr)


export
count : Ord t => t -> BinTree (Bag t) -> Nat
count v Empty = 0
count v (Node (MkBag value k) left right) =
  case compare v value of
    LT => count v left
    EQ => k
    GT => count v right


export
binTreeBagFromList : Ord t => List t -> BinTree (Bag t)
binTreeBagFromList = foldl (flip put) Empty

