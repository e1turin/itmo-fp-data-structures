module BinTreeBag

import Data.Nat

%default total

public export
data BinTree : (t : Type) -> Type where
  Empty : BinTree t
  Node : (value : t)
      -> (left, right : BinTree t)
      -> BinTree t

%name BinTree t

export
Leaf : t -> BinTree t
Leaf value = Node value Empty Empty

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

export
Functor BinTree where
  map f Empty = Empty
  map f (Node value left right) = Node (f value) (map f left) (map f right)

export
Foldable BinTree where
  foldr f acc Empty = acc
  foldr f acc (Node value left right) =
    let rightFold = foldr f acc right
        midFold = f value rightFold
        leftFold = foldr f midFold left
      in leftFold

  -- foldl by default impl

export
toList : Ord t => BinTree t -> List t
toList = foldr (::) []

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

export
Ord t => Eq (BinTree t) where
  Empty == Empty = True
  Empty == (Node _ _ _) = False
  (Node _ _ _) == Empty = False
  t1 == t2 = BinTreeBag.toList t1 == BinTreeBag.toList t2

-- export
-- Ord t => Eq (BinTree (Bag t)) where
--   Empty == Empty = True
--   Empty == (Node _ _ _) = False
--   (Node _ _ _) == Empty = False
--   t1 == t2 = BinTreeBag.toList t1 == BinTreeBag.toList t2

--
-- Algebraic structures
--

||| Add value in tree as in set.
export
insert : Ord a => a -> BinTree a -> BinTree a
insert x Empty = Leaf x
insert x (Node value left right) =
  case compare x value of
    LT => Node value (insert x left) right
    EQ => Node x left right
    GT => Node value left (insert x right)

export
Ord t => Semigroup (BinTree t) where
  Empty <+> Empty = Empty
  Empty <+> node = node
  node <+> Empty = node
  t1 <+> t2 = foldr insert t1 t2

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

export
Semigroup (BinTree t) => Monoid (BinTree t) where
  neutral = Empty

--
-- Show implementation
--

export
Show t => Show (Bag t) where
  show (MkBag value count) = "{\{show value}: \{show count}}"

export
Show t => Show (BinTree t) where
  show Empty = ""
  show (Node value Empty Empty) = "[\{show value}]"
  show (Node value left Empty) = "[\{show left} \{show value}]"
  show (Node value Empty right) = "[\{show value} \{show right}]"
  show (Node value left right) = "[\{show left} \{show value} \{show right}]"

--
-- Other methods
--

||| Utility function. Shifts left tree to right tree's left branch
shiftLess : BinTree b -> BinTree b -> BinTree b
shiftLess Empty node = node
shiftLess node Empty = node
shiftLess less (Node b tl tr) = Node b (shiftLess less tl) tr

export
remove : Ord a => a -> BinTree a -> BinTree a
remove x Empty = Empty
remove x (Node value left right) =
  case compare x value of
    LT => Node value (remove x left) right
    EQ => shiftLess left right
    GT => Node value left (remove x right)

export
drop : Ord t => t -> BinTree (Bag t) -> BinTree (Bag t)
drop x Empty = Empty
drop x (Node b tl tr) =
  case compare x b.value of
    LT => Node b (drop x tl) tr
    EQ => case b.count of
            0 => shiftLess tl tr
            (S k) => Node ({ count := k } b) tl tr
    GT => Node b tl (drop x tr)

export
filter : Ord t => BinTree t -> (f : t -> Bool) -> BinTree t
filter Empty f = Empty
filter node f = foldr pick Empty node where
  pick : t -> BinTree t -> BinTree t
  pick et acc = case f et of
                False => acc
                True => insert et acc

export
find : Ord t => t -> BinTree t -> BinTree t
find v Empty = Empty
find v node@(Node value left right) =
  case compare v value of
    LT => find v left
    EQ => node
    GT => find v right

export
count : Ord t => t -> BinTree (Bag t) -> Nat
count v Empty = 0
count v (Node (MkBag value k) left right) =
  case compare v value of
    LT => count v left
    EQ => k
    GT => count v right

export
size : BinTree t -> Nat
size tree = foldr (\_, acc => 1 + acc) 0 tree

export
binTreeFromList : Ord t => List t -> BinTree t
binTreeFromList = foldl (flip insert) Empty

export
binTreeBagFromList : Ord t => List t -> BinTree (Bag t)
binTreeBagFromList = foldl (flip put) Empty

