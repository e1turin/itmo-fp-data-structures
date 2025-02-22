module BinTreeBag

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
    let leftFold = foldr f acc left
        midFold = f value leftFold
        rightFold = foldr f midFold right
      in rightFold

  -- foldl by default impl

--
-- Ordering
--

export
Eq t => Eq (Bag t) where
  (MkBag v1 c1) == (MkBag v2 c2) = v1 == v2 && c1 == c2

-- TODO: Fix equality as sets
export
Eq (Bag t) => Eq (BinTree (Bag t)) where
  Empty == Empty = True
  (Node b1 tl1 tr1) == (Node b2 tl2 tr2) = b1 == b2 && tl1 == tl2 && tr1 == tr2
  _ == _ = False

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


||| Puts value in bag as in multiset.
export
put : Ord t => t -> BinTree (Bag t) -> BinTree (Bag t)
put x Empty = Leaf (MkBag x 1)
put x (Node b tl tr) =
  case compare x b.value of
    LT => Node b (put x tl) tr
    EQ => Node ({ count $= (+ 1) } b) tl tr
    GT => Node b tl (put x tr)

||| Implementation of Semigroup for TreeBag (Multiset).
||| Operation on it acts not like on common sets.
export
[BinTreeBagSemi]
Ord t => Semigroup (BinTree (Bag t)) where
  Empty <+> Empty = Empty
  Empty <+> node = node
  node <+> Empty = node
  t1 <+> t2 = merge t1 t2 where
    merge : BinTree (Bag t) -> BinTree (Bag t) -> BinTree (Bag t)
    merge = foldr (\b, acc => put b.value acc)

export
Semigroup (BinTree t) => Monoid (BinTree t) where
  neutral = Empty

--
-- Show implementation
--

export
Show t => Show (Bag t) where
  show (MkBag value count) = "(\{show value}: \{show count})"

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
binTreeBagFromList : Ord t => List t -> BinTree (Bag t)
binTreeBagFromList xs = foldl (flip put) Empty xs

export
binTreeToList : Ord t => BinTree t -> List t
binTreeToList tree = foldl (flip (::)) [] tree

