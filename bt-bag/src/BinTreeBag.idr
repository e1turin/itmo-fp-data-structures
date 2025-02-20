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

export
record Bag t where
  constructor MkBag
  value : t
  count : Nat

%name Bag b

export
insert : Ord a => a -> BinTree a -> BinTree a
insert x Empty = Leaf x
insert x node@(Node value left right) =
  case compare x value of
    LT => Node value (insert x left) right
    EQ => node
    GT => Node value left (insert x right)

export
put : Ord t => t -> BinTree (Bag t) -> BinTree (Bag t)
put x Empty = Leaf (MkBag x 1)
put x (Node b tl tr) =
  case compare x b.value of
    LT => Node b (put x tl) tr
    EQ => Node ({ count $= (+ 1) } b) tl tr
    GT => Node b tl (put x tr)

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
pick : Ord t => t -> BinTree (Bag t) -> BinTree (Bag t)
pick x Empty = Empty
pick x (Node b tl tr) =
  case compare x b.value of
    LT => Node b (pick x tl) tr
    EQ => case b.count of
              0 => shiftLess tl tr
              (S k) => Node ({ count := k } b) tl tr
    GT => Node b tl (pick x tr)

export
Functor Bag where
  map f b@(MkBag v c) = { value $= f } b

export
Functor BinTree where
  map f Empty = Empty
  map f (Node value left right) = Node (f value) (map f left) (map f right)

export
Foldable BinTree where
  foldr f acc Empty = acc
  foldr f acc (Node value left right) =
    let leftFold = foldr f acc left
        rightFold = foldr f leftFold right
      in f value rightFold

  foldl f acc Empty = acc
  foldl f acc (Node value left right) =
    case (left, right) of
      (Empty, Empty) => ?strip_1
      (Empty, (Node x l r)) => ?strip_2
      ((Node x l r), Empty) => ?strip_3
      ((Node x l1 r1), (Node y l2 r2)) => ?strip_4

export
Eq t => Eq (Bag t) where
  (MkBag v1 c1) == (MkBag v2 c2) = v1 == v2 && c1 == c2

export
Eq (Bag t) => Eq (BinTree (Bag t)) where
  Empty == Empty = True
  (Node b1 tl1 tr1) == (Node b2 tl2 tr2) = b1 == b2 && tl1 == tl2 && tr1 == tr2
  _ == _ = False

export
(Eq (Bag t), Ord t) => Ord (Bag t) where
  b1 < b2 = case compare b1.value b2.value of
              LT => True
              EQ => b1.count < b2.count
              GT => False

export
Ord t => Semigroup (BinTree (Bag t)) where
  Empty <+> Empty = Empty
  Empty <+> node = node
  node <+> Empty = node
  t1@(Node b1 tl1 tr1) <+> t2@(Node b2 tl2 tr2) =
    case compare b1.value b2.value of
      LT => Node b2 (t1 <+> tl2) tr2
      EQ => Node ({ count $= (+ b2.count) } b1) (tl1 <+> tl2) (tr1 <+> tr2)
      GT => Node b1 (tl1 <+> t2) tr1

export
Ord t => Monoid (BinTree (Bag t)) where
  neutral = Empty

filter : Ord t => BinTree t -> (f : t -> Bool) -> BinTree t
filter Empty f = Empty
filter node f = foldr go Empty node
  where
    go : t -> BinTree t -> BinTree t
    go et acc = case f et of
                     False => acc
                     True => insert et acc


