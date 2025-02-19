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
insert : Ord t => t -> BinTree (Bag t) -> BinTree (Bag t)
insert x Empty = Leaf (MkBag x 1)
insert x (Node b tl tr) = 
  case compare x b.value of
    LT => Node b (insert x tl) tr
    EQ => Node ({ count $= (+ 1) } b) tl tr
    GT => Node b tl (insert x tr)

export
remove : Ord t => t -> BinTree (Bag t) -> BinTree (Bag t)
remove x Empty = Empty
remove x (Node b tl tr) = 
  case compare x b.value of
    LT => Node b (remove x tl) tr
    EQ => case b.count of
              0 => shiftLess tl tr
              (S k) => Node ({ count := k } b) tl tr
    GT => Node b tl (remove x tr)
  where 
    shiftLess : BinTree (Bag t) -> BinTree (Bag t) -> BinTree (Bag t)
    shiftLess Empty node = node
    shiftLess node Empty = node
    shiftLess less (Node b tl tr) = Node b (shiftLess less tl) tr

export
Eq t => Eq (Bag t) where
  (MkBag v1 c1) == (MkBag v2 c2) = v1 == v2 && c1 == c2

export
Eq (Bag t) => Eq (BinTree (Bag t)) where
  Empty == Empty = True
  (Node b1 tl1 tr1) == (Node b2 tl2 tr2) = b1 == b2 && tl1 == tl2 && tr1 == tr2
  _ == _ = False

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

-- TODO: proof @f is isomophic
Functor BinTree where
  map f Empty = Empty
  map f (Node value left right) = Node (f value) (map f left) (map f right)


