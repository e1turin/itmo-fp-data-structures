module BinTreeBag

%default total

public export
data BinTreeBag : (t : Type) -> Type where
  Empty : BinTreeBag t
  Node : BinTreeBag t 
      -> (entry : (t, Nat))
      -> BinTreeBag t 
      -> BinTreeBag t

%name BinTreeBag t1, t2, t3, t4, t5, t6

export
insert : Ord t => t -> BinTreeBag t -> BinTreeBag t
insert x Empty = Node Empty (x, 1) Empty
insert x (Node tl et@(value, count) tr) = 
  case compare x value of
    LT => Node (insert x tl) et tr
    EQ => Node tl (value, (count + 1)) tr
    GT => Node tl et (insert x tr)

export
remove : Ord t => t -> BinTreeBag t -> BinTreeBag t
remove x Empty = Empty
remove x (Node tl et@(value, count) tr) = 
  case compare x value of
    LT => Node (remove x tl) et tr
    EQ => case count of
              0 => shiftLess tl tr
              (S k) => Node tl (value, k) tr
    GT => Node tl et (remove x tr)
  where 
    shiftLess : BinTreeBag t -> BinTreeBag t -> BinTreeBag t
    shiftLess less (Node tl et@(vr, cr) tr) = Node (shiftLess less tl) et tr
    shiftLess node Empty = node
    shiftLess Empty node = node

export
Eq t => Eq (BinTreeBag t) where
  (Node t1l e1 t1r) == (Node t2l e2 t2r) = e1 == e2 && t1l == t2l && t1r == t2r
  Empty == Empty = True
  _ == _ = False