module BinTree


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

--
-- Interfaces for data structures
--

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
Ord t => Eq (BinTree t) where
  Empty == Empty = True
  Empty == (Node _ _ _) = False
  (Node _ _ _) == Empty = False
  t1 == t2 = BinTree.toList t1 == BinTree.toList t2

--
-- Algebraic structures
--

||| Add value in tree as set.
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

export
Semigroup (BinTree t) => Monoid (BinTree t) where
  neutral = Empty

--
-- Show implementation
--

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
export
shiftLeft : Ord t => BinTree t -> BinTree t -> BinTree t
shiftLeft Empty node = node
shiftLeft node Empty = node
shiftLeft less (Node v left right) = Node v (shiftLeft less left) right

export
remove : Ord a => a -> BinTree a -> BinTree a
remove x Empty = Empty
remove x (Node value left right) =
  case compare x value of
    LT => Node value (remove x left) right
    EQ => shiftLeft left right
    GT => Node value left (remove x right)

export
filter : Ord t => BinTree t -> (f : t -> Bool) -> BinTree t
filter Empty f = Empty
filter node f = foldr take Empty node
  where
    take : t -> BinTree t -> BinTree t
    take et acc = if f et
                    then insert et acc
                    else acc

export
find : Ord t => t -> BinTree t -> BinTree t
find v Empty = Empty
find v node@(Node value left right) =
  case compare v value of
    LT => find v left
    EQ => node
    GT => find v right

export
size : BinTree t -> Nat
size tree = foldr (\_, acc => 1 + acc) 0 tree

export
binTreeFromList : Ord t => List t -> BinTree t
binTreeFromList = foldl (flip insert) Empty

