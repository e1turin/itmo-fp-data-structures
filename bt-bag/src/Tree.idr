module Tree

%default total


namespace Naive

  ||| Binary Search Tree without constraint
  public export
  data Tree : Type -> Type where
       Empty : Tree t
       Node : (left : Tree t) -> (value : t) -> (Tree t) -> Tree t

  %name Tree t1, t2, t3, t4

  insert : Ord t => t -> Tree t -> Tree t
  insert x Empty = Node Empty x Empty
  insert x node@(Node t1 y t2) =
    case compare x y of
      LT => Node (insert x t1) y t2
      EQ => node
      GT => Node t1 y (insert x t2)


namespace Basic

  ||| Binary Search Tree with constraint
  public export
  data BSTree : Type -> Type where
      Empty : Ord t => BSTree t
      Node : Ord t => (left : BSTree t)
                   -> (value : t)
                   -> (right : BSTree t)
                   -> BSTree t

  insert : t -> BSTree t -> BSTree t
  insert x Empty = Node Empty x Empty
  insert x node@(Node left value right) =
    case compare x value of
      LT => Node (insert x left) value right
      EQ => node
      GT => Node left value (insert x right)
