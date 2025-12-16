Inductive tree (A : Type) : Type :=
  | leaf : tree A
  | node : A -> tree A -> tree A -> tree A.

Fixpoint tree_map {A B : Type} (f : A -> B) (t : tree A) : tree B :=
  match t with
  | leaf => leaf
  | node x l r => node (f x) (tree_map f l) (tree_map f r)
  end.

Fixpoint tree_fold {A B : Type} (fleaf : B) (fnode : A -> B -> B -> B) (t : tree A) : B :=
  match t with
  | leaf => fleaf
  | node x l r => fnode x (tree_fold fleaf fnode l) (tree_fold fleaf fnode r)
  end.
