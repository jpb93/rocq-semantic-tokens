Inductive nat : Type :=
  | O : nat
  | S : nat -> nat.

Definition default : nat := O.

Definition test_let (n : nat) : nat :=
  let default := S n in
  default.

Definition nested_let (n : nat) : nat :=
  let x := n in
  let y := S x in
  let x := y in
  x.

