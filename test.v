
Definition plus (n m : nat) : nat :=
  match n with
  | O => m
  | S p => S (plus p m)
  end.

Fixpoint mult (n m : nat) : nat :=
  match n with
  | O => O
  | S p => plus m (mult p m)
  end.

(* IGNORE COMMENTS *)
(* **** ()()()() (* *) (*(*(* *)*)*)***** ***( HELLO **&******l)*)

Lemma plus_O_n : forall n, plus O n = n.
Proof.
  intros n.
  reflexivity.
Qed.
