Module RocqTokenTest.

(***************************)
(* Inductive nat : Type := *)
(* | O : nat               *)
(* | S : nat -> nat.       *)
(***************************)

Inductive mumble : Type := Grumble | Bumble | Boop.

Inductive color : Set := red | yellow | blue.

Inductive wrapper :=
  | Wrap : (nat -> nat) -> wrapper.

Definition plus (n m : nat) : nat :=
  match n with
  | O => m
  | S p => S (plus p m)
  end.

Definition next (n : nat) : nat := S n.

Fixpoint mult (n m : nat) : nat :=
  match n with
  | O => O
  | S p => plus m (mult p m)
  end.

Fixpoint fib (n : nat) : nat :=
  match n with
  | O => O
  | S O => S O
  | S (S n' as m) => plus (fib m) (fib n')
  end.

(* IGNORE COMMENTS *)
(* **** ()()()() (* *) (*(*(* *)*)*)***** ***( HELLO **&******l)*)

Lemma plus_O_n : forall n, plus O n = n.
Proof.
  intros n.
  reflexivity.
Qed.

Lemma plus_n_O : forall n, plus n O = n.
Proof.
  intros n.
  induction n.
  - reflexivity.
  - simpl.
    rewrite IHn.
    reflexivity.
Qed.

End RocqTokenTest.
