(* test_scope.v *)

Inductive nat : Type :=
  | O : nat
  | S : nat -> nat.

Definition default (n : nat) :=
  match n with
  | Some n => n
  | None => O
  end.

(* Shadowing: parameter 'default' shadows global 'default' *)
Fixpoint nth (l : list nat) (default : nat) : nat :=
  match l with
  | nil => default        (* this 'default' is the parameter, NOT the global *)
  | cons _ t => nth t default
  end.

(* Nested scopes *)
Definition nested (x : nat) : nat :=
  let y := S x in
  let x := y in
  fun z => x + y + z.

(* Match bindings *)
Definition match_test (n : nat) : nat :=
  match n with
  | O => O
  | S p => p
  end.


Inductive wrapper :=
  | Wrap : (nat -> nat) -> wrapper.

Definition apply_wrapped (w : wrapper) (x : nat) : nat :=
  match w with
  | Wrap f => f x
  end.

(* forall bindings *)
Lemma example : forall n m, n = m -> m = n.
Proof. intros n m H. auto. Qed.
