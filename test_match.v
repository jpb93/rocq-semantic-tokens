(* test_match.v *)

Inductive nat : Type :=
  | O : nat
  | S : nat -> nat.

Inductive wrapper :=
  | Wrap : (nat -> nat) -> wrapper.

Definition pred (n : nat) : nat :=
  match n with
  | O => O
  | S m => m
  end.

Definition apply (w : wrapper) (x : nat) : nat :=
  match w with
  | Wrap f => f x
  end.

Definition nested (n : nat) : nat :=
  match n with
  | O => O
  | S m => match m with
           | O => S O
           | S k => k
           end
  end.

Definition wildcard (n : nat) : nat :=
  match n with
  | O => O
  | _ => S O
  end.
