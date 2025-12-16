Inductive nat : Type :=
| O : nat
| S : nat -> nat.

Definition default : nat := O.

Definition test (default : nat) : nat := default.
