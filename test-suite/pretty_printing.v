(* Test file for pretty printing *)

Parameter P Q R : Prop.
Theorem imp_trans : (P -> Q) -> (Q -> R) -> P -> R.
Proof.
  auto.
Qed.

(** All characters *)
Theorem t : a -> a.
Theorem t : a <- a.
Theorem t : a *  a.
Theorem t : a <= a.
Theorem t : a >= a.
Theorem t : a => a.
Theorem t : a <> a.
Theorem t : a <-> a.
Theorem t : a |- a.
Theorem t : a \/ a.
Theorem t : a /\ a.
Theorem t : a ~  a.
