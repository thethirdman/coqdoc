(* show test file *)
(** Without proof: *)
Parameter P Q R : Prop.
Theorem imp_trans : (P -> Q) -> (Q -> R) -> P -> R.
Proof.
  auto.
Qed.

(** Showing proof: *)
(* begin show *)
Parameter P Q R : Prop.
Theorem imp_trans : (P -> Q) -> (Q -> R) -> P -> R.
Proof.
  auto.
Qed.
(* end show *)
