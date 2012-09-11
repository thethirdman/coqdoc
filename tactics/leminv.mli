(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

open Loc
open Pp
open Names
open Term
open Glob_term
open Proof_type
open Constrexpr
open Misctypes

val lemInv_gen : quantified_hypothesis -> constr -> tactic
val lemInvIn_gen : quantified_hypothesis -> constr -> identifier list -> tactic

val lemInv_clause :
  quantified_hypothesis -> constr -> identifier list -> tactic

val inversion_lemma_from_goal :
  int -> identifier -> identifier located -> sorts -> bool ->
    (identifier -> tactic) -> unit
val add_inversion_lemma_exn :
  identifier -> constr_expr -> glob_sort -> bool -> (identifier -> tactic) ->
    unit
