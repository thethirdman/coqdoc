(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

open Pp
open Names
open Term
open Mod_subst

(** {6 Global reference is a kernel side type for all references together } *)
type global_reference =
  | VarRef of variable
  | ConstRef of constant
  | IndRef of inductive
  | ConstructRef of constructor

val isVarRef : global_reference -> bool
val isConstRef : global_reference -> bool
val isIndRef : global_reference -> bool
val isConstructRef : global_reference -> bool

val eq_gr : global_reference -> global_reference -> bool
val canonical_gr : global_reference -> global_reference

val destVarRef : global_reference -> variable
val destConstRef : global_reference -> constant
val destIndRef : global_reference -> inductive
val destConstructRef : global_reference -> constructor


val subst_constructor : substitution -> constructor -> constructor * constr
val subst_global : substitution -> global_reference -> global_reference * constr

(** Turn a global reference into a construction *)
val constr_of_global : global_reference -> constr

(** Turn a construction denoting a global reference into a global reference;
   raise [Not_found] if not a global reference *)
val global_of_constr : constr -> global_reference

(** Obsolete synonyms for constr_of_global and global_of_constr *)
val constr_of_reference : global_reference -> constr
val reference_of_constr : constr -> global_reference

module RefOrdered : sig
  type t = global_reference
  val compare : global_reference -> global_reference -> int
end

module RefOrdered_env : sig
  type t = global_reference
  val compare : global_reference -> global_reference -> int
end

module Refset : Set.S with type elt = global_reference
module Refmap : Map.S with type key = global_reference

(** {6 Extended global references } *)

type syndef_name = kernel_name

type extended_global_reference =
  | TrueGlobal of global_reference
  | SynDef of syndef_name

module ExtRefOrdered : sig
  type t = extended_global_reference
  val compare : t -> t -> int
end

(** {6 Temporary function to brutally form kernel names from section paths } *)

val encode_mind : dir_path -> identifier -> mutual_inductive
val decode_mind : mutual_inductive -> dir_path * identifier
val encode_con : dir_path -> identifier -> constant
val decode_con : constant -> dir_path * identifier

(** {6 Popping one level of section in global names } *)

val pop_con : constant -> constant
val pop_kn : mutual_inductive-> mutual_inductive
val pop_global_reference : global_reference -> global_reference
