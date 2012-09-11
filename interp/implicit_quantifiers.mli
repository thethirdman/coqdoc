(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

open Loc
open Names
open Decl_kinds
open Term
open Sign
open Evd
open Environ
open Nametab
open Mod_subst
open Glob_term
open Constrexpr
open Pp
open Libnames
open Globnames
open Typeclasses

val declare_generalizable : Vernacexpr.locality_flag -> (identifier located) list option -> unit

val ids_of_list : identifier list -> Idset.t
val destClassApp : constr_expr -> Loc.t * reference * constr_expr list
val destClassAppExpl : constr_expr -> Loc.t * reference * (constr_expr * explicitation located option) list

(** Fragile, should be used only for construction a set of identifiers to avoid *)

val free_vars_of_constr_expr : constr_expr -> ?bound:Idset.t ->
  identifier list -> identifier list

val free_vars_of_binders :
  ?bound:Idset.t -> Names.identifier list -> local_binder list -> Idset.t * Names.identifier list

(** Returns the generalizable free ids in left-to-right
   order with the location of their first occurence *)

val generalizable_vars_of_glob_constr : ?bound:Idset.t -> ?allowed:Idset.t ->
  glob_constr -> (Names.identifier * Loc.t) list

val make_fresh : Names.Idset.t -> Environ.env -> identifier -> identifier

val implicits_of_glob_constr : ?with_products:bool -> Glob_term.glob_constr -> Impargs.manual_implicits

val combine_params_freevar :
  Names.Idset.t -> (global_reference * bool) option * (Names.name * Term.constr option * Term.types) ->
  Constrexpr.constr_expr * Names.Idset.t

val implicit_application : Idset.t -> ?allow_partial:bool ->
  (Names.Idset.t -> (global_reference * bool) option * (Names.name * Term.constr option * Term.types) ->
    Constrexpr.constr_expr * Names.Idset.t) ->
  constr_expr -> constr_expr * Idset.t
