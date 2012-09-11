(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

open Names
open Term
open Globnames
open Misctypes
open Glob_term

(** [notation_constr] *)

(** [notation_constr] is the subtype of [glob_constr] allowed in syntactic
    extensions (i.e. notations).
    No location since intended to be substituted at any place of a text.
    Complex expressions such as fixpoints and cofixpoints are excluded,
    as well as non global expressions such as existential variables. *)

type notation_constr =
  (** Part common to [glob_constr] and [cases_pattern] *)
  | NRef of global_reference
  | NVar of identifier
  | NApp of notation_constr * notation_constr list
  | NHole of Evar_kinds.t
  | NList of identifier * identifier * notation_constr * notation_constr * bool
  (** Part only in [glob_constr] *)
  | NLambda of name * notation_constr * notation_constr
  | NProd of name * notation_constr * notation_constr
  | NBinderList of identifier * identifier * notation_constr * notation_constr
  | NLetIn of name * notation_constr * notation_constr
  | NCases of case_style * notation_constr option *
      (notation_constr * (name * (inductive * name list) option)) list *
      (cases_pattern list * notation_constr) list
  | NLetTuple of name list * (name * notation_constr option) *
      notation_constr * notation_constr
  | NIf of notation_constr * (name * notation_constr option) *
      notation_constr * notation_constr
  | NRec of fix_kind * identifier array *
      (name * notation_constr option * notation_constr) list array *
      notation_constr array * notation_constr array
  | NSort of glob_sort
  | NPatVar of patvar
  | NCast of notation_constr * notation_constr cast_type

(** Note concerning NList: first constr is iterator, second is terminator;
    first id is where each argument of the list has to be substituted
    in iterator and snd id is alternative name just for printing;
    boolean is associativity *)

(** Types concerning notations *)

type scope_name = string

type tmp_scope_name = scope_name

type subscopes = tmp_scope_name option * scope_name list

(** Type of the meta-variables of an notation_constr: in a recursive pattern x..y,
    x carries the sequence of objects bound to the list x..y  *)
type notation_var_instance_type =
  | NtnTypeConstr | NtnTypeConstrList | NtnTypeBinderList

(** Type of variables when interpreting a constr_expr as an notation_constr:
    in a recursive pattern x..y, both x and y carry the individual type
    of each element of the list x..y *)
type notation_var_internalization_type =
  | NtnInternTypeConstr | NtnInternTypeBinder | NtnInternTypeIdent

(** This characterizes to what a notation is interpreted to *)
type interpretation =
    (identifier * (subscopes * notation_var_instance_type)) list *
    notation_constr
