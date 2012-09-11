(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

open Pp
open Genarg
open Vernacexpr
open Names
open Nameops
open Nametab
open Ppconstr
open Pptactic
open Glob_term
open Pcoq
open Libnames
open Ppextend
open Topconstr

(** Prints a vernac expression *)
val pr_vernac_body : vernac_expr -> std_ppcmds

(** Prints a vernac expression and closes it with a dot. *)
val pr_vernac : vernac_expr -> std_ppcmds
