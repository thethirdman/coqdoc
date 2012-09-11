(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

open Pp
open Compat
open Names
open Pcoq
open Genarg
open Tacexpr
open Vernacexpr

(** Making generic actions in type generic_argument *)

let make_generic_action
  (f:Loc.t -> ('b * raw_generic_argument) list -> 'a) pil =
  let rec make env = function
    | [] ->
	Gram.action (fun loc -> f loc env)
    | None :: tl -> (* parse a non-binding item *)
        Gram.action (fun _ -> make env tl)
    | Some (p, t) :: tl -> (* non-terminal *)
        Gram.action (fun v -> make ((p,in_generic t v) :: env) tl) in
  make [] (List.rev pil)

let make_rule_gen mkproditem mkact pt =
  let (symbs,ntl) = List.split (List.map mkproditem pt) in
  let act = make_generic_action mkact ntl in
  (symbs, act)

(** Grammar extensions declared at ML level *)

type grammar_prod_item =
  | GramTerminal of string
  | GramNonTerminal of
      Loc.t * argument_type * prod_entry_key * identifier option

let make_prod_item = function
  | GramTerminal s -> (gram_token_of_string s, None)
  | GramNonTerminal (_,t,e,po) ->
      (symbol_of_prod_entry_key e, Option.map (fun p -> (p,t)) po)

let make_rule mkact = make_rule_gen make_prod_item mkact

(** Tactic grammar extensions *)

let extend_tactic_grammar s gl =
  let mkact loc l = Tacexpr.TacExtend (loc,s,List.map snd l) in
  let rules = List.map (make_rule mkact) gl in
  maybe_uncurry (Gram.extend Tactic.simple_tactic)
    (None,[(None, None, List.rev rules)])

(** Vernac grammar extensions *)

let vernac_exts = ref []
let get_extend_vernac_grammars () = !vernac_exts

let extend_vernac_command_grammar s nt gl =
  let nt = Option.default Vernac_.command nt in
  vernac_exts := (s,gl) :: !vernac_exts;
  let mkact loc l = VernacExtend (s,List.map snd l) in
  let rules = List.map (make_rule mkact) gl in
  maybe_uncurry (Gram.extend nt) (None,[(None, None, List.rev rules)])
