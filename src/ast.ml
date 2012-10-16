open Cst
open Ident
(* This module contains the AST representation
 * it also in charge of output to .vdoc
 *)

(* Bounded name table *)
let name_index = ref (("plop", Cst.Comment "EXEMPLE")::[])

(* Context information: FIXME *)
type context = {loc : string}

type name    = string
type request = int

(* Ast type : contains chunks of documentation, requests to the toplevel,
 * [contains also settings/headers for the output ?]
 *)

