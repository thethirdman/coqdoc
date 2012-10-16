open Cst
open Ident
(* This module contains the AST representation
 * the evaluation of this ast creates an abstract representation of
 * what will be put in the .vdoc
 *)

(** stores the defined symbols in coqdoc: primitives and user-defined functions
 (symbol * (name -> context -> arglist -> doc) list
 *)
let symbol_table = Hashtbl.create 42

type symbol = string
type arglist = string list

(*FIXME: set up real type *)
type context = unit

type ast_node =
  Doc of Cst.doc
  | Query of (symbol * arglist)
  | Seq of ast_node list (*FIXME: move to a canonical representation*)


(* Dummy function for test purposes *)
let _ =
  Hashtbl.add symbol_table "id"
    (fun context arglist -> Cst.List (List.map (fun elt -> Cst.Content elt) arglist))


let rec extract_queries = function
  Cst.Query (name, arglist) -> Query (name, arglist)
  | Cst.List lst -> Seq (List.fold_right (fun elt acc -> (extract_queries
  elt)::acc) lst [])
  | d -> Doc d

let rec translate = function
    Cst.Doc d  -> extract_queries d
  | Cst.Seq s  -> Seq (List.fold_right (fun elt acc -> (translate elt)::acc) s [])
  | _ -> Seq [] (* FIXME: real type *)

(* Evaluates the queries of an ast *)
let rec eval = function
  Doc d -> Doc d
  | Query (name, arglist) -> Doc ((Hashtbl.find symbol_table name) () arglist)
  | Seq s -> Seq (List.fold_right (fun elt acc -> (eval elt)::acc) s [])
