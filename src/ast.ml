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
type query = (string * string list)

(*FIXME: set up real type *)
type context = unit

type no_query = [ `Doc of Cst.doc ]

type with_query = [`Query of query | no_query ];;

type ast_with_query = with_query list
type ast_no_query = no_query list

(* Dummy function for test purposes *)
let _ =
  Hashtbl.add symbol_table "id"
    (fun context arglist -> `List (List.map (fun elt -> `Content elt) arglist))


(** Cst.doc -> ast *)
let rec extract_queries = function
  `Query (name, arglist) -> `Query (name, arglist)
  | d -> `Doc d

(** Cst.cst -> ast *)
let rec translate cst =
  let rec aux elt acc = match elt with
    Cst.Doc d  -> (extract_queries d)::acc
    (*FIXME: ugliness *)
    | Cst.Code c -> (`Doc (`Content c))::acc
    | _ -> acc (* FIXME: real type *) in
  List.fold_right aux cst []

(* Evaluates the queries of an ast *)
let rec eval ast =
  let aux : with_query -> no_query = function
    #no_query as q -> q
    | `Query (name, arglist) -> `Doc ((Hashtbl.find symbol_table name) ()
        arglist) in
  List.map aux ast
