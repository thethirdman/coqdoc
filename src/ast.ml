(* This module contains the AST representation
 * the evaluation of this ast creates an abstract representation of
 * what will be put in the .vdoc
 *)

(** stores the defined symbols in coqdoc: primitives and user-defined functions
 (symbol * (name -> context -> arglist -> doc) list
 *)
open Settings
open Coqtop

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

let pretty_print ct i_type c =
  if (i_type = Settings.Vdoc) && (c <> "") then
    try
    let ret = Coqtop.parse ct c in
    let rec foo tok_type c = match tok_type with
      Interface.Keyword -> Cst.Keyword c
      | Interface.Ident -> Cst.Ident c
      | Interface.Literal -> Cst.Literal c in
    let aux ((bef,aft),tok_type) =
      (foo tok_type (String.sub c bef (aft - bef)))  in

    `Doc (`Code (List.fold_left (fun acc elt -> (aux elt)::acc) [] (Coqtop.handle_value
    ret).Interface.markup));
    with Invalid_argument _ -> `Doc (`Content c)
    else
      `Doc (`Content c)

  (** Cst.cst -> ast *)
let rec translate ct i_type cst =
  let rec aux elt acc = match elt with
    Cst.Doc d  -> (extract_queries d)::acc
    (*FIXME: ugliness *)
  | Cst.Code c -> (pretty_print ct i_type c)::acc
    | _ -> acc (* FIXME: real type *) in
  List.fold_right aux cst []

  (* Evaluates the queries of an ast *)
let rec eval ast =
  let aux : with_query -> no_query = function
    #no_query as q -> q
    | `Query (name, arglist) ->
        try
          `Doc ((Hashtbl.find symbol_table name) () arglist)
        with Not_found -> Printf.fprintf stderr "Error: Invalid query \"%s\"\n"
        name; exit 1
  in
  List.map aux ast
