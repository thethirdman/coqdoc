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

type 'a ast =
  (* Function for documentation printing *)
  Doc_chunk of (('a -> 'a) -> 'a)
  (* Function for name binding *)
  (* Should be name -> request 'a -> 'a *)
  | Let of (name -> 'a)
  (* Prints bounded code *)
  | Print of ((context -> name -> 'a) -> name -> 'a)
  | Seq of ('a ast) list

let rec ast_of_cst cst =
  let rec aux = function
    | Cst.Let (name,value) ->name_index := (name,(Cst.Doc (Cst.Content value)))::!name_index;
      Let (fun name -> List.assoc name
      !name_index)
    | Cst.List lst ->
      Seq (List.fold_right (fun elt acc -> (aux elt)::acc) lst [])
    | s -> Doc_chunk (fun f -> f (Cst.Doc s)) in
  match cst with
  | Cst.Doc s -> aux s
  | Cst.Seq lst -> Seq
    (List.fold_right (fun elt acc -> (ast_of_cst elt)::acc) lst [])
  | s -> Doc_chunk (fun f -> f s)


(** Sample function for pretty printing *)
let rec pp_ast = function
  Doc_chunk f -> Ident.print (f (fun id -> id))
  | Let f -> Ident.print (f "plop")
  | Seq lst -> List.iter pp_ast lst
  | _ -> Printf.printf "smth else\n"
