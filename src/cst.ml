(* This module contains the CST representation of the entry file parsing
 *
 *)

exception End_of_file

(* First data-type, splits documentation and code *)
type source =
  | Raw_Comment of string
  | Raw_Doc of string
  | Raw_Code of string

(* Type representing inline tex/html in source files *)
type raw_content = { latex : string; latex_math : string; html : string}

(* Doc data-type, first level of documentation representation *)
type doc =
  Vernac of string
  | Pretty_print of string
  | Add_token of string*raw_content
  | Rm_token of string
  | Section of (int*string)
  | Item of (int*doc) (* List in coqdoc *)
  | Hrule
  | Emphasis of doc (*FIXME: replace with type doc *)
  | Raw of raw_content
  | Verbatim of string
  | Content of string
  | List of doc list
  (* Coqdoc's keywords *)
  | Let of (string * string)
  | Print of string


(* Final CST *)
type cst =
  | Comment of string
  | Doc of doc
  | Code of string
  | Seq of cst list

let insert_seq elt = function
  (Seq lst) -> (Seq (elt::lst))
  | s -> (Seq (elt::s::[]))

let make_cst lst doc_converter =
  let rec aux elt acc = match elt with
        Raw_Doc s -> insert_seq (Doc (doc_converter s)) acc
        | Raw_Comment s -> insert_seq (Comment s) acc
        | Raw_Code s -> insert_seq (Code s) acc
  in List.fold_right aux lst (Seq [])

