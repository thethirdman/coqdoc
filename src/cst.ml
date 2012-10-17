(* This module contains the CST representation of the entry file parsing
 *
 *)

exception End_of_file

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
  (* Type for documentation queries: @name{arg_list} *)
  | Query of (string*string list)


(* Final CST *)
type 'a cst_node =
  | Comment of string
  | Doc of 'a
  | Code of string

type 'a cst = ('a cst_node) list

(* Inserts an element into a cst.Seq type. Creates a Seq if necessary*)

(* Converts source and doc types into the common type cst *)
let make_cst lst (doc_converter:string -> doc) =
  let aux node = match node with
    | Doc s -> Doc (doc_converter s)
    | Code n -> Code n
    | Comment n -> Comment n in
  List.map aux lst
