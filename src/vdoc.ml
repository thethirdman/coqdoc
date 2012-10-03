open Interface

(* This module contains the interface converting source files (.v and .tex)
 * to the Intermediate Representation
 *
 * It also does the convertion from Intermediate Representation to
 * Output files (.html, .tex)
 *)

exception End_of_file

type source =
  | Comment of string
  | Doc of string
  | Code of string

type raw_content =
  { latex : string;
    latex_math : string;
    html : string
  }

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

let rec insert elt = function
  | List lst -> List (elt::lst)
  | n        -> List (elt::n::[])
