open Interface

(* This module contains the interface converting source files (.v and .tex)
 * to the Intermediate Representation
 *
 * It also does the convertion from Intermediate Representation to
 * Output files (.html, .tex)
 *)

exception End_of_file

type source =
  | Doc of string (* FIXME *)
  | Code of string
