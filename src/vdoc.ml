open Interface

(* This module contains the interface converting source files (.v and .tex)
 * to the Intermediate Representation
 *
 * It also does the convertion from Intermediate Representation to
 * Output files (.html, .tex)
 *)

module Vdoc = struct

  (*FIXME:
    * Manage hyperlinks
    * hide/show ?
    *)
  type markup =
    | Parse of string * parse_answer (* FIXME: real type*)
    | Raw of string (* raw line *)
    | Goals of goals option
    | Section of int (* section delimiter, containing section level*)
    | List of markup list
    | Rule
    | Emphasis of string (*FIXME: create a more generic symbol*)
    | Verbatim of string (*FIXME: string list ?*)
    | Empty_line

  let state = ref []

  let holder content coqtop = ()
  let add_pp_token tok pp   = ()
  let rm_pp_token tok       = ()
  let do_nothing _          = ()

  let add (elt:markup) = state := elt::!state

  let pretty_print () =
    let aux = function
      | Raw source           -> print_string ("raw: " ^ source ^ "\n")
      | Parse (s,a)          -> print_string ("parse: " ^ s ^ "\n")
      | Verbatim s           -> print_string ("Verbatim:\n\t" ^ s ^ "\n")
      | _ -> print_string ("goals\n")
    in
    List.iter aux !state
end
