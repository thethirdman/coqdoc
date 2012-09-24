open Interface

(* This module contains the interface converting source files (.v and .tex)
 * to the Intermediate Representation
 *
 * It also does the convertion from Intermediate Representation to
 * Output files (.html, .tex)
 *)

module Vdoc = struct

  type markup =
    Interp of string * string (* source,code *)
    | Parse of string * parse_answer (* FIXME: real type*)
    | Raw of string (* raw line *)

  let state = ref []

  let holder content coqtop = ()

  let add (elt:markup) = state := elt::!state

  let pretty_print () =
    let aux = function
      | Interp (source,code) -> print_string ("Interp: " ^ source ^ "\n\n" ^ code)
      | Raw source           -> ()
      | Parse _              -> print_string ("jambon\n");
    in
    List.iter aux !state
end
