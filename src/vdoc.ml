(** Backend of coqdoc does the translation vdoc -> .output *)

(** This backend manages the default rules for printing and
 ** user-made rules*)

open Cst
open Printf

let rec default_html = function
  `Vernac s -> sprintf"[%s]" s
  | `Pretty_print s -> sprintf "[[%s]]" s
  | `Section (lvl,s) -> sprintf "<h%d>%s</h%d>" lvl s lvl
  | `Item (lvl,d) -> sprintf "<ul>%d: %s</ul>" lvl (default_html d)
  | `Hrule -> "<hr/>"
  | `Emphasis d -> sprintf "<b>%s</b>" (default_html d)
  | `Raw raw -> raw.html
  | `Verbatim s -> sprintf"<tt>%s</tt>" s
  | `Content s -> s
  | `List lst -> List.fold_right (fun elt acc -> (default_html elt) ^ acc)
  lst ""
  | _ -> failwith "Unhandled type"

let to_output outc ?(f = (fun e -> None)) default_fun ast =
  let rec aux elt = match (f elt) with
    None -> output_string outc (default_fun elt)
    | Some s -> output_string outc s in
  List.iter (function Ast.Doc e -> aux e |_ -> failwith "ERROR VDOC") ast
