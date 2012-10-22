(** Backend of coqdoc does the translation vdoc -> .output *)

(** This backend manages the default rules for printing and
 ** user-made rules*)

open Cst
open Printf

module Backend =
  functor (Formatter :
    sig
      val initialize : unit    -> unit
      val header     : unit    -> string
      val doc        : Cst.doc -> string option
      val indent     : int     -> string (*FIXME: not used right now*)
      val newline    : unit    -> string (*FIXME: idem *)
      val index      : 'a list -> string
      val footer     : unit    -> string
    end) ->
struct

let transform outc ?(f = (fun e -> None)) default_fun ast =
  Formatter.initialize (); output_string outc (Formatter.header ());
  let rec aux elt = match (Formatter.doc elt) with
    None -> output_string outc (default_fun elt)
    | Some s -> output_string outc s in
  List.iter (function Ast.Doc e -> aux e |_ -> failwith "ERROR VDOC") ast;
  output_string outc (Formatter.index []); (*FIXME*)
  output_string outc (Formatter.footer ());
end
