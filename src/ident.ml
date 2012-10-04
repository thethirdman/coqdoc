(* Identity output module for test purposes *)

open Printf
open Vdoc

module Ident = struct

  let print_doc doc =
    let rec aux = function
      | Vdoc.Vernac s -> printf "[%s]" s
      | Vdoc.Pretty_print s -> printf "[[%s]]" s
      | Vdoc.Section (a,b) -> printf "%*s*%s" a "" b
      | Vdoc.Item (a,b) -> printf "%*s-%s" a "" ""(*FIXME: replace with type doc *)
      | Vdoc.Emphasis s -> printf "_"; aux s; printf "_"
      | Vdoc.Raw s -> print_string ("raw")
      | Vdoc.Verbatim s -> printf "<<%s>>" s
    | Vdoc.Content s -> printf "%s" s
    | Vdoc.List lst -> List.iter aux lst
    | _ -> printf "foo\n" in
  printf "(**";
  aux doc;
  printf "*)"

let print = function
  | Vdoc.Comment s -> printf "(*i%si*)" s
  | Vdoc.Doc s -> printf "ERROR FAIL OMG"
  | Vdoc.Code s -> printf "%s" s
end
