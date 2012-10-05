(* Identity output module for test purposes *)

open Printf
open Vdoc

module Ident = struct
  let print_raw raw =
    if raw.html <> "" then printf "#%s#" raw.html;
    if raw.latex <> "" then printf "%%%s%%" raw.latex;
    if raw.latex_math <> "" then printf "$%s$" raw.latex_math

  let print_doc doc =
    let rec aux = function
      | Vdoc.Vernac s -> printf "[%s]" s
      | Vdoc.Pretty_print s -> printf "[[%s]]" s
      | Vdoc.Section (a,b) -> printf "%s %s" (String.make a '*') b
      | Vdoc.Item (a,b) -> printf "%*s-%s" a "" ""(*FIXME: replace with type doc *)
      | Vdoc.Emphasis s -> printf "_"; aux s; printf "_"
      | Vdoc.Raw s -> print_raw s
      | Vdoc.Verbatim s -> printf "<<%s>>" s
    | Vdoc.Content s -> printf "%s" s
    | Vdoc.List lst -> List.iter aux lst
    | Vdoc.Hrule -> printf("----")
    | _ -> printf "foo\n" in
  printf "(**";
  aux doc;
  printf "*)"

let print = function
  | Vdoc.Comment s -> printf "(*%s*)" s
  | Vdoc.Doc s -> printf "ERROR FAIL OMG"
  | Vdoc.Code s -> printf "%s" s
end
