(* Identity output module for test purposes *)

open Printf
open Cst

module Ident = struct
  let print_raw raw =
    if raw.html <> "" then printf "#%s#" raw.html;
    if raw.latex <> "" then printf "%%%s%%" raw.latex;
    if raw.latex_math <> "" then printf "$%s$" raw.latex_math

  let print_doc doc =
    let rec aux = function
      | Cst.Vernac s -> printf "[%s]" s
      | Cst.Pretty_print s -> printf "[[%s]]" s
      | Cst.Section (a,b) -> printf "%s %s" (String.make a '*') b
      | Cst.Item (a,b) -> printf "%*s-%s" a "" ""(*FIXME: replace with type doc *)
      | Cst.Emphasis s -> printf "_"; aux s; printf "_"
      | Cst.Raw s -> print_raw s
      | Cst.Verbatim s -> printf "<<%s>>" s
    | Cst.Content s -> printf "%s" s
    | Cst.List lst -> List.iter aux lst
    | Cst.Hrule -> printf("----")
    | _ -> printf "foo\n" in
  printf "(**";
  aux doc;
  printf "*)"

let rec print = function
  | Cst.Comment s -> printf "(*%s*)" s
  | Cst.Doc e -> print_doc e
  | Cst.Code s -> printf "%s" s
  | Cst.Seq  s -> List.iter print s

end
