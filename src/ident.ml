(* Identity output module for test purposes *)
(* The call to print will reproduce the input file
 * on stdout*)

open Printf

module Ident = struct
  let print_raw raw =
    if raw.html <> "" then printf "#%s#" raw.html;
    if raw.latex <> "" then printf "%%%s%%" raw.latex;
    if raw.latex_math <> "" then printf "$%s$" raw.latex_math

  let print_doc doc =
    let rec aux = function
      | `Vernac s -> printf "[%s]" s
      | `Pretty_print s -> printf "[[%s]]" s
      | `Section (a,b) -> printf "%s %s" (String.make a '*') b
      | `Item (a,b) -> printf "%*s- %s" a "" ""(*FIXME: replace with type doc *)
      | `Emphasis s -> printf "_"; aux s; printf "_"
      | `Raw s -> print_raw s
      | `Verbatim s -> printf "<<%s>>" s
      | `Content s -> printf "%s" s
      | `List lst -> List.iter aux lst
      | `Hrule -> printf("----")
      | `Query (name, arg_list) ->
          printf "@%s{" name;
          printf "%s" (List.hd arg_list);
          List.iter (fun e -> printf ",%s" e) (List.tl arg_list);
          printf "}"
      | _ -> printf "foo\n" in
    printf "(**";
    aux doc;
    printf "*)"

let print lst =
  let aux = function
    | Cst.Comment s -> printf "(*%s*)" s
    | Cst.Doc e -> print_doc e
    | Cst.Code s -> printf "%s" s in
  List.iter aux lst

end
