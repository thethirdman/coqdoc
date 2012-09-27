(* .vdoc parser *)

(*open Coq*)
open Interface
open Coqtop
open Vdoc
open Lexing
open Lexer

module Parser = struct
  exception Invalid_keyword of string
  exception Error_msg of string

  let extract_value exp = match exp with
    Good s | Unsafe s -> s
    | Fail (loc,s)  -> raise (Invalid_keyword s)

    (* Parsing function for a .v file *)
  let rec parse_vernac coqtop file = ()
    (*Cpretty.coq_bol file*)

  type context =
    Nothing
        | Example
        | Example_star
        | Eval

  let in_coqtex = ref Nothing
  and end_section = Str.regexp   ".*\\\\end{coq_\\(.*\\)}.*"
  and begin_section = Str.regexp ".*\\\\begin{coq_\\(.*\\)}.*"

  (* Parsing function for a .tex file *)
  (*let rec parse_coqtex coqtop line =
    if Str.string_match begin_section line 0 then
      if (Str.matched_group 1 line) = "example" then in_coqtex := Example
      else if (Str.matched_group 1 line) = "example*" then in_coqtex :=
        Example_star
      else if (Str.matched_group 1 line) = "eval" then in_coqtex := Eval
      else raise (Invalid_keyword "Invalid section")
    else if Str.string_match end_section line 0 then
      in_coqtex := Nothing
    else if !in_coqtex <> Nothing then
      let ret = (Coqtop.parse coqtop line) in
      match !in_coqtex with
      | Example -> ()
      | Example_star -> ()
      | Eval -> ()
      | _ -> assert false
    else*)

  (*let get_parse_function file =
    if Filename.check_suffix file ".tex" then
      parse_coqtex
    else
      parse_vernac*)

  let parse_file coqtop src dest =
    let src_file = open_in src and dst_file = open_out dest
    (*and parse_fn = get_parse_function src *) in
    try
      let ret = document (from_channel src_file);
      (*while true do
        parse_fn coqtop (input_line src_file);*)
      (*done;*)
      Parser.main ret;
      with End_of_file -> close_in src_file; close_out dst_file;

end
