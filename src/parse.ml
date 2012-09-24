(* .vdoc parser *)

(*open Coq*)
open Interface
open Coqtop
open Vdoc

module Parser = struct
  exception Invalid_keyword of string
  exception Error_msg of string

  let command = Str.regexp "^\\(.*\\)@\\([a-zA-Z0-9_-]+\\){\\(.*\\)}\\(.*\\)$"

  (*FIXME: add real exn *)
  let extract_value exp msg = match exp with
    Good s | Unsafe s -> s
    | _ -> raise (Invalid_keyword msg)

  (* Parsing function for a .v file *)
  let rec parse_vernac coqtop line =
    if Str.string_match command line 0 then
      begin
        let before = Str.matched_group 1 line
        and kw =  Str.matched_group 2 line
        and content = Str.matched_group 3 line
        and after = Str.matched_group 4 line in
        (parse_vernac coqtop before);
        (match kw with
        | "code" ->  Vdoc.add (Vdoc.Interp (line,
            (extract_value (Coqtop.interp coqtop Coqtop.default_logger
            content) "Fail ton interp")))
        | "goal" ->  Vdoc.holder content coqtop
        | "parse" -> (Vdoc.add (Vdoc.Parse (line,
        (extract_value (Coqtop.parse coqtop content) "Fail to parse"))))
        | s -> raise (Invalid_keyword s));
        (Vdoc.add (Vdoc.Raw after))
      end
    else
      Vdoc.add (Vdoc.Raw line)

  let in_coqtex = ref false
  and end_section = Str.regexp   ".*\\\\end{coq_\\(.*\\)}.*"
  and begin_section = Str.regexp ".*\\\\begin{coq_\\(.*\\)}.*"

  (* Parsing function forf a .tex file *)
  let rec parse_coqtex coqtop line =
    if Str.string_match begin_section line 0 then
      in_coqtex := true
    else if Str.string_match end_section line 0 then
      in_coqtex := false
    else if !in_coqtex then
      let ret = (Coqtop.interp coqtop Coqtop.default_logger line) in
        match ret with
      | Good resp | Unsafe resp -> Vdoc.add (Vdoc.Interp (line, resp))
      | _ -> raise (Invalid_keyword "parse_coqtex")
    else
      Vdoc.add (Vdoc.Raw line)

  let get_parse_function file =
    if Filename.check_suffix file ".tex" then
      parse_coqtex
    else
      parse_vernac

  let parse_file coqtop src dest =
    let src_file = open_in src and dst_file = open_out dest
    and cur_line = ref ""
    and parse_fn = get_parse_function src in
    try
      while true do
        (*cur_line :=*) parse_fn coqtop (input_line src_file);
      output_string dst_file (!cur_line ^ "\n");
      Vdoc.pretty_print ();
      done;
      with End_of_file -> close_in src_file; close_out dst_file;

      end
