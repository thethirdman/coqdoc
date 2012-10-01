(* Main definition file for coqdoc *)

open Coqtop
open Lexer
open Lexing
open Parser

  (* Coqdoc's command line parser *)
  let usage = "This is coqdoc ...\nUsage: "
          ^ Sys.argv.(0) ^ " [options] [files]\n"
  let file = ref "" and output = ref ""

  let speclist = [
    ("-o", Arg.String (fun s -> output := s), "Specify output file")]

  (* Function for parsing anonymous arguments *)
  let parse_anon = function
    s when Sys.file_exists s -> file := s;
      | x -> raise (Arg.Bad ("Invalid argument: " ^ x))

  let pp_doc = function
  | Vdoc.Vernac s -> print_endline ("Vernac: " ^ s)
  | Vdoc.Pretty_print s -> print_endline ("pp: " ^ s)
  | Vdoc.Section (a,b) -> print_endline ("section: " ^ b)
  | Vdoc.Elt_list (a,b) -> print_endline ("lst: " ^ b)(*FIXME: replace with type doc *)
  | Vdoc.Emphasis s -> print_endline ("emph: " ^ s)
  | Vdoc.Raw s -> print_endline ("raw")
  | Vdoc.Verbatim s -> print_endline ("verbat: " ^ s)
  | Vdoc.Content s -> print_endline ("cont: " ^ s)
  | _ -> print_endline "foo"

  let treat_doc str =
    let lexbuf = from_string str in
    while lexbuf.lex_eof_reached do
      pp_doc (Parser.parse_doc lex_doc lexbuf)
    done

  let _ =
    Arg.parse speclist parse_anon usage;
      if !file <> "" then
        begin
          if !output = "" then
            output := "out.txt";
        (*FIXME: add arg mngmnt for coqtop *)
        (*let ct = Coqtop.spawn [] in*)
        let src_file = open_in !file (*and dst_file = open_out !output*) in
        let lst = ref [] in
        let lexbuf = from_channel src_file in
        try
          while true do
            let ret = Parser.main document lexbuf in
            match ret with
            | Vdoc.Comment s -> ()
            | Vdoc.Doc s -> lst := s::!lst
            | Vdoc.Code s -> ()
          done
        with Vdoc.End_of_file -> ();
         List.iter treat_doc !lst
        end
          else
            print_string usage
