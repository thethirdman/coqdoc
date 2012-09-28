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

  let _ =
    Arg.parse speclist parse_anon usage;
      if !file <> "" then
        begin
          if !output = "" then
            output := "out.txt";
        (*FIXME: add arg mngmnt for coqtop *)
        let ct = Coqtop.spawn [] in
        let src_file = open_in !file and dst_file = open_out !output in
        let lexbuf = from_channel src_file in
        try
          while true do
            let ret = Parser.main document lexbuf in
            match ret with
            | Vdoc.Comment s -> print_string ("(*" ^ s ^ "*)")
            | Vdoc.Code s -> print_string s
          done
            with Vdoc.End_of_file -> ()
        end
          else
            print_string usage
