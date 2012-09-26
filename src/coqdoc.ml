(* Main definition file for coqdoc *)

open Coqtop
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
        Parser.parse_file ct !file !output
        end
      else
        print_string usage
