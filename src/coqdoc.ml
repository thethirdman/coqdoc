(* Main definition file for coqdoc *)

open Parse
open Coqtop

(* Coqdoc's command line parser *)
let usage = "This is coqdoc ...\nUsage: "
          ^ Sys.argv.(0) ^ " [options] [files]\n"
let file = ref "" and output = ref ""
let html_or_tex = ref true

let speclist = [
  ("-o", Arg.String (fun s -> output := s), "Specify output file")]

(* Handler function for doc generation *)
let gen_doc () =
  let (pid, ic, oc) = Coqtop.start_coqtop () in
  Parser.parse_file ic oc !file !output;
  Unix.kill pid Sys.sigkill

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
        gen_doc ()
        end
      else
        print_string usage
