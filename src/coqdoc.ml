(* Main definition file for coqdoc *)

open Coqtop
open Vernac_lexer
open Lexing
open Parser
open Lexer
open MenhirLib
open Ident
open Ast

  (* Coqdoc's command line parser *)
  (* FIXME: make a real usage doc_string *)
  let usage = "This is coqdoc ...\nUsage: "
          ^ Sys.argv.(0) ^ " [options] [files]\n"
  let file = ref "" and output = ref ""

  (* Option list for coqdoc *)
  let speclist = [
    ("-o", Arg.String (fun s -> output := s), "Specify output file")]

  (* Function for parsing anonymous arguments *)
  let parse_anon = function
    s when Sys.file_exists s -> file := s;
      | x -> raise (Arg.Bad ("Invalid argument: " ^ x))


  (* Takes a coqdoc documentation string, returns a Cst.doc tree *)
  let treat_doc str =
    let lexbuf = from_string str in
    (Parser.parse_doc lex_doc lexbuf)

  let rec traverse f = function
  Doc d -> f d
  | Seq s -> List.iter (fun e -> traverse f  e) s
  | Query  _ -> failwith "FAIL"

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
          (*let lexbuf = from_channel src_file in*)
          try
            while true do
              let revised_parser =
              (MenhirLib.Convert.Simplified.traditional2revised Parser.main) in
              let ret = revised_parser (Vernac_lexer.lex src_file) in
              lst := ret::!lst
            done
          with Cst.End_of_file -> ();
          let cst = Cst.make_cst (List.rev !lst) treat_doc in
          let ast = Ast.translate cst in (traverse Ident.print_doc (eval ast))
        end
          else
            print_string usage
