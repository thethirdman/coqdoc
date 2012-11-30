(* Main definition file for coqdoc *)

open Vernac_lexer
open Lexing
open Lexer
open Settings
open Vdoc
open Coqtop

module Lord = Backend(Html)
(* Takes a coqdoc documentation string, returns a Cst.doc tree *)
let treat_doc str =
  let lexbuf = from_string str in
  (Parser.parse_doc lex_doc lexbuf)

let _ =
  Settings.parse ();
  let ct = Coqtop.spawn [] in
  if !(io.i_type) = Vdoc then
    begin
      let lst = ref [] in
      (*let lexbuf = from_channel src_file in*)
      try
        while true do
          let revised_parser =
          (MenhirLib.Convert.Simplified.traditional2revised Parser.main) in
          let ret = revised_parser (Vernac_lexer.lex !(io.i_chan)) in
          lst := ret::!lst
        done
      with Cst.End_of_file -> ();
      let cst = Cst.make_cst (List.rev !lst) treat_doc in
      let ast = Ast.translate ct !(io.i_type) cst in Lord.transform !(io.o_chan)
        (fun s -> "error") (Ast.eval ast)
    end
  else
    begin
      Printexc.record_backtrace true;
    let lst = ref [] in
    try
      let lexbuf = Lexing.from_channel !(io.i_chan) in
      while true do
      let ret = Parser.parse_tex lex_tex lexbuf
      in lst := ret::!lst;
      done
    with Cst.End_of_file -> ();
    let cst = (List.rev !lst) in
    let ast = Ast.translate ct !(io.i_type) cst in Lord.transform !(io.o_chan)
    (fun s -> "error") (Ast.eval ast)
    end
