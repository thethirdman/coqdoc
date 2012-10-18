(* Main definition file for coqdoc *)

open Vernac_lexer
open Lexing
open Lexer
open Settings

  (* Takes a coqdoc documentation string, returns a Cst.doc tree *)
  let treat_doc str =
    let lexbuf = from_string str in
    (Parser.parse_doc lex_doc lexbuf)

  let _ =
    Settings.parse ();
    (*let ct = Coqtop.spawn [] in*)
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
    let ast = Ast.translate cst in Vdoc.to_output !(io.o_chan) Vdoc.default_html (Ast.eval ast)
