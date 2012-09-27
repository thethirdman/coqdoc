{
  open Parser
  open Lexing

  exception Error of string

  let buff = Buffer.create 42

  let backtrack lexbuf = lexbuf.lex_curr_pos <- lexbuf.lex_start_pos;
    lexbuf.lex_curr_p <- lexbuf.lex_start_p

}

  let sp = [' '  '\t']
  let nl = "\r\n" | '\n'

  let sp_nl = sp | nl (* Space or newline *)

rule document = parse
  '(' '*' {Buffer.reset buff; comment lexbuf}
  | sp_nl {document lexbuf}
  | _ as c {Buffer.reset buff; Buffer.add_char buff c; code lexbuf}
  | eof { EOF }

and comment = parse
  | eof {raise (Error "Expecting '*)'")}
  | '*' ')' {DOC (Buffer.contents buff)}
  | _ as c {Buffer.add_char buff c; comment lexbuf}

and code = parse
  '(' '*' {backtrack lexbuf; CODE (Buffer.contents buff)}
  | eof {CODE (Buffer.contents buff)}
  | _ as c {Buffer.add_char buff c; code lexbuf}
