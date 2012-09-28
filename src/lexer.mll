{
  open Parser
  open Lexing

  exception Error of string

  let buff = Buffer.create 42
  let tokens = Queue.create ()

  let backtrack lexbuf = lexbuf.lex_curr_pos <- lexbuf.lex_start_pos;
    lexbuf.lex_curr_p <- lexbuf.lex_start_p

  let treat_eof () =
    if Queue.is_empty tokens then
      EOF
    else
      Queue.pop tokens
}

  let sp = [' '  '\t']
  let nl = "\r\n" | '\n'

  let sp_nl = sp | nl (* Space or newline *)

rule document = parse
  "(*" {Buffer.reset buff; Queue.push STARTCOM tokens; comment lexbuf}
  | _ as c {Buffer.reset buff; Buffer.add_char buff c; code lexbuf}
  | eof {treat_eof ()}

and comment = parse
  | eof {raise (Error "Expecting '*)'")}
  | "*)" {Queue.push (CONTENT (Buffer.contents buff)) tokens;
          Queue.push ENDCOM tokens; Queue.pop tokens}
  | _ as c {Buffer.add_char buff c; comment lexbuf}

and code = parse
  "(*"     {Queue.push (CONTENT (Buffer.contents buff)) tokens;
            Queue.push STARTCOM tokens; Buffer.reset buff; comment lexbuf}
  | eof    {Queue.push (CONTENT (Buffer.contents buff)) tokens; treat_eof ()}
  | _ as c {Buffer.add_char buff c; code lexbuf}
