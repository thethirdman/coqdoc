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
  let get_flush () =
    let str = Buffer.contents buff in
    Buffer.reset buff; (CONTENT str)

  let tok_lst =
    [("[",STARTVERNAC);
      ("]",ENDVERNAC);
      ("[[",STARTPP);
      ("]]",ENDPP);
      ("<<",STARTVERBATIM);
      (">>",ENDVERBATIM);
      ("----",HRULE);
      ("_",EMPHASIS);
      ("%",LATEX);
      ("$",LATEX_MATH);
      ("#",HTML);]
  let tok_htbl = Hashtbl.create 11

  let _ = List.iter (fun (key,tok) -> Hashtbl.add tok_htbl key tok) tok_lst
}

  let sp = [' '  '\t']
  let nl = "\r\n" | '\n'
  let tok_reg = "[" | "]" | "[[" | "]]" | "<<" | ">>" | "----" | "_" | "%"
              | "$" | "#"

  let sp_nl = sp | nl (* Space or newline *)

  rule document = parse
  "(**"  {Buffer.reset buff; Queue.push STARTDOC tokens; doc lexbuf}
  | "(*" {Buffer.reset buff; Queue.push STARTCOM tokens; comment lexbuf}
  | _ as c {Buffer.reset buff; Buffer.add_char buff c; code lexbuf}
  | eof {treat_eof ()}

and doc = parse
  | eof {raise (Error "Expecting '*)'")}
  | "*)" {Queue.push (get_flush ()) tokens;
          Queue.push ENDCOM tokens; Queue.pop tokens}
  | _ as c {Buffer.add_char buff c; doc lexbuf}

and comment = parse
  | eof {raise (Error "Expecting '*)'")}
  | "*)" {Queue.push (get_flush ()) tokens;
          Queue.push ENDCOM tokens; Queue.pop tokens}
  | _ as c {Buffer.add_char buff c; comment lexbuf}

and code = parse
  "(**"     {Queue.push (get_flush ()) tokens;
            Queue.push STARTDOC tokens; doc lexbuf}
  | "(*"     {Queue.push (get_flush ()) tokens;
            Queue.push STARTCOM tokens; comment lexbuf}
  | eof    {Queue.push (CONTENT (Buffer.contents buff)) tokens; treat_eof ()}
  | _ as c {Buffer.add_char buff c; code lexbuf}

and lex_doc = parse
  tok_reg as tok {Queue.push (get_flush ()) tokens;
                  Queue.push (Hashtbl.find tok_htbl tok) tokens;
                 Queue.pop tokens}
  | ("*"+ as lvl) ([^'\n']* as title) '\n'
    {Queue.push (get_flush ()) tokens;
      Queue.push (SECTION ((String.length lvl), title)) tokens; Queue.pop tokens}
    | (" "* as lvl) '-' ([^'\n']+ as elt)
    {Queue.push (get_flush ()) tokens;
      Queue.push (LST ((String.length lvl), elt)) tokens; Queue.pop tokens}
  | eof {Queue.push (get_flush ()) tokens; treat_eof ()}
  | _ as c {Buffer.add_char buff c; lex_doc lexbuf}
