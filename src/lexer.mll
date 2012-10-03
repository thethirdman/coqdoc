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
    Buffer.clear buff; (CONTENT str)

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

  let counter = ref 0

  let _ = List.iter (fun (key,tok) -> Hashtbl.add tok_htbl key tok) tok_lst
}

  let sp = [' '  '\t']
  let nl = "\r\n" | '\n'
  let tok_reg = "[" | "]" | "[[" | "]]" | "<<" | ">>" | "----" | "_" | "%"
              | "$" | "#"

  let nl_end = nl | "*)"
  let sp_nl = sp | nl (* Space or newline *)

  rule document = parse
  "(**" {Buffer.clear buff; Queue.push STARTDOC tokens; counter := 0;
      doc lexbuf}
  | "(*" {Buffer.clear buff; Queue.push STARTCOM tokens; counter := 0;
      comment lexbuf}
  | _ as c {Buffer.clear buff; Buffer.add_char buff c; code lexbuf}
  | eof {treat_eof ()}

and doc = parse
  | eof {raise (Error "Expecting '*)'")}
  | "*)" {if !counter = 0 then
            (Queue.push (get_flush ()) tokens; Queue.push ENDCOM tokens;
            Queue.pop tokens)
          else
            (Buffer.add_string buff "*)"; decr counter; doc lexbuf)}
  | "(**" | "(*" as elt {incr counter; Buffer.add_string buff elt; doc lexbuf}
  | _ as c {Buffer.add_char buff c; doc lexbuf}

and comment = parse
  | eof {raise (Error "Expecting '*)'")}
  | "*)" { if !counter = 0 then
            (Queue.push (get_flush ()) tokens; Queue.push ENDCOM tokens;
              Queue.pop tokens)
          else
            (Buffer.add_string buff "*)"; decr counter; comment lexbuf)}
  | "(**" | "(*" as elt {incr counter; Buffer.add_string buff elt; comment lexbuf}
  | _ as c {Buffer.add_char buff c; comment lexbuf}

and code = parse
  "(**" {Queue.push (get_flush ()) tokens; counter := 0;
            Queue.push STARTDOC tokens; doc lexbuf}
  | "(*"     {Queue.push (get_flush ()) tokens; counter := 0;
            Queue.push STARTCOM tokens; comment lexbuf}
  | eof    {Queue.push (CONTENT (Buffer.contents buff)) tokens; treat_eof ()}
  | _ as c {Buffer.add_char buff c; code lexbuf}

and lex_doc = parse
  sp_nl* (tok_reg as tok) sp_nl* {Queue.push (get_flush ()) tokens;
                  Queue.push (Hashtbl.find tok_htbl tok) tokens;
                 Queue.pop tokens}
  | sp* ("*"+ as lvl) ' ' ([^'\n']* as title)
    {Queue.push (get_flush ()) tokens;
      Queue.push (SECTION ((String.length lvl), title)) tokens; Queue.pop tokens}
  | (sp* as lvl) '-' ([^'\n']+ as elt) nl_end
    {Queue.push (get_flush ()) tokens;
      Queue.push (LST ((String.length lvl), elt)) tokens; lex_doc lexbuf;
      Queue.push ENDLST; Queue.pop tokens}
  | eof { (if (Buffer.length buff <> 0) then
            Queue.push (get_flush ()) tokens); treat_eof ()}
  | "(*" | "(**" | "*)" as elt {Buffer.add_string buff elt; lex_doc lexbuf}
  | _ as c {Buffer.add_char buff c; lex_doc lexbuf}
