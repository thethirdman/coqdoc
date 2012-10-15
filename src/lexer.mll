{
  open Lexing
  open Parser

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
  let name = ['a'-'z''A'-'Z''0'-'9']+

rule lex_doc = parse
    (tok_reg as tok) {Queue.push (get_flush ()) tokens;
                  Queue.push (Hashtbl.find tok_htbl tok) tokens;
                 Queue.pop tokens}
  | "@let " (name as name) "=" (['0'-'9']+ as elt)
  {Queue.push (get_flush ()) tokens;
      Queue.push (LET (name, elt)) tokens; Queue.pop tokens}
  | "@print{" (name as name) "}"
  {Queue.push (get_flush ()) tokens;
      Queue.push (PRINT name) tokens; Queue.pop tokens}
  | ("*"+ as lvl) ' ' ([^'\n']* as title)
    {Queue.push (get_flush ()) tokens;
      Queue.push (SECTION ((String.length lvl), title)) tokens; Queue.pop tokens}
  (*| (sp* as lvl) '-'*
    {Queue.push (get_flush ()) tokens;
      Queue.push (LST (String.length lvl)) tokens; lex_doc lexbuf; (* FIXME*)
      Queue.push ENDLST tokens; Queue.pop tokens}*)
  | eof { (if (Buffer.length buff <> 0) then
            Queue.push (get_flush ()) tokens); treat_eof ()}
  | "(*" | "(**" | "*)" as elt {Buffer.add_string buff elt; lex_doc lexbuf}
  | _ as c {Buffer.add_char buff c; lex_doc lexbuf}
