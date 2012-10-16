{
  open Lexing
  open Parser

  exception Error of string

  let buff = Buffer.create 42
  let tokens = Queue.create ()
  let lst_lvl = ref (-1)

  let backtrack lexbuf = lexbuf.lex_curr_pos <- lexbuf.lex_start_pos;
    lexbuf.lex_curr_p <- lexbuf.lex_start_p

  let treat_eof () =
    if Queue.is_empty tokens then
      EOF
    else
      Queue.pop tokens

  let get_flush () =
    let str = Buffer.contents buff in
    Buffer.clear buff; Queue.push (CONTENT str) tokens

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
    (tok_reg as tok) {get_flush ();
                  Queue.push (Hashtbl.find tok_htbl tok) tokens;
                 Queue.pop tokens}
  | '@' (name as query) '{' (_* as arglist) '}'
    {get_flush (); Queue.push (QUERY (query,arglist)) tokens; Queue.pop tokens}
  | ("*"+ as lvl) ' ' ([^'\n']* as title)
    {get_flush ();
      Queue.push (SECTION ((String.length lvl), title)) tokens; Queue.pop tokens}
  | (sp* as lvl) "- "
    {get_flush (); lst_lvl := (String.length lvl);
      Queue.push (LST !lst_lvl) tokens; Queue.pop tokens}
  | (sp* as lvl) {let cur_lvl = String.length lvl in
    if cur_lvl = !lst_lvl then
      begin
        Buffer.add_string buff lvl; lex_doc lexbuf
      end
    else if !lst_lvl <> (-1) then
      begin
        Queue.push ENDLST tokens; Buffer.add_string buff lvl; lst_lvl := -1;
        lex_doc lexbuf
      end
    else
      begin
        Buffer.add_string buff lvl; lex_doc lexbuf
      end}
  | eof { (if (Buffer.length buff <> 0) then
            get_flush ()) ; treat_eof ()}
  | "(*" | "(**" | "*)" as elt {Buffer.add_string buff elt; lex_doc lexbuf}
  | _ as c {Buffer.add_char buff c; lex_doc lexbuf}
