{
  open Lexing
  open Parser

  exception Error of string

  let buff = Buffer.create 42
  let tokens = Queue.create ()

  let lst_lvl = ref []
  let push elt = lst_lvl := elt::!lst_lvl
  let pop () = match !lst_lvl with [] -> () | e::l -> lst_lvl := l
  let get_lvl () = match !lst_lvl with [] -> -1 | e::l -> e

  (*let pop_lvl () = match !lst_lvl with
    [] -> raise (Invalid_argument "pop_lvl")
    | [e] -> e
    | e::l -> lst_lvl := l; e
  let push_lvl e = lst_lvl := e::!lst_lvl*)

  let backtrack lexbuf = lexbuf.lex_curr_pos <- lexbuf.lex_start_pos;
    lexbuf.lex_curr_p <- lexbuf.lex_start_p

  let treat_eof () =
    if Queue.is_empty tokens then
      if !lst_lvl <> [] then (pop (); ENDLST) else EOF
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

let tex_lst =
  [("coq_example", STARTEXAMPLE);
   ("coq_examle*", STARTEXAMPLESTAR);
   ("coq_eval", STARTEVAL)]

let _ = List.iter (fun (key,tok) -> Hashtbl.add tok_htbl key tok) tok_lst

}

let sp = [' '  '\t']
let nl = "\r\n" | '\n'
let tok_reg = "[" | "]" | "[[" | "]]" | "<<" | ">>" | "----" | "_" | "%"
            | "$" | "#"

let nl_end = nl | "*)"
let sp_nl = sp | nl (* Space or newline *)
let name = ['a'-'z''A'-'Z''0'-'9']+

let tex_reg = "coq_example" | "coq_example*" | "coq_eval"

rule lex_doc = parse
  (tok_reg as tok) {get_flush ();
                Queue.push (Hashtbl.find tok_htbl tok) tokens;
               Queue.pop tokens}
| '@' (name as query) '{' (_* as arglist) '}'
  {get_flush (); Queue.push (QUERY (query,arglist)) tokens; Queue.pop tokens}
| ("*"+ as lvl) ' ' ([^'\n']* as title)
  {get_flush ();
    Queue.push (SECTION ((String.length lvl), title)) tokens; Queue.pop tokens}
| nl (sp* as lvl) "- " {get_flush ();
  let depth = String.length lvl in 
  if depth > (get_lvl ()) then (* New sublist *)
    (Queue.push (LST depth) tokens; Queue.push ITEM tokens;
    push depth;
    Queue.pop tokens)
  else if depth < (get_lvl ()) then (* End of sublist *)
    (Queue.push ENDLST tokens;
    Queue.push ITEM tokens;
    (*Queue.push (lex_doc lexbuf) tokens ;*)
    pop ();
    Queue.pop tokens)
  else (* Another item *)
    (Queue.push ITEM tokens; Queue.pop tokens)}

| nl as n { Buffer.add_string buff n; lex_doc lexbuf }

| eof { (if (Buffer.length buff <> 0) then get_flush ()); treat_eof ()}
| "(*" | "(**" | "*)" as elt {Buffer.add_string buff elt; lex_doc lexbuf}
| _ as c {Buffer.add_char buff c; lex_doc lexbuf}

and lex_tex = parse
  ("\\begin{" sp* (tex_reg as env) sp* "}") as sentence
  {
    try
      get_flush (); Queue.push (List.assoc env tex_lst) tokens; Queue.pop tokens
    with Not_found -> Buffer.add_string buff sentence; lex_tex lexbuf
    }
  | ("\\end{" sp* (tex_reg as env) sp* "}") as sentence
  {
    get_flush ();
    try ignore (List.assoc env tex_lst); Queue.push ENDTEX tokens ; Queue.pop tokens
    with Not_found -> Buffer.add_string buff sentence; lex_tex lexbuf
  }
  | eof { (if (Buffer.length buff <> 0) then
            get_flush ()) ; treat_eof ()}
  | _ as c {Buffer.add_char buff c; lex_tex lexbuf}
