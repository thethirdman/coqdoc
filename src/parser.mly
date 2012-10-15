%token EOF STARTCOM ENDCOM STARTDOC STARTVERNAC ENDVERNAC
%token STARTPP ENDPP STARTVERBATIM ENDVERBATIM HRULE
%token EMPHASIS LATEX LATEX_MATH HTML ENDLST
%token <int> LST
%token <int*string> SECTION
%token <string> CONTENT
%token <string*string> LET

%start main parse_doc /* FIXME: good return type */
%type <Cst.source> main
%type <Cst.doc> parse_doc

%{
  let merge_contents lst = List.fold_right (fun a b -> a^b) lst ""
%}

%%


main:
STARTCOM list(CONTENT) ENDCOM
  {Cst.Raw_Comment (merge_contents $2)}
| STARTDOC list(CONTENT) ENDCOM
  {Cst.Raw_Doc (merge_contents $2)}
| CONTENT
  { Cst.Raw_Code $1 }
| EOF
  {raise Cst.End_of_file}


parse_doc:
  lst = list(parse_seq) EOF
    {Cst.List lst}

parse_seq:
  term = parse_term
    {term}
  | EMPHASIS lst=list (parse_term) EMPHASIS
    {Cst.Emphasis (Cst.List lst)}
  | LST lst=list(parse_term) ENDLST
    {Cst.Item ($1,(Cst.List lst))}

parse_term:
STARTVERNAC CONTENT ENDVERNAC
  {Cst.Vernac $2}
| STARTPP CONTENT ENDPP
  {Cst.Pretty_print $2}
| STARTVERBATIM list(CONTENT) ENDVERBATIM
  {Cst.Verbatim (merge_contents $2)}
| SECTION
  {Cst.Section $1}
| HRULE
  {Cst.Hrule}
| LATEX CONTENT LATEX
  {Cst.Raw {Cst.latex = $2; Cst.latex_math=""; Cst.html="";}}
| LATEX_MATH CONTENT LATEX_MATH
  {Cst.Raw {Cst.latex = ""; Cst.latex_math=$2; Cst.html="";}}
| HTML CONTENT HTML
  {Cst.Raw {Cst.latex = ""; Cst.latex_math=""; Cst.html=$2;}}
| CONTENT
  {Cst.Content $1}
| name = LET
  {Cst.Let name}
(*| EOF
  {Vdoc.List []}*)
