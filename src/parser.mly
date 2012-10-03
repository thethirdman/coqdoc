

%token EOF STARTCOM ENDCOM STARTDOC STARTVERNAC ENDVERNAC
%token STARTPP ENDPP STARTVERBATIM ENDVERBATIM HRULE
%token EMPHASIS LATEX LATEX_MATH HTML ENDLST
%token <int> LST
%token <int*string> SECTION
%token <string> CONTENT

%start main parse_doc /* FIXME: good return type */
%type <Vdoc.source> main
%type <Vdoc.doc> parse_doc

%%

main:
STARTCOM CONTENT ENDCOM
  {Vdoc.Comment $2}
| STARTDOC CONTENT ENDCOM
  {Vdoc.Doc $2}
| CONTENT
  { Vdoc.Code $1 }
| EOF
  {raise Vdoc.End_of_file}

parse_doc:
  lst = list(parse_term) EOF
    {Vdoc.List lst}
  | EMPHASIS lst=list (parse_term) EMPHASIS
    {Vdoc.Emphasis lst}
  | LST lst=list(parse_term) ENDLST
    {Vdoc.Item $1,lst}

parse_term:
STARTVERNAC CONTENT ENDVERNAC
  {Vdoc.Vernac $2}
| STARTPP CONTENT ENDPP
  {Vdoc.Pretty_print $2}
| STARTVERBATIM CONTENT ENDVERBATIM
  {Vdoc.Verbatim $2}
| SECTION
  {Vdoc.Section $1}
| HRULE
  {Vdoc.Hrule}
| LATEX CONTENT LATEX
  {Vdoc.Raw {Vdoc.latex = $2; Vdoc.latex_math=""; Vdoc.html="";}}
| LATEX_MATH CONTENT LATEX_MATH
  {Vdoc.Raw {Vdoc.latex = ""; Vdoc.latex_math=$2; Vdoc.html="";}}
| HTML CONTENT HTML
  {Vdoc.Raw {Vdoc.latex = ""; Vdoc.latex_math=""; Vdoc.html=$2;}}
| CONTENT
  {Vdoc.Content $1}
(*| EOF
  {Vdoc.List []}*)
