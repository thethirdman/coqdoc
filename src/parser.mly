

%token EOF STARTCOM ENDCOM STARTDOC STARTVERNAC ENDVERNAC
%token STARTPP ENDPP STARTVERBATIM ENDVERBATIM HRULE
%token EMPHASIS LATEX LATEX_MATH HTML
%token <int*string> SECTION LST
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
STARTVERNAC CONTENT ENDVERNAC
  {Vdoc.Vernac $2}
| STARTPP CONTENT ENDPP
  {Vdoc.Pretty_print $2}
| SECTION
  {Vdoc.Section $1}
| LST
  {Vdoc.Elt_list $1}
| HRULE
  {Vdoc.Hrule}
| EMPHASIS CONTENT EMPHASIS
  {Vdoc.Emphasis $2}
| LATEX CONTENT LATEX
  {Vdoc.Raw {Vdoc.latex = $2; Vdoc.latex_math=""; Vdoc.html="";}}
| LATEX_MATH CONTENT LATEX_MATH
  {Vdoc.Raw {Vdoc.latex = ""; Vdoc.latex_math=$2; Vdoc.html="";}}
| HTML CONTENT HTML
  {Vdoc.Raw {Vdoc.latex = ""; Vdoc.latex_math=""; Vdoc.html=$2;}}
| CONTENT
  { Vdoc.Content $1}
