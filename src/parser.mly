

%token EOF STARTCOM ENDCOM
%token <string> CONTENT

%start main /* FIXME: good return type */
%type <Vdoc.source> main

%%

main:
| STARTCOM CONTENT ENDCOM
  {Vdoc.Comment $2}
| CONTENT
  { Vdoc.Code $1 }
| EOF
  {raise Vdoc.End_of_file}
