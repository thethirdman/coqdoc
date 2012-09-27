

%token EOF
%token <string> CODE DOC

%start main /* FIXME: good return type */
%type <Vdoc.source> main

%%

main:
DOC
  { Vdoc.Doc $1 }
| CODE
  { Vdoc.Code $1 }
| EOF
  {raise Vdoc.End_of_file}
