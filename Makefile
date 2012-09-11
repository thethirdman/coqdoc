LIBS=unix,str

all: coqdoc

coqdoc:
	ocamlbuild -I src coqdoc.native -libs ${LIBS}
