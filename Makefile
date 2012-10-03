OPT=-menhir
all: coqdoc

coqdoc:
	ocamlbuild ${OPT} coqdoc.native

debug:
	ocamlbuild ${OPT} coqdoc.byte

clean:
	ocamlbuild -clean
