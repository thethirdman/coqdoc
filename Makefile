all: coqdoc

coqdoc:
	ocamlbuild coqdoc.native

debug:
	ocamlbuild coqdoc.byte

clean:
	ocamlbuild -clean
