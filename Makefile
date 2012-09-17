all: coqdoc

coqdoc:
	ocamlbuild coqdoc.native

clean:
	ocamlbuild -clean
