all: coqdoc

coqdoc:
	ocamlbuild coqdoc.native

debug:
	ocamlbuild coqdoc.byte

test: coqdoc
	make -C test-suite

clean:
	ocamlbuild -clean
