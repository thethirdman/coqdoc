INCLUDES=src,src/lib
PACKAGES= -use-ocamlfind -tag thread \
					-pkgs lablgtk2,lablgtksourceview2,lablgtksourceview2.gtksourceview2,unix,str,threads
all: coqdoc

coqdoc:
	ocamlbuild -Is ${INCLUDES} coqdoc.native ${PACKAGES}
