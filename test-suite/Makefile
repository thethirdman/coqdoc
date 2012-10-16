TESTFILES=sections lists pretty_printing hrule emphasis verbatim html \
					hide show latex nested_com nested_emph query
DIFFTOOL=diff
COQDOC=../coqdoc.native
TESTER=tester.py

RESFILES=$(addsuffix .result, ${TESTFILES})

all: $(addsuffix .v, ${TESTFILES})

%.v:
	@-${COQDOC} doc/$@ > out.txt
	@${TESTER} doc/$@ out.txt

clean:
	rm log/*
