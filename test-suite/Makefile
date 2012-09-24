TESTFILES=sections lists pretty_printing hrule emphasis verbatim html \
					hide show latex
DIFFTOOL=diff

RESFILES=$(addsuffix .result, ${TESTFILES})

all: ${RESFILES}

show.result: show.v
	@mkdir -p log result
	@coqdoc -g $< -o result/$@ 2> /dev/null

latex.result: latex.v
	@mkdir -p log result
	@coqdoc -latex -g $< -o result/$@ 2> /dev/null


%.result: doc/%.v
	@mkdir -p log result
	@printf "Testing $< \n"; \
	REF=`mktemp ref.XXX`; \
	NEW=`mktemp new.XXX`; \
	coqdoc $< --utf8 -o result/$@ 2>>coqdoc.log; \
	cp $${REF} $${NEW}; \
	RET=`$${DIFFTOOL} diff $${REF} $${NEW} > log/$@.log` ; \
	rm $${NEW} $${REF}

clean:
	rm -f $(addprefix result/, ${RESFILES}) result/coqdoc.* log/*.log

distclean:
	rm -rf log result
