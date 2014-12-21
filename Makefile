all: bin/cc lib/libucc.s bin/sim bin/as

bin/cc: FORCE
	ocamlbuild src/main.native
	mv main.native bin/cc

lib/libucc.s: lib/libucc.c lib/intrinsics.s bin/cc
	bin/ucc -s lib/libucc.c
	cat lib/intrinsics.s >> lib/libucc.s

bin/sim:
	git submodule update --init
	$(MAKE) -C extlib/gaia
	cp extlib/gaia/sim bin/sim
	$(MAKE) -C extlib/gaia clean

bin/as:
	cp extlib/gaia/asm.py bin/as

test: all test/*.c
	prove test/*.c

clean:
	ocamlbuild -clean
	rm -f *~ test/*~ test/*.out bin/cc bin/sim bin/as lib/libucc.s

FORCE:
.PHONY: FORCE clean all test
