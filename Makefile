all: bin/cc lib/libucc.s bin/sim bin/as

bin/cc: FORCE
	ocamlbuild src/main.native
	mv main.native bin/cc

lib/libucc.s: lib/libucc.c bin/cc
	bin/ucc -s -o lib/libucc.s lib/libucc.c

bin/sim:
	git submodule update --init
	$(MAKE) -C extlib/gaia
	cp extlib/gaia/sim bin/sim
	$(MAKE) -C extlib/gaia clean

bin/as:
	cp extlib/gaia/asm.py bin/as

test: all test/*.c
	prove -j 4 test/*.c

test-benz:
	git submodule update --init
	$(MAKE) -C test/benz test

test-all: test test-benz

clean:
	ocamlbuild -clean
	rm -f *~ test/*~ test/*.i test/*.s test/*.out bin/cc bin/sim bin/as lib/libucc.s
	$(MAKE) -C test/benz clean

FORCE:
.PHONY: FORCE clean all test
