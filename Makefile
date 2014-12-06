all: bin/cc lib/libucc.s bin/sim

bin/cc: FORCE
	ocamlbuild src/main.native
	mv main.native bin/cc

lib/libucc.s: lib/libucc.c lib/intrinsics.s bin/cc
	bin/cc lib/libucc.c
	cat lib/intrinsics.s >> lib/libucc.s

bin/sim:
	$(MAKE) -C extlib/sim/Yebi
	cp extlib/sim/Yebi/ysim bin/sim
	$(MAKE) -C extlib/sim/Yebi clean

clean:
	ocamlbuild -clean
	rm -f *~ test/*~ test/*.out bin/cc bin/sim lib/libucc.s

FORCE:
.PHONY: FORCE clean all
