all: cc libucc.s

cc: FORCE
	ocamlbuild main.native
	mv main.native cc

libucc.s: libucc.c intrinsics.s cc
	./cc libucc.c
	cat intrinsics.s >> libucc.s

clean:
	ocamlbuild -clean
	rm -f *~ ./test/*~ ./test/*.out cc libucc.s

FORCE:
.PHONY: FORCE clean all
