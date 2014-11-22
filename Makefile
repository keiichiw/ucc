all:
	ocamlbuild main.native
	mv main.native ucc

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -f *~ ./test/*~ ucc
