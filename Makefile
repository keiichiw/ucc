all:
	ocamlbuild main.native
clean:
	ocamlbuild -clean
	rm -f *~
