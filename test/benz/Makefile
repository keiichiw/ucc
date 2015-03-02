UCCDIR=../..

test: build/benz.out
	$(UCCDIR)/bin/sim -simple build/benz.out > log.txt
	diff expected.txt log.txt

build/benz.out:
	mkdir -p build
	cp $(UCCDIR)/extlib/benz-gaia/*.c build/
	cp -r $(UCCDIR)/extlib/benz-gaia/include build/
	rm build/main.c
	$(UCCDIR)/bin/ucc -Wa=-Wno-unused-label -I./build/include build/*.c main.c
	mv a.out build/benz.out

build/repl.out: repl.c
	mkdir -p build
	cp $(UCCDIR)/extlib/benz-gaia/*.c build/
	cp -r $(UCCDIR)/extlib/benz-gaia/include build/
	rm build/main.c
	$(UCCDIR)/bin/ucc -Wa=-Wno-unused-label -I./build/include build/*.c repl.c
	mv a.out build/repl.out

clean:
	rm -rf build/ log.txt *~
