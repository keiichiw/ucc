UCCDIR=../..

test: build/benz.out
	$(UCCDIR)/bin/sim -simple build/benz.out > log.txt
	diff expected.txt log.txt

build/benz.out:
	mkdir -p build
	cp $(UCCDIR)/extlib/benz-gaia/*.c build/
	cp -r $(UCCDIR)/extlib/benz-gaia/include build/
	cp main.c build/
	$(UCCDIR)/bin/ucc -Wa=-Wno-unused-label -I./build/include build/*.c
	mv a.out build/benz.out

clean:
	rm -rf build/ log.txt *~
