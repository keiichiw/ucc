# UCC - Udon C Compiler  [![Build Status](https://travis-ci.org/kw-udon/ucc.svg?branch=master)](https://travis-ci.org/kw-udon/ucc)

C compiler for [GAIA](https://github.com/wasabiz/GAIA3) CPU written in OCaml.

Requirement
==============
* OCaml 4.0.1
* ocamlbuild 4.0.1
* Clang

Build
==============
``make``

To run the tests, use the following command.

``make test``

Example
==========
``./bin/run ./test/fib-loop.c``

If you want to see an assembly, run the following command.

``./bin/ucc -S ./test/fib-loop.c``

Then, `./test/fib-loop.s` will be generated.

You can also run a test.

``prove ./test/fib-loop.c``
