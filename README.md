# UCC - Udon C Compiler  [![Build Status](https://travis-ci.org/kw-udon/ucc.svg?branch=master)](https://travis-ci.org/kw-udon/ucc)

C compiler for [Yebi](https://github.com/wasabiz/Yebi) CPU written in OCaml.

Requirement
==============
* OCaml 4.0.1
* ocamlbuild 4.0.1
* Menhir 20140422

Build
==============
``make``

To run the tests, use the following command.

``make test``

Example
==========
``./bin/ucc ./test/fib-loop.c``

Then ``./test/fib-loop.out`` will be created.

(If you use `./bin/cc` instead of `./bin/ucc`, assembly `./test/fib-loop.s`  will be generated.)

By the following command, you can run this binary on simulator.

``./bin/sim ./test/fib-loop.out``

You can also run a test.

``prove ./test/fib-loop.c``
