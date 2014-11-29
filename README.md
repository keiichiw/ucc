# UCC - Udon C Compiler #

C compiler for [Yebi](https://github.com/wasabiz/Yebi) CPU written in OCaml.

Compile
==============
``make``

You need OCaml and menhir.

Example
==========
``./bin/ucc ./test/fib-loop.c``

Then ``./test/fib-loop.out`` will be created.

You can simulate by running the following command.

``./bin/sim ./test/fib-loop.out``
