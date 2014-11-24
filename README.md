# UCC - Udon C Compiler #

C compiler for [Yebi](https://github.com/wasabiz/Yebi) CPU written in OCaml.

Compile
==============
``make``

You need OCaml and menhir.

Example
==========
``./ucc ./test/fib-loop.c``

Then ``./test/fib-loop.s`` will be created.