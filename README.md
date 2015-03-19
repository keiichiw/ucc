# UCC - Udon C Compiler  [![Build Status](https://travis-ci.org/kw-udon/ucc.svg?branch=master)](https://travis-ci.org/kw-udon/ucc)

C compiler for [GAIA](https://github.com/wasabiz/GAIA3) CPU written in OCaml.

Requirement
==============
* OCaml 4.0.1
* ocamlbuild 4.0.1
* Clang (for preprocessing and syntax check)

Build
==============
``make``

To run the tests, use the following command.

``make test``

Example
==========
You can compile C file by the following command.

``./bin/ucc ./test/printf.c``

Then, `a.out` will be created. It runs on the simulator.

``./bin/sim -simple a.out``

You can check generated assembly by `-S` option.

 ``./bin/ucc -S ./test/printf.c``

If you want to see other options, please use `-h` option.
