# Photon
---------
A c based Lisp compiler / language. It should be fast and lightweight, hence the name "Photon". Additionally, the language should be as minimal and flexible as possible. The language very closely maps to the c type system and compiles any code written into c. The c code is then compiled by a c-compiler to produce efficient code.

Only works on 64bit Linux at the moment.

Short Term Features
--------
* [x] Naming functions anything.
* [ ] floats / doubles
* [ ] Function overloading
* [ ] Loading / defining functions dynamically
* [ ] Loading lisp code
* [ ] Proper symbols
* [ ] Lisp macros (tick/backtick syntax)
* [ ] Structs
* [ ] ...

Long Term Features
---------
* [ ] libgccjit backend.
* [ ] offloading compiled code into a c-compatible dll and header file.
* [ ] Front end optimizations like tail call optimizations, function inlining and constant propagation. Note: pure functions can be evaluated during constant propagation.
To test the compiler.
---------------------

1. Check out the code
2. cd photon
3. make
4. ./photon test.lisp
5. ./photon \#to start the REPL
