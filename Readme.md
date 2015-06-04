# Photon
---------
A c based Lisp compiler / language. It should be fast and lightweight, hence the name "Photon". Additionally, the language should be as minimal and flexible as possible. The language very closely maps to the c type system and compiles any code written into c. The c code is then compiled by a c-compiler to produce efficient code.

Only works on 64bit Linux at the moment.

To test the compiler.
---------------------

1. Check out the code
2. cd photon
3. make
4. ./photon test.lisp
5. ./photon \#to start the REPL

Short Term Features
--------
* [x] Naming symbols anything.
* [ ] Function overloading
* [x] Global variables.
* [x] setf
* [x] floats / doubles
* [ ] Loading lisp code
* [ ] Proper symbols
* [ ] Lisp macros (tick/backtick syntax)
* [ ] Structs
 * [ ] Defining
 * [ ] member access
* [ ] Arrays / pointers
* [ ] sizeof macro.
* [ ] Standard library
 * [ ] -ldl related
 * [ ] -lm related
* [ ] ffi
* [ ] enums
* [ ] ...


Long Term Features
---------
* [ ] Packages / Modules
* [ ] libgccjit backend.
* [ ] offloading compiled code into a c-compatible static or dynamic link library and header file.
* [ ] Front end optimizations like tail call optimizations, function inlining and constant propagation. Note: pure functions can be evaluated during constant propagation.
* [ ] Bignums
