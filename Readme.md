# Photon
---------
A C based Lisp compiler / language. The design goal is to be fast and lightweight as C, while as powerfull and flexible as Lisp. The language very closely maps to the c type system and compiles any code written into c. The c code is then compiled by a c-compiler to produce efficient code. Currently the compiler used is TCC, which compiles to slow C.

Only works on 64bit Linux at the moment.

To test the compiler.
---------------------

1. Check out the code
2. cd photon
3. make
4. ./photon test.lisp
5. ./photon \#to start the REPL

V1 Release Checklist
--------
* [x] Naming symbols anything.
* [x] Global variables.
* [x] setf
* [x] floats / doubles
* [x] Loading lisp code
* [x] Proper symbols -  one symbol - one ID. Symbols to be used for functions, variable names. This meams that vars will be symbol based instead of string based.
* [x] Quoting 'symbolname
* [x] Opaque types
* [ ] c-macros
* [ ] Structs
 * [ ] Defining
 * [ ] member access
* [ ] Arrays / pointers
* [ ] sizeof macro.
* [ ] Lisp macros (tick/backtick syntax)
* [ ] Standard library
 * [ ] -ldl related
 * [ ] -lm related
* [ ] ffi
* [ ] enums
* [ ] function overloading macro
* [ ] loops (while expr body) result is the output of the last item.
* [ ] 
* [ ] ...



Vn Features
---------
* [ ] Packages / Modules
* [ ] libgccjit backend.
* [ ] (or) Use user selected compiler backend. Fallback to tcc if necessesary.
* [ ] offloading compiled code into a c-compatible static or dynamic link library and header file.
* [ ] Front end optimizations like tail call optimizations, function inlining and constant propagation. Note: pure functions can be evaluated during constant propagation.
* [ ] Bignums
* [ ] SSE support

License
------
See [License file](License.txt). 