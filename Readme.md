# Photon
---------
A C based Lisp compiler / language. The design goal is to be fast and lightweight as C, while as powerful and flexible as Lisp. The language very closely maps to the c type system and compiles into c. The c code is then compiled by a compiler to produce efficient(-ish) code. Currently the compiler used is TCC, which generates relatively slow assembly code.

This compiler is in a very early stage of development, please dont expect anything to work yet.

Check out the file [test.lisp](test.lisp) to see the current functionality.

Only works on 64bit Linux at the moment.

To test the compiler.
---------------------
1. mkdir photon
2. cd photon
3. git clone git@github.com:rolfrm/Photon.git
4. git clone git@github.com:rolfrm/iron.git
5. cd Photon
6. make
7. ./foton test.lisp
8. ./foton 

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
* [x] c-macros - Register the functions/types needed to work with macros.
* [x] Equality "eq"
* [x] conditionals "if"
* [x] loops (while expr body) result is the output of the last item.
* [x] a better REPL (history, multi-line, line-edit)
* [ ] proper literal strings
* [x] Structs
 * [x] Defining - structs can be defined using the type macro
 * [ ] member access
* [x] Arrays / pointers - c-style deref
* [x] sizeof macro - size calculated at compile time.
* [x] Lisp macros (tick/backtick syntax) choosen expr/unexpr equal of tick/backtick
* [ ] ffi
* [ ] Standard library - use ffi to bind c std lib
 * [ ] libc
 * [ ] -ldl related
 * [ ] -lm related
* [ ] enums
* [ ] function overloading macro
* [ ] Fix massive memory leaks
* [ ] Bugs / Stability
* [ ] line number of exprs.

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