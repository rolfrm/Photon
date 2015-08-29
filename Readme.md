# Photon
---------
Photon is a language and compiler. It is statically typed with a type system that is closely related to that of C. It is also dynamically executed in a way that is similar to most Lisp implementations. It aims to provide the performance and simplicity of C while having the flexibility and power of Lisp. 

The compiler compiles the Photon code into C code. The C code is then compiled by a compiler to produce efficient(-ish) native code. Currently the compiler used is TCC, which generates relatively slow assembly code.

This compiler is in a very early stage of development, please dont expect anything to work yet.

To test the compiler I am trying to build some games examples of these are 'growth.lisp' and 'simple_game.lisp'.

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
7. ./foton growth.lisp ;; run the 'growth' game.
8. ./foton repl.lisp ;; run the REPL.

V1 Release Checklist (Aiming at August 21 release)
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
* [x] Structs
 * [x] Defining - structs can be defined using the type macro
 * [x] member access
* [x] Arrays / pointers - c-style deref
* [x] address off. (ref obj)
* [x] sizeof macro - size calculated at compile time.
* [x] Lisp macros (tick/backtick syntax) choosen expr/unexpr equal of tick/backtick
* [x] ffi
* [x] literal hex values
* [x] Standard library - use ffi to bind c std lib
 * [x] libc
 * [x] -ldl related
 * [x] -lm related
* [x] function overloading macro / varadic functions
* [x] macros that do not require 'expand
* [x] proper function printing (needed for callback functions). using printers, a bit slow..
* [x] faster type-of macro (current version requires compiling the code to check the types. Better to just do it once. Memorizing?
* [x] Call function pointers
* [x] proper interned literal strings (checkout tries. no probably to hash tables instead.)
* [ ] Waste less memory
* [x] Bugs / Stability	 	 
* [ ] better error handling
* [ ] line number of exprs.
* [ ] lvalue checking (setf 1 10) is invalid for instance. (setf a 10) might be ok.
* [x] Windows 64bit support.

Future Features
---------
* [ ] Windows/Linux 32 bit support
* [ ] Recursion support
* [ ] Packages / Modules
* [ ] libgccjit backend.
* [ ] (and/or) Use user selected compiler backend. Fallback to tcc if necessesary.
* [ ] Compile working image into a c-compatible static or dynamic link library and header file.
* [ ] Front end optimizations like tail call optimizations, function inlining and constant propagation. Note: pure functions can be evaluated during constant propagation.
* [ ] Bignums (can be built into the language)
* [ ] inline assembly
* [ ] SSE support (made with inline assembly)
* [ ] enums

License
------
See [License file](License.txt). 