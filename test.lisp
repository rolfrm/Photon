;; The following code runs / compiles

;; Constants
1 
"hello world!"

;; Function calls
(write_line "hello world!")
(i64+ 10 10)

;; Defining functions
(defun add (f64 (a f64) (b f64)) (f+ a b))
(add (cast 45 f64) (cast 67 f64))

;; Variables and functions can be named any unicode thing, except things starting with '('/')'
;; and they cannot contain whitespace either
(defun + (i64 (a i64) (b i64))
  (i64+ a b))
(+ 1 5)

;; Defining / using variables
(defvar test1 1)
(defvar test2 (+ test1 1))
(defvar test3 (+ test1 test2))
(defvar test4 (+ test3 test2))
(defvar test5 (+ test4 test3))
(defvar test6 (+ test5 test4))
(defvar test7 (+ test6 test5))
(defvar test8 (+ test7 test6))

(+ test8 test8)
(setf test8 100) ; Setting a variable
(+ test8 test7)

;; Shallow comparison

(defvar a 1)
(defvar b 2)
(eq a b)
(eq b b)


;; Types
(type f64) ; returns the type of f64 (double).
(type (ptr f64)) ; a pointer to an f64. (double *)

(type (fcn (ptr f64))) ;a function that returns a pointer t an f64 and takes no args.

(type (fcn f64 (a f64) (b f64))) ; a function that returns a f64 and takes two f64s. 
(print_type (type (struct _vec2 (x f32) (y f32)))) ; this actually defines a struct named _vec2.
(type (alias (ptr _vec2) vec2)) ; defines vec2 as a _vec2 struct.
(defvar xy :type vec2)

;; Types can be compared
(write_line "Should be true..")
(eq (type (ptr _vec2)) (type (ptr _vec2)))
(write_line "Should be false..")
(eq (type (ptr _vec2)) (type vec2))

;; Casting variables to different type
(cast "asd" (ptr i32))
(cast 0.001 f32) ; Currently the only valid way of creating a float
(cast 2.5 f64)

;; everything is symbol based
(quote hello) ; creats a symbol named 'hello'
(get-symbol "hello") ; creates a symbol from a string.

;; files can be loaded and executed
(load "test2.lisp")


;; (defcmacro overload (void (sym expr) (fcn expr))
;;   (progn
;;     (assert (is-symbol fcn))
;;     (assert (is-symbol sym))
;;     (register-overload (get-symbol sym) (get-function (get-symbol fcn)))))
;; (overload + f+)
;; (overload + i64+)

(defcmacro one_expr (expr1) expr1)
(expand one_expr (write_line "??"))
(defcmacro two-expr (expr1 expr2)
  expr1)
(expr 1)
(expr (write_line "???"))
(defcmacro no-expr ()
   (expr (progn (write_line "???") (write_line "!!!")))) 
(expand no-expr)
(expand no-expr)

;; a macro is really just a function that takes exprs and compiles them to code.
;; unexpr is evaluated at 
(defcmacro fun-expr (expr1 expr2)
  (expr (progn
	  (write_line (unexpr expr1))
	  (unexpr expr2))))
; `(write_line ,expr1))
;(expr (unexpr (expr (unexpr "hello?")))))

(expand fun-expr "hello" (write_line "WORLD!"))

(if (eq 2 1) 2 3)
(defvar a 0)
(defun not (bool (x bool)) (eq (cast 0 bool) x))

(while (not (eq a 10))
  (progn 
    (setf a (+ a 1))
    (if (eq a 5)
	(progn (write_line "aaa") 1)
	(progn (write_line "bbb") 2))
    a))

(deref "asd")
;; (defvar libc (load-lib "libc")) ;dlopen
;; (defext libc malloc (fcn (ptr void) (size u64))) ;getsym??
;; (defext libc free (fcn void (ptr (ptr void))))
;; (free (malloc 10))
;; (unload-lib libc) ;dlclose
(defvar libm (load-lib "libm.so"))
(load-symbol libm (quote cos) (quote cos) (type (fcn f64 (x f64))))
(cos 3.14)
