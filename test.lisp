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
(defvar libc (load-lib "/lib/x86_64-linux-gnu/libc.so.6"))
(load-symbol libc (quote printf) (quote printf) (type (fcn void (fmt (ptr char)) (x i64))))
(load-symbol libc (quote usleep) (quote usleep) (type (fcn void (time i32))))
(write_line "asd")
(printf "test: %i\n" 5)
(write_line "the following works if libglfw is installed")

(defvar libglfw (load-lib "libglfw.so"))
(load-symbol libglfw (quote glfw-init) (quote glfwInit) (type (fcn void)))
(load-symbol libglfw (quote glfw-create-window) (quote glfwCreateWindow) 
	     (type (fcn (ptr void) (width i32) (height i32) 
			(title (ptr char)) (a (ptr void)) (b (ptr void)))))
(load-symbol libglfw (quote glfw-swap-buffers) (quote glfwSwapBuffers) 
	     (type (fcn void (a ( ptr void)))))
(load-symbol libglfw (quote glfw-make-current) (quote glfwMakeContextCurrent) (type (fcn void (win (ptr void)))))

(defvar libgl (load-lib "libGL.so"))
(load-symbol libgl (quote gl-clear) (quote glClear) (type (fcn void (mask i32))))
(load-symbol libgl (quote gl-clear-color) (quote glClearColor) 
	     (type (fcn void (r f32) (g f32) (b f32) (a f32))))

(glfw-init)
(defvar win (glfw-create-window (cast 512 i32) (cast 512 i32) "test.." (cast 0 (ptr void)) (cast 0 (ptr void))))

(glfw-make-current win)

(defvar r 0.0)
(progn
  (gl-clear-color (cast 255.0 f32) (cast 1.0 f32) (cast 1.0 f32) (cast 1.0 f32))
  (gl-clear (cast 16384 i32))
  (glfw-swap-buffers win)
  (usleep (cast 250000 i32))
  (gl-clear-color (cast 0.0 f32) (cast 0.0 f32) (cast 1.0 f32) (cast 1.0 f32))
  (gl-clear (cast 16384 i32))
  (glfw-swap-buffers win)
  (usleep (cast 250000 i32))
  (gl-clear-color (cast 0.0 f32) (cast 0.0 f32) (cast 1.0 f32) (cast 1.0 f32))
  (gl-clear (cast 16384 i32))
  (glfw-swap-buffers win)
  (usleep (cast 250000 i32))
  (gl-clear-color (cast 1.0 f32) (cast 0.0 f32) (cast 0.0 f32) (cast 1.0 f32))
  (gl-clear (cast 16384 i32))
  (glfw-swap-buffers win)
  (usleep (cast 250000 i32))
  (gl-clear-color (cast 0.0 f32) (cast 1.0 f32) (cast 0.0 f32) (cast 1.0 f32))
  (gl-clear (cast 16384 i32))
  (glfw-swap-buffers win)
  (usleep (cast 250000 i32))
  (gl-clear-color (cast 255.0 f32) (cast 1.0 f32) (cast 1.0 f32) (cast 1.0 f32))
  (gl-clear (cast 16384 i32))
  (glfw-swap-buffers win)
  (usleep (cast 250000 i32))
  (gl-clear-color (cast 0.0 f32) (cast 0.0 f32) (cast 1.0 f32) (cast 1.0 f32))
  (gl-clear (cast 16384 i32))
  (glfw-swap-buffers win)
  (usleep (cast 250000 i32))
  (gl-clear-color (cast 0.0 f32) (cast 0.0 f32) (cast 1.0 f32) (cast 1.0 f32))
  (gl-clear (cast 16384 i32))
  (glfw-swap-buffers win)
  (usleep (cast 250000 i32))
  (gl-clear-color (cast 1.0 f32) (cast 0.0 f32) (cast 0.0 f32) (cast 1.0 f32))
  (gl-clear (cast 16384 i32))
  (glfw-swap-buffers win)
  (usleep (cast 250000 i32))
  (write_line "done.."))
