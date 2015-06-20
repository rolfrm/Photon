;; The following code runs / compiles
(load "std.lisp")
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

;; Defining / using variables
(defvar test1 1)
(defvar test2 (i64+ test1 1))
(defvar test3 (i64+ test1 test2))
(defvar test4 (i64+ test3 test2))
(defvar test5 (i64+ test4 test3))
(defvar test6 (i64+ test5 test4))
(defvar test7 (i64+ test6 test5))
(defvar test8 (i64+ test7 test6))

(i64+ test8 test8)
(setf test8 100) ; Setting a variable
(i64+ test8 test7)

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

(defvar numbers:one 1)
(defvar numbers:two 2)
(defvar numbers:three 3)
;(type (enum numbers (one 1) (two 2) (three 3)))
;; alt:
;(type (enum number-names (one "one") (two "two")))
;(enum_value numbers one) ;; 1
;numbers:three ;; 3

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

(expand fun-expr "hello" (write_line "WORLD!"))
(write_line "gets here")
(if (eq 2 1) 2 3)

(defvar a 0)

(write_line "gets here")
(while (not (eq a 10))
  (progn 
    (setf a (i64+ a 1))
    (if (eq a 5)
	(progn (write_line "aaa") 1)
	(progn (write_line "bbb") 2))
    a))

(deref "asd")

;; Loading a library
(defvar libm (load-lib "libm.so"))
(load-symbol libm (quote cos) (quote cos) (type (fcn f64 (x f64))))
(load-symbol libm (quote cosf) (quote cosf) (type (fcn f32 (x f32))))
(cos 3.14)
(cosf 3.14)


(defcmacro load-symbol+ (lib name cname _type)
  (expr (load-symbol (unexpr lib) 
		     (quote (unexpr name))
		     (quote (unexpr cname))
		     (type (unexpr _type)))))

(defvar libc (load-lib "/lib/x86_64-linux-gnu/libc.so.6"))
;(load-symbol libc (quote printf) (quote printf) (type ))
;(load-symbol libc (quote usleep) (quote usleep) (type (fcn void (time i32))))
;(expand load-symbol+ libc usleep usleep (fcn void (time i32)))
(defcmacro load-libc (name type)
  (expr 
   (progn
     
     (expand load-symbol+ libc (unexpr name) (unexpr name) (unexpr type)))))

(expand load-libc printf (fcn void (fmt (ptr char)) (x i64)))
(expand load-libc usleep (fcn void (time i32)))
(expand load-libc malloc (fcn (ptr void) (bytes i64)))
(expand load-libc free (fcn void (ptr (ptr void))))
(expand load-libc realloc (fcn (ptr void) (ptr (ptr void)) (bytes u64)))
(expand load-libc memcpy (fcn void (dst (ptr void)) (src (ptr void)) (bytes u64)))
(expand load-libc exit  (fcn void (status i32)))
(defvar teststr "asdaasd")
(defvar testarray (malloc 10))
(memcpy testarray (cast teststr (ptr void)) 8)
(write_line ")))))(((((")
(write_line (cast testarray (ptr char)))
(write_line ")))))(((((")

(defun add-to-list (void (list (ptr (ptr void)))
		    (cnt (ptr u64)) (data (ptr void)) (elem-size u64))
  (progn
     (setf (deref list) 
	   (realloc (deref list) (u64* elem-size (u64+ (deref cnt) 1))))
     (memcpy (cast
	      (u64+ (cast (deref list) u64) (u64* (deref cnt) elem-size))
	      (ptr void))
	     data
	     elem-size)
     (setf (deref cnt) (u64+ (deref cnt) 1))
     (write_line "lol")
     ))
(defvar add-test (cast null (ptr i64)))
(defvar add-test-cnt (cast 0 u64))
(defvar to-add (cast 15 i64))
(add-to-list (cast (addrof add-test) (ptr (ptr void)))
	     (addrof add-test-cnt)
	     (cast (addrof to-add) (ptr void))
	     (size-of (type i64)))
(setf to-add 20)
(add-to-list (cast (addrof add-test) (ptr (ptr void)))
	     (addrof add-test-cnt)
	     (cast (addrof to-add) (ptr void))
	     (size-of (type i64)))
(setf to-add 30)
(add-to-list (cast (addrof add-test) (ptr (ptr void)))
	     (addrof add-test-cnt)
	     (cast (addrof to-add) (ptr void))
	     (size-of (type i64)))
(write_line "new deref:")
(deref add-test)
(deref (cast (u64+ (cast add-test u64)
	     8) (ptr i64)))
(deref (cast (u64+ (cast add-test u64)
	     16) (ptr i64)))
add-test-cnt
(exit 0)

(write_line "asd")
(printf "test: %i\n" 5)
(write_line "the following works if libglfw is installed")

(defcmacro comment (_expr)
  (expr (write_line "lol..")))

(defvar libglfw (load-lib "libglfw.so"))
(load-symbol libglfw (quote glfw:init) (quote glfwInit) (type (fcn void)))
(load-symbol libglfw (quote glfw:create-window) (quote glfwCreateWindow) 
	     (type (fcn (ptr void) (width i32) (height i32) 
			(title (ptr char)) (a (ptr void)) (b (ptr void)))))
(load-symbol libglfw (quote glfw:swap-buffers) (quote glfwSwapBuffers) 
	     (type (fcn void (a ( ptr void)))))
(load-symbol libglfw (quote glfw:make-current) (quote glfwMakeContextCurrent) (type (fcn void (win (ptr void)))))

(defvar libgl (load-lib "libGL.so"))
(load-symbol libgl (quote gl:clear) (quote glClear) (type (fcn void (mask i32))))
(load-symbol libgl (quote gl:clear-color) (quote glClearColor) 
	     (type (fcn void (r f32) (g f32) (b f32) (a f32))))
(write_line "create-shader")
(defvar gl:fragment-shader 0x8b30)
(defvar gl:vertex-shader 0x8b31)
(load-symbol libgl (quote gl:create-shader) (quote glCreateShader)
	     (type (fcn u32 (type u32))))
(load-symbol libgl (quote gl:shader-source) (quote glShaderSource)
	     (type (fcn void 
			(shader u32) 
			(count u32) 
			(shader-string (ptr (ptr char)))
			(length (ptr u32)))))

(glfw:init)

(defvar win (glfw:create-window 512 512 "test.." null null))

(glfw:make-current win)
(defvar sleeptime (cast 10000 i32))
(defvar gl:color-buffer-bit (cast 0x4000 i32))
(defvar r 0.0)
(progn
  (gl:clear-color 1.0  1.0 1.0  1.0 )
  (gl:clear gl:color-buffer-bit)
  (glfw:swap-buffers win)
  (usleep sleeptime)
  (gl:clear-color 1.0  0.0 1.0  1.0 )
  (gl:clear gl:color-buffer-bit)
  (glfw:swap-buffers win)
  (usleep sleeptime)
  (gl:clear-color 0.0  0.0 1.0  1.0 )
  (gl:clear gl:color-buffer-bit)
  (glfw:swap-buffers win)
  (usleep sleeptime)
  (gl:clear-color 0.0  0.0 0.0  1.0 )
  (gl:clear gl:color-buffer-bit)
  (glfw:swap-buffers win)
  (usleep sleeptime)
  (write_line "done..")
  (i64+ 5 100))

(defvar m (cast (malloc 100) (ptr char)))
(setf (deref m) (deref "a"))
(setf (deref m) (deref "œ"))

;(load "overload.lisp")

"SUCCESS!"

;; testing for memory corruption
(defvar a 0)
(while (not (eq a 1000))
  (progn
    (setf a (i64+ a 1))
    (free (realloc (malloc 1000) 2000))
    10
    ))
