;; The following code runs / compiles
(load "std.lisp")
(load "overload.lisp")
(assert (eq false (eq 0 1)))

;; Constants
1 
"hello world!"

;; Function calls
(i64+ 10 10)

;(defvar i65+ (cast (addrof i64+) (fcn i8 (a i8) (b i8))))

;; Defining functions
(defun add (f64 (a f64) (b f64)) (f+ a b))
(add 45 67)

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
(print-type (type (struct _vec2 (x f64) (y f64)))) ; this actually defines a struct named _vec2.
(type (alias _vec2 vec2)) ; defines vec2 as a _vec2 struct.

;(type (enum numbers (one 1) (two 2) (three 3)))
;; alt:
;(type (enum number-names (one "one") (two "two")))
;(enum_value numbers one) ;; 1
;numbers:three ;; 3
(progn
  (defvar xy :type vec2)
  (noop))

(defun makevec2 (vec2 (a f64) (b f64))
  (let ((out xy))
    (setf (member out x) a)
    (setf (member out y) b)
    out))

(print "...\n")


(defcmacro vec2op (operator)
  (let ((name (symbol2expr (symbol-combine (quote vec2) (expr2symbol operator)))))
    (expr
     (progn
       (defun (unexpr name) (vec2 (a vec2) (b vec2))
	 (let ((out xy))
	   (setf (member out x) 
		 ((unexpr operator) (member a x) (member b x)))
	   (setf (member out y)
		 ((unexpr operator) (member a y) (member b y)))
	   out))
       (overload (unexpr operator) (unexpr name))))))
(vec2op *)(vec2op +) (vec2op /) (vec2op -)

(defun vec2scale (vec2 (a vec2) (b f64))
  (progn
    (setf (member a x) (* (member a x) b))
    (setf (member a y) (* (member a y) b))
    a))

(defun printvec2 (void (a vec2))
  (progn
    (print "(") 
    (print (member a x))
    (print " , ")
    (print (member a y))
    (print ")")))

(overload * vec2scale)

(defcmacro print-default (body)
  (expr
   (progn
     (unexpr body)
     (printstr "::\n")
     (noop))))
    
(overload print printvec2)
(overload-default print print-default)

(print "okk..")

(defcmacro printnl (body)
  (expr
   (progn
     (print (unexpr body))
     (print "\n"))))

(defcmacro no-print (body)
  (expr
   (progn 
     (unexpr body)
     (noop))))
			 
(set-printer (quote printnl))
