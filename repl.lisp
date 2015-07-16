(load "std2.lisp")

(defvar t mat4-eye)
(setf (member t m03) 10.0)
t
(defvar b (vec 1 2 3 1))
(dot t b)

(defun _print-ptr ((ptr expr) (expr2 (ptr expr)))
  (if (is-ptr-type? (type-of expr2))
      (expr 
       (progn
	 (printstr "ptr: ")
	 (print-hex (cast (unexpr expr2) i64))
	 (printstr "\n"))
       )      
      (cast null (ptr expr))))
(declare-macro print-ptr _print-ptr)

(overload print print-ptr)
(print (cast 131324210 (ptr i64)))

(defvar a :type i32)
(defvar tid :type i32)
(setf a 0)

(defun test-ptr (void)
  (progn
    (print "hello!\n")
    (usleep 1000000)
    (print "thread!\n")
    null))

(defun test-launch (void)
  (progn
    (launch test-ptr)
    (launch test-ptr)
    (launch test-ptr)
    (launch test-ptr)
    (launch test-ptr)))

(set-printer (quote printnl))


(defun *test-macro ((ptr expr) (take (ptr expr)) (exprs (ptr expr)))
    (sub-expr.expr exprs (cast (expr2number take) u64)))

(declare-macro test-macro *test-macro :rest)

(test-macro 1 2 3 4)

(defmacro asd (a b c)
  a)

(asd 5 4 5)
