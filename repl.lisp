(load "std2.lisp")

(defvar t mat4-eye)
(setf (member t m03) 10.0)
t
(defvar b (vec 1 2 3 1))
(dot t b)

(defmacro print-ptr (expr2)
  (if (is-ptr-type? (type-of expr2))
      (expr 
       (progn
	 (printstr "ptr: ")
	 (print-hex (cast (unexpr expr2) i64))
	 (printstr "\n"))
       )      
      (cast null (ptr expr))))

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
    (launch (addrof test-ptr))
    (launch (addrof test-ptr))
    (launch (addrof test-ptr))
    (launch (addrof test-ptr))
    (launch (addrof test-ptr))
))
(test-launch)
(set-printer (quote printnl))


(defun *test-macro ((ptr expr) (take (ptr expr)) (exprs (ptr expr)))
    (sub-expr.expr exprs (cast (expr2number take) u64)))

(declare-macro test-macro *test-macro :rest)

(test-macro 1 2 3 4)

(defmacro asd (a b c)
  a)

(asd 5 4 5)

(defmacro test2 (a b c &rest d)
  (sub-expr.expr d (cast (expr2number a) u64)))

(test2 1 2 3 4 5 6 7)

(defvar buf (cast (alloc 100) (ptr char)))
(getcwd buf 100)
(chdir "..")
(getcwd buf 100)
(dealloc (cast buf (ptr void)))
(setf buf (cast null (ptr char)))

(defun plus1 (i64 (a i64)) (i64+ 1 a))

(defvar plus12 (addrof plus1))

((addrof plus1) 3)


(defun cctest(void (userdata (ptr void)))
  (while true
    (print "hello cc: " userdata "\n")
    (ccyield)))


(defvar cc (ccstart))
(ccthread cc cctest (cast 0 (ptr void)))
(ccthread cc cctest (cast 3 (ptr void)))
(ccthread cc cctest (cast 4 (ptr void)))

(for it 0 (< it 100) (i64+ it 1)
     (ccstep cc))
