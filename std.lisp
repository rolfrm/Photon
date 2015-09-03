;; this file is a bit special because it builds up most of the standard library.
;; without this the language is very basic and so there are a lot of dependencies here.

(defvar false (cast 0 bool))
(defvar true (cast 1 bool))
(defvar newline "
")
(defun not (bool (x bool)) (eq false x))
(defvar null (cast 0 (ptr void)))
(defvar null-expr (cast null (ptr expr)))
(defvar error-type (cast null (ptr type_def)))
(defvar libc :type lib)
(if! (is-linux?)
     (setf libc (load-lib "libc.so"))
     (setf libc (load-lib "msvcrt.dll")))

;(defun +quote ((ptr expr) (sstr (ptr expr)))
;  (expr (get-symbol (stringify (unexpr sstr)))))
;(declare-macro quote +quote)

;; Loading a library
(defun +load-symbol+ ((ptr expr) (_lib (ptr expr)) (name (ptr expr)) (cname (ptr expr)) (_type (ptr expr)))
  (expr 
   (var! ((addr (load-symbol (unexpr _lib) (stringify (unexpr cname)))))
	 (progn
	   (if! (eq null addr) 
		(progn
		  (builtin-print-str "Unknown symbol! ")
		  (builtin-print-str (stringify (unexpr cname)))
		  (builtin-print-str newline))
		(noop))
	   
	   (defun! (stringify (unexpr name))
	       (type (unexpr _type)) addr )))))


(declare-macro load-symbol+ +load-symbol+)

(defun -load-libc ((ptr expr) (name (ptr expr)) (type (ptr expr)))
  (expr 
   (load-symbol+ libc (unexpr name) (unexpr name) (unexpr type))))

(declare-macro load-libc -load-libc)

(load-libc fprintf (fcn void (file (ptr void)) (fmt (ptr char)) (x i64)))


(load-symbol+ libc alloc malloc (fcn (ptr void) (bytes u64)))
(load-symbol+ libc dealloc free (fcn void (ptr (ptr void))))
(load-libc realloc (fcn (ptr void) (ptr (ptr void)) (bytes u64)))
(load-libc memcpy (fcn void (dst (ptr void)) (src (ptr void)) (bytes u64)))
(load-libc memset (fcn void (dst (ptr void)) (c u8) (bytes u64)))
(load-libc exit  (fcn void (status i32)))
(load-libc strlen (fcn i64 (str (ptr char))))
(load-libc chdir (fcn i32 (path (ptr char))))
(load-libc getcwd (fcn (ptr char) (buf (ptr char)) (buflen u64)))
(load-libc fopen (fcn (ptr void) (filename (ptr char)) (mode (ptr char))))
(load-libc fflush (fcn i32 (file (ptr void))))
(load-libc remove (fcn void (file (ptr char))))
(load-libc setbuf  (fcn void (file (ptr void)) (buffer (ptr void))))

(defun alloc0 ((ptr void) (size u64))
  (var ((buffer (alloc size)))
       (progn
	 (memset buffer 0 size)
	 buffer)))

(defun *ptr+ ((ptr expr) (ptr (ptr expr)) (offset (ptr expr)))
  (var ((ptr-type (type-of ptr)))
   (var ((size_expr (number2expr (cast (size-of (ptr-inner ptr-type)) i64)))
	 (out-expr null-expr))
	(progn
	  (if! (and (is-ptr-type? ptr-type)
		    (is-integer-type? (type-of offset)))
	       (setf out-expr 
		     (expr
		      (cast 
		       (.+ 
			(cast (unexpr ptr) i64)
			(.* (unexpr offset) 
			    (unexpr size_expr)))
		       (unexpr (type2expr ptr-type)))))
	       (noop))
	  out-expr))))

(declare-macro ptr+ *ptr+)

(defun string-concat ((ptr char) (a (ptr char)) (b (ptr char)))
  (var ((a-len (cast (strlen a) u64))
	(b-len (cast (strlen b) u64)))
       (var ((buffer (alloc0 (.+ 1 (.+ a-len b-len)))))
	    (progn
	      (memcpy buffer (cast a (ptr void)) a-len)
	      (memcpy (ptr+ buffer (cast a-len i64)) (cast b (ptr void)) b-len)
	      (cast buffer (ptr char))))))

;; (defun symbol-combine ((ptr symbol) (a (ptr symbol)) (b (ptr symbol)))
;;   (var ((aname (symbol-name a)) (bname (symbol-name b)))
;;        (var ((combined (string-concat aname bname)))
;; 	    (var ((sym (get-symbol combined)))
;; 		 (progn
;; 		   (dealloc (cast combined (ptr void)))
;; 		   sym)))))


(defun unfold-body ((ptr expr) (header (ptr expr)) (args (ptr expr)))
  (var ((sexprs (cast
		 (alloc (.* (cast (size-of (type (ptr expr))) u64)
			      (.+ 1 (sub-expr.cnt args)))) (ptr (ptr expr))))
	(i (cast 0 u64)))
       (progn
	 (setf (deref sexprs) header)
	 (while! (not (eq  i (sub-expr.cnt args)))
		 (progn
		   (setf (deref (ptr+ sexprs (.+ 1 (cast i i64)))) (sub-expr.expr args i))
		   (setf i (.+ i 1))))
	 (make-sub-expr sexprs (.+ i 1)))))

(defun *let ((ptr expr) (vars (ptr expr)) (args (ptr expr)))
  (expr
   (var (unexpr vars)
	(unexpr (unfold-body (expr progn) args)))))

(declare-macro let *let :rest)

(defun *for ((ptr expr) (_var (ptr expr)) (_varval (ptr expr)) (_expr (ptr expr)) (_incr (ptr expr)) ( _body (ptr expr)))
  (expr 
   (var (((unexpr _var) (unexpr _varval)))
	(while! (unexpr _expr)
	  (progn
	    (unexpr (unfold-body (expr progn) _body))
	    (setf (unexpr _var) (unexpr _incr)))))))
(declare-macro for *for :rest)

(defun *while ((ptr expr) (_expr (ptr expr)) (_body (ptr expr)))
  (expr
   (while! (unexpr _expr)
	   (unexpr (unfold-body (expr progn)
				_body)))))

(declare-macro while *while :rest)

(load-symbol+ libc +std:print-f64 fprintf (fcn i32 (file (ptr void)) (fmt (ptr char)) (x f64)))
(load-symbol+ libc +std:print-i64 fprintf (fcn i32 (file (ptr void)) (fmt (ptr char)) (x i64)))
(load-symbol+ libc +std:print-str fprintf (fcn i32 (file (ptr void)) (fmt (ptr char))))

(if! (is-linux?)
     (eval! (expr 
	     (progn
	       (load-symbol+ libc std:stdin stdin (ptr void))
	       (load-symbol+ libc std:stdout stdout (ptr void))
	       (noop))))
     (eval! (expr
	    (progn
	      (defvar std:stdout (fopen "std-out.txt" "a"))
	      (setbuf std:stdout null)
	      (noop)))))

;(remove "std-out.txt")
(defvar std:file std:stdout)
;(cast std:file i64)
(+std:print-f64 std:file "asdasdasd
" 2)
;(exit 0)
(defun std:print-f64 (i32 (fmt (ptr char)) (x f64))
  (+std:print-f64 std:file "%f" x))

(defun std:print-i64 (i32 (fmt (ptr char)) (x i64))
  (+std:print-i64 std:file "%lli" x))



(defun printf64 (void (x f64))
  (std:print-f64 "%f" x))

(defun printf32 (void (x f32))
  (std:print-f64 "%f" (cast x f64)))

(defun printi64 (void (x i64))
  (std:print-i64 "%lli" x))

(defun printi32 (void (x i32))
  (printi64 (cast x i64)))

(defun printi16 (void (x i16))
  (printi64 (cast x i64)))

(defun printi8 (void (x i8))
  (printi64 (cast x i64)))

(defun printu64 (void (x u64))
  (printi64 (cast x i64)))

(defun printu32 (void (x u32))
  (printu64 (cast x u64)))

(defun printu16 (void (x u16))
  (printu64 (cast x u64)))

(defun printu8 (void (x u8))
  (printu64 (cast x u64)))

(defun printchar (void (x char))
  (std:print-i64 "%c" (cast x i64)))

(defun printstr (void (x (ptr char)))
  (+std:print-str std:file x))

;; (defun print-symbol (void (x (ptr symbol)))
;;   (printstr (symbol-name x)))

(defun print-hex(void (x i64))
  (std:print-i64 "%x" x))

(defvar newline "
")

(defun print-newline(void)
  (std:print-i64 newline 0))

(defun write-line (void (str (ptr char)))
  (progn (printstr str)
	 (print-newline)))
(defun if+ ((ptr expr) (expected-type (ptr type_def)) (exprs (ptr expr)))
  (let ((cnd (sub-expr.expr exprs 0))
	(_then (sub-expr.expr exprs 1))
	(_else (sub-expr.expr exprs 2)))
    (let ((out-expr null-expr)
	  (then-type (type-of3 expected-type _then))
	  (else-type (type-of3 expected-type _else)))
      (if! (and (and (eq then-type else-type) 
		     (eq expected-type (cast null (ptr type_def))))
		(not (eq then-type (type void))))
	   (setf expected-type then-type)
	   (noop))
      
      (if! (or
	    (eq null (cast expected-type (ptr void)))
	    (eq (type void) expected-type))
	   ;;in this case regular if statement
	   (setf out-expr 
		 (expr
		  (if! (unexpr cnd) (unexpr _then) (unexpr _else))))
	   ;; else we need to create a temporary variable an
	   ;; set the return value of the body
	   ;; and the types needs to be the same.
	   (let ((tmp-sym (gensym)))
	     (if! (and (eq then-type else-type) (eq expected-type then-type))
		  (setf out-expr
			(expr (var (((unexpr tmp-sym) :type (unexpr (type2expr expected-type))))
				   (progn
				     (if! (unexpr cnd)
					  (setf (unexpr tmp-sym) (unexpr _then))
					  (setf (unexpr tmp-sym) (unexpr _else)))
				     (unexpr tmp-sym)))))
		  (progn
		    (builtin-print-str " Error expanding if macro "))
		  )))
  
      out-expr)))
				 
(declare-macro if if+)


(defun *defmacro ((ptr expr) (name (ptr expr)) (args (ptr expr)) (body (ptr expr)))
  (if (check-type-run?)
      (expr (noop))
      (let ((defun-name (expr (macro (unexpr name))))
	    (arg-cnt (sub-expr.cnt args))
	    (exprtype (expr (ptr expr)))
	    (use-rest false)
	    (use-type false))
	(printstr "Defining macro '")
	(print-expr name)
	(printstr "'.")
	(printstr newline)
	(let ((convargs (cast (alloc (.* (size-of (type (ptr expr))) (.+ 1 arg-cnt))) (ptr (ptr expr))))
	      (it 0)
	      (item 0))
	  (if (and (eq (sub-expr.cnt args) 3)
		   (eq (intern (sub-expr.expr args 0)) (intern (expr &type))))
	      (expr
	       (progn
		 (defun (unexpr defun-name) 
		     ((ptr expr) ((unexpr (sub-expr.expr args 1)) (ptr type_def)) 
		      ((unexpr (sub-expr.expr args 2)) (ptr expr)))
		   (unexpr body))
		 (declare-macro (unexpr name) (unexpr defun-name))
		 (noop)))
	      (progn
		
		(setf (deref convargs) exprtype)
		(while (not (eq it (cast arg-cnt i64)))
		  (let ((arg (sub-expr.expr args (cast it u64))))
		    (if! (and 
			  (not (is-sub-expr arg))
			  (eq (intern (sub-expr.expr args (cast it u64))) (intern (expr &rest))))
			 (progn
			   (setf use-rest true)
			   (setf item (.- item 1))
			   (noop))
			 (setf (deref (ptr+ convargs (.+ 1 item)))
			       (let ((sub-args (cast (alloc (.* (size-of (type (ptr expr))) 2)) (ptr (ptr expr)))))
				 (setf (deref sub-args) arg)
				 (setf (deref (ptr+ sub-args 1)) exprtype)
				 (make-sub-expr sub-args 2))))
		    )
		  (setf it (.+ 1 it))
		  (setf item (.+ 1 item))
		  )
		(expr
		 (progn
		   
		   (defun (unexpr defun-name) 
		       (unexpr (make-sub-expr convargs (.+ (cast (not use-rest) u64) arg-cnt ))) (unexpr body))
		   (unexpr
		    (let ((out-expr null-expr))
		      (if! use-rest
			   (setf out-expr (expr (declare-macro (unexpr name) (unexpr defun-name) :rest)))
			   (setf out-expr (expr (declare-macro (unexpr name) (unexpr defun-name)))))
		      out-expr))))
		))))))

(declare-macro defmacro *defmacro)

(defmacro test1 (&type a d)
  (sub-expr.expr d 0))
(test1 5)
(defmacro test2 (a d)
  d)
(defvar test3 :type f64)
(setf test3 (test2 1 4))

(if false "asd" 3)
  
(defvar a :type f64)
(setf a (if true 1 2))

(defun add-to-list (void (list (ptr (ptr void)))
		    (cnt (ptr u64)) (data (ptr void)) (elem-size u64))
  (progn
    (setf (deref list) 
	  (realloc (deref list) (.* elem-size (.+ (deref cnt) 1))))
    (memcpy (cast
	     (.+ (cast (deref list) u64) (.* (deref cnt) elem-size))
	     (ptr void))
	    data
	    elem-size)
    (setf (deref cnt) (.+ (deref cnt) 1))
    ))

(defmacro add-to-list+ (lst cnt item)
  (var ((size (size-of (type-of item)))
	(item-sym (gensym)))
       (if (not (is-integer-type? (type-of cnt)))
	   null-expr
	   (expr 
	    (var! (((unexpr item-sym) (unexpr item)))
		 (var! ((lstptr (addrof (unexpr lst)))
		       (cntptr (cast (addrof (unexpr cnt)) (ptr u64)))
		       (itemaddr (addrof (unexpr item-sym))))
		      (add-to-list (cast lstptr (ptr (ptr void)))
				   cntptr
				   (cast itemaddr (ptr void))
				   (unexpr (number2expr (cast size i64))))))))))

(defmacro when (test &rest body)
  (expr
   (if! (unexpr test)
	(unexpr (unfold-body (expr progn)
			     body))
	(noop))))

(defmacro unless (test &rest body)
  (expr 
   (if! (unexpr test)
	(noop)
	(progn
	  (unexpr (unfold-body (expr progn)
			       body))
	  ))))

(defvar asserts (cast null (ptr (ptr expr))))
(defvar asserts-cnt (cast 0 u64))
(defvar last-assert :type (ptr expr))

(defmacro assert (_expr)
  (var ((n (number2expr (cast asserts-cnt i64))))
       (progn
	 (add-to-list+ asserts asserts-cnt _expr)
	 (expr
	  (unless (unexpr _expr)
	    (printstr "\n**** ERROR *****\n")
	    (print-expr (deref (ptr+ asserts (unexpr n))))	
	    (printstr "\n")
	    (write-line "****  *****\n")	
	    (exit 1)
	    )))))

(defun +vararg2 ((ptr expr) (a (ptr expr)) (b (ptr expr)) (c (ptr expr)))
  (progn
    (sub-expr.expr c 1)))

(declare-macro vararg2 +vararg2 :rest)

(vararg2 1 2 3 4 5)

(defmacro vararg-test (a b &rest c)
  (progn
    (sub-expr.expr c 1)))

(assert (eq 4 (vararg-test 1 2 3 4 5)))
(for it 0 (not (eq it 10)) (.+ it 1)
     (setf it (.+ it 1))
     (printi64 it)
     print-newline)

(unless false
  (write-line "hej!"))

(defmacro max (a b)
  (let ((as (gensym)) (bs (gensym)))
    (expr (var (((unexpr as) (unexpr a)) ((unexpr bs) (unexpr b)))
	    (if (< (unexpr as) (unexpr bs))
		(unexpr bs)
		(unexpr as))))))


(defmacro min (a b)
  (let ((as (gensym)) (bs (gensym)))
    (expr (var (((unexpr as) (unexpr a)) ((unexpr bs) (unexpr b)))
	    (if (> (unexpr as) (unexpr bs))
		(unexpr bs)
		(unexpr as))))))

(defmacro lambda (args body)
  (let ((s (gensym)))
    (expr 
     (progn
       (defun (unexpr s) (unexpr args)
	 (unexpr body))
       (addrof (unexpr s))))))

(defun sign (i64 (x i64))
  (if (< x 0)
      -1
      1))

; range: from start to stop, excluding stop.
(defmacro range (it start stop &rest body)
  (let ((s (gensym))
	(d (gensym)))
    (expr
     (var! (((unexpr it) (unexpr start))
	   ((unexpr s) (unexpr stop)))
       (var!(((unexpr d) (sign (.- (unexpr s) (unexpr start)))))
	 (while! (not (eq (unexpr it) (unexpr s)))
		 (progn
		   (unexpr (unfold-body (expr progn) body))
		   (setf (unexpr it) (.+ (unexpr it) (unexpr d))))))))))
; Usage:
;;; (range a 0 -10 (printi64 a) (printstr newline))
;;; (range b 0 10 (printi64 b) (printstr newline))

(defun unfold-body2 ((ptr expr) (header (ptr expr)) (args (ptr expr)))
    (let ((sexprs (cast
		   (alloc (.* (cast (size-of (type (ptr expr))) u64)
				(.+ (sub-expr.cnt header) 
				      (sub-expr.cnt args)))) 
		   (ptr (ptr expr)))))
      (range i 0 (cast (sub-expr.cnt header) i64)
	     (setf (deref (ptr+ sexprs i)) (sub-expr.expr header (cast i u64))))
      (let ((offset (cast (sub-expr.cnt header) i64)))
	(range i 0 (cast (sub-expr.cnt args) i64)
	       (setf (deref (ptr+ sexprs (.+ offset i))) (sub-expr.expr args (cast i u64)))))
      (make-sub-expr sexprs (.+ (sub-expr.cnt header) (sub-expr.cnt args)))))
;; compile-time-if
(defmacro if!!(cond _then _else)
  (progn
    (eval! (expr (defvar if!!-result (unexpr cond))))
    (var ((cond-r (deref (cast (get-var (intern (expr if!!-result))) (ptr bool)))))
      (if cond-r
	  _then
	  _else))))


(if!! (is-linux?)
      (load-libc usleep (fcn void (us i64)))
      (progn
	(eval! (expr (load-libc Sleep (fcn void (ms i32)))))
	(eval! (expr (defun usleep (void (microseconds i64))
		       (Sleep (cast (./ microseconds 1000) i32)))))))
(usleep 100)
