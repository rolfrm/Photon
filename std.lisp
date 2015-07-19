;; this file is a bit special because it builds up most of the standard library.
;; without this the language is very basic and so, there are a lot of dependencies
;; here.

(defvar false (cast 0 bool))
(defvar true (cast 1 bool))
(defun not (bool (x bool)) (eq false x))
(defvar null (cast 0 (ptr void)))

(defvar libc (load-lib "/lib/x86_64-linux-gnu/libc.so.6"))

;; Loading a library
(defun +load-symbol+ ((ptr expr) (_lib (ptr expr)) (name (ptr expr)) (cname (ptr expr)) (_type (ptr expr)))
  (expr (load-symbol (unexpr _lib) 
		     (quote (unexpr name))
		     (quote (unexpr cname))
		     (type (unexpr _type)))))

(declare-macro load-symbol+ +load-symbol+)

(defun -load-libc ((ptr expr) (name (ptr expr)) (type (ptr expr)))
  (expr 
   (load-symbol+ libc (unexpr name) (unexpr name) (unexpr type))))

(declare-macro load-libc -load-libc)

(load-libc printf (fcn void (fmt (ptr char)) (x i64)))
(load-libc usleep (fcn void (time i32)))
(load-symbol+ libc alloc malloc (fcn (ptr void) (bytes u64)))
(load-symbol+ libc dealloc free (fcn void (ptr (ptr void))))
(load-libc realloc (fcn (ptr void) (ptr (ptr void)) (bytes u64)))
(load-libc memcpy (fcn void (dst (ptr void)) (src (ptr void)) (bytes u64)))
(load-libc memset (fcn void (dst (ptr void)) (c u8) (bytes u64)))
(load-libc exit  (fcn void (status i32)))
(load-libc strlen (fcn i64 (str (ptr char))))
(load-libc chdir (fcn i32 (path (ptr char))))
(load-libc getcwd (fcn (ptr char) (buf (ptr char)) (buflen u64)))
(load-symbol+ libc std:rand rand (fcn i32))
(defun rand(i64)
  (bit-or (cast (std:rand) i64) 
	  (<< (cast (std:rand) i64) 32)))
(defun alloc0 ((ptr void) (size u64))
  (var ((buffer (alloc size)))
       (progn
	 (memset buffer 0 size)
	 buffer)))

(defun *ptr+ ((ptr expr) (ptr (ptr expr)) (offset (ptr expr)))
  (var ((size_expr (number2expr (cast (size-of (ptr-inner (type-of ptr))) i64))))
       (progn
	 (expr
	  (cast 
	   (i64+ 
	    (cast (unexpr ptr) i64)
	    (i64* (unexpr offset) (unexpr size_expr)))
	   (unexpr (type2expr (type-of ptr))))))))
(declare-macro ptr+ *ptr+)

(defun string-concat ((ptr char) (a (ptr char)) (b (ptr char)))
  (var ((a-len (cast (strlen a) u64))
	(b-len (cast (strlen b) u64)))
       (var ((buffer (alloc0 (u64+ 1 (u64+ a-len b-len)))))
	    (progn
	      (memcpy buffer (cast a (ptr void)) a-len)
	      (memcpy (ptr+ buffer (cast a-len i64)) (cast b (ptr void)) b-len)
	      (cast buffer (ptr char))))))

(defun symbol-combine ((ptr symbol) (a (ptr symbol)) (b (ptr symbol)))
  (var ((aname (symbol-name a)) (bname (symbol-name b)))
       (var ((combined (string-concat aname bname)))
	    (var ((sym (get-symbol combined)))
		 (progn
		   (dealloc (cast combined (ptr void)))
		   sym)))))


(defun unfold-body ((ptr expr) (header (ptr expr)) (args (ptr expr)))
  (var ((sexprs (cast
		 (alloc (u64* (cast (size-of (type (ptr expr))) u64)
			      (u64+ 1 (sub-expr.cnt args)))) (ptr (ptr expr))))
	(i (cast 0 u64)))
       (progn
	 (setf (deref sexprs) header)
	 (loop-while (not (eq  i (sub-expr.cnt args)))
	   (progn
	     (setf (deref (ptr+ sexprs (i64+ 1 (cast i i64)))) (sub-expr.expr args i))
	     (setf i (u64+ i 1))))
	 (make-sub-expr sexprs (u64+ i 1)))))

(defun *let ((ptr expr) (vars (ptr expr)) (args (ptr expr)))
  (expr
   (var (unexpr vars)
	(unexpr (unfold-body (expr progn) args)))))

(declare-macro let *let :rest)

(defun *for ((ptr expr) (_var (ptr expr)) (_varval (ptr expr)) (_expr (ptr expr)) (_incr (ptr expr)) ( _body (ptr expr)))
  (expr 
   (var (((unexpr _var) (unexpr _varval)))
	(loop-while (unexpr _expr)
	  (progn
	    (unexpr (unfold-body (expr progn) _body))
	    (setf (unexpr _var) (unexpr _incr)))))))
(declare-macro for *for :rest)

(defun *while ((ptr expr) (_expr (ptr expr)) (_body (ptr expr)))
  (expr
   (loop-while (unexpr _expr)
      (unexpr (unfold-body (expr progn)
			   _body)))))

(declare-macro while *while :rest)

(defun *defmacro ((ptr expr) (name (ptr expr)) (args (ptr expr)) (body (ptr expr)))
  (let ((defun-name (symbol2expr (symbol-combine (quote **) (expr2symbol name))))
	(arg-cnt (sub-expr.cnt args))
	(exprtype (expr (ptr expr)))
	(use-rest false))
    
    (let ((convargs (cast (alloc (u64* (size-of (type (ptr expr))) (u64+ 1 arg-cnt))) (ptr (ptr expr))))
	  (it 0)
	  (item 0))
      (setf (deref convargs) exprtype)
      (while (not (eq it (cast arg-cnt i64)))
	(let ((arg (sub-expr.expr args (cast it u64))))
	  (if (and 
	       (not (is-sub-expr arg))
	       (eq (expr2symbol (sub-expr.expr args (cast it u64))) (quote &rest)))
	      (progn
		(setf use-rest true)
	        (setf item (i64- item 1))
		(noop))
	      (setf (deref (ptr+ convargs (i64+ 1 item)))
		    (let ((sub-args (cast (alloc (u64* (size-of (type (ptr expr))) 2)) (ptr (ptr expr)))))
		      (setf (deref sub-args) arg)
		      (setf (deref (ptr+ sub-args 1)) exprtype)
		      (make-sub-expr sub-args 2))))
	  )
	(setf it (i64+ 1 it))
	(setf item (i64+ 1 item))
	)
      (let ((r
	     (expr
	      (progn
		(defun (unexpr defun-name) 
		    (unexpr (make-sub-expr convargs (u64+ (cast (if use-rest 0 1) u64) arg-cnt ))) (unexpr body))
		(unexpr
		 (if use-rest
		     (expr (declare-macro (unexpr name) (unexpr defun-name) :rest))
		     (expr (declare-macro (unexpr name) (unexpr defun-name)))))))))
	r))))

(declare-macro defmacro *defmacro)

(defvar libm (load-lib "libm.so"))
(load-symbol libm (quote cos) (quote cos) (type (fcn f64 (x f64))))
(load-symbol libm (quote cosf) (quote cosf) (type (fcn f32 (x f32))))

(type (alias i32 pthread-attr-t))

(load-symbol+ libc std:print-f64 printf (fcn (ptr void) (fmt (ptr char)) (x f64)))
(load-symbol+ libc std:print-i64 printf (fcn (ptr void) (fmt (ptr char)) (x i64)))

(defun printf64 (void (x f64))
  (std:print-f64 "%f" x))

(defun printf32 (void (x f32))
  (std:print-f64 "%f" (cast x f64)))

(defun printi64 (void (x i64))
  (std:print-i64 "%p" x))

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

(defun printstr (void (x (ptr char)))
  (std:print-f64 x 0.0))

(defun print-symbol (void (x (ptr symbol)))
  (printstr (symbol-name x)))

(defun print-hex(void (x i64))
  (std:print-i64 "%x" x))

(defun write-line (void (str (ptr char)))
  (progn (printstr str)
	 (printstr "\n")))

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
    ))

(defmacro add-to-list+ (lst cnt item)
  (var ((size (size-of (type-of item))))
       (var ((out-expr 
	      (expr 
	       (var ((lstptr (addrof (unexpr lst)))
		     (cntptr (addrof (unexpr cnt)))
		     (itemaddr (addrof (unexpr item))))
		    (add-to-list (cast lstptr (ptr (ptr void)))
				 cntptr
				 (cast itemaddr (ptr void))
				 (unexpr (number2expr (cast size i64))))))))
	    
	    out-expr)))

(defvar asserts (cast null (ptr (ptr expr))))
(defvar asserts-cnt (cast 0 u64))
(defvar last-assert :type (ptr expr))

(defmacro assert (_expr)
  (var ((n (number2expr (cast asserts-cnt i64))))
       (progn
	 (add-to-list+ asserts asserts-cnt _expr)
	 (expr
	  (if (not (unexpr _expr))
	      (progn
		(printstr "\n**** ERROR *****\n")
		(print-expr (deref (ptr+ asserts (unexpr n))))	
		(printstr "\n")
		(write-line "****  *****\n")	
		(exit 1)
		(noop)
		)
	      (noop)		
	      )))))



(string-concat "hello" "world")

(symbol-combine (quote a) (quote b))

(defmacro vararg-test (a b &rest c)
  (sub-expr.expr c 1))

(vararg-test 1 2 3 4 5)

(assert (eq 4 (vararg-test 1 2 3 4 5)))


(let ((a 10) (b 20))
  (i64+ a b)
  (i64+ a b)
  (i64+ a b)
  (i64+ (i64+ a b) 30))

(for it 0 (not (eq it 10)) (i64+ it 1)
     (setf it (i64+ it 1))
     (printi64 it)
     (printstr "\n"))

(defmacro when (test &rest body)
  (expr
   (if (unexpr test)
       (progn
	 (unexpr (unfold-body (expr progn)
			      body))
	 (noop))
       (noop))))

(defmacro unless (test &rest body)
  (expr 
   (if (unexpr test)
       (noop)
       (progn
	 (unexpr (unfold-body (expr progn)
			      body))
	 ))))

(unless false
  (write-line "hej!"))

(defmacro max (a b)
  (let ((as (gensym)) (bs (gensym)))
    (expr (let (((unexpr as) (unexpr a)) ((unexpr bs) (unexpr b)))
	    (if (< (unexpr as) (unexpr bs))
		(unexpr bs)
		(unexpr as))))))


(defmacro min (a b)
  (let ((as (gensym)) (bs (gensym)))
    (expr (let (((unexpr as) (unexpr a)) ((unexpr bs) (unexpr b)))
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

