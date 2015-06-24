
(defvar false (cast 0 bool))
(defvar true (cast 1 bool))
(defun not (bool (x bool)) (eq false x))
(defvar null (cast 0 (ptr void)))

(defun write-line (void (str (ptr char)))
  (write_line str))

(defvar libm (load-lib "libm.so"))
(load-symbol libm (quote cos) (quote cos) (type (fcn f64 (x f64))))
(load-symbol libm (quote cosf) (quote cosf) (type (fcn f32 (x f32))))
(cos 3.14)
(cosf 3.14)

;; Loading a library
(defcmacro load-symbol+ (lib name cname _type)
  (expr (load-symbol (unexpr lib) 
		     (quote (unexpr name))
		     (quote (unexpr cname))
		     (type (unexpr _type)))))

(defvar libm (load-lib "libm.so"))
(load-symbol+ libm  cos cos  (fcn f64 (x f64)))
(load-symbol+ libm  cosf cosf (fcn f32 (x f32)))
(cos 3.14)
(cosf 3.14)



(defvar libc (load-lib "/lib/x86_64-linux-gnu/libc.so.6"))
(defcmacro load-libc (name type)
  (expr 
     (load-symbol+ libc (unexpr name) (unexpr name) (unexpr type))))

(load-libc printf (fcn void (fmt (ptr char)) (x i64)))
(load-libc usleep (fcn void (time i32)))
(load-libc malloc (fcn (ptr void) (bytes i64)))
(load-libc free (fcn void (ptr (ptr void))))
(load-libc realloc (fcn (ptr void) (ptr (ptr void)) (bytes u64)))
(load-libc memcpy (fcn void (dst (ptr void)) (src (ptr void)) (bytes u64)))
(load-libc exit  (fcn void (status i32)))
(load-libc strlen (fcn u32 (str (ptr char))))


(defcmacro ptr+ (ptr offset)
  (var ((size_expr (number2expr (cast (size-of (ptr-inner (type-of ptr))) i64))))
       (progn
	 (printf "size_expr: %i\n" (cast (size-of (ptr-inner (type-of ptr))) i64))
	 (expr
	  (cast 
	   (i64+ 
	    (cast (unexpr ptr) i64)
	    (i64* (unexpr offset) (unexpr size_expr)))
	   (unexpr (type2expr (type-of ptr))))))))
(ptr+ "asdasd" 4)

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

(defcmacro add-to-list+ (lst cnt item)
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

(defcmacro assert (_expr)
  (var ((n (number2expr (cast asserts-cnt i64))))
       (progn
	 (add-to-list+ asserts asserts-cnt _expr)
	 (printf "assert idx: %i\n" (cast asserts-cnt i64))
	 (expr
	  (if (not (unexpr _expr))
	      (progn
		(write-line "\n**** ERROR *****")
		(print-expr (deref (ptr+ asserts (unexpr n))))	
		(write-line "****  *****\n")	
		(exit 1)
		1
		)
	      (progn
		(write-line "OK")
		2)

		
	      )))))

