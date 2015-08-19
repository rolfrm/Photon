;; Builds on top of std.lisp and overload.lisp
(load "std.lisp")
(load "overload.lisp")

;; uses generic +. Hence is compatible with anything that uses it
(defmacro incr (var amount)
  (expr
   (setf (unexpr var) (+ (unexpr var) (unexpr amount)))))

(defmacro range (var from to &rest body)
  (expr
   (var! (((unexpr var) (unexpr from)))
     (while! (not (eq (unexpr var) (unexpr to)))
	     (progn
	       (unexpr (unfold-body (expr progn)
				    body))
	       (incr (unexpr var) 1)
	       )))))

(defvar libm (load-lib "libm.so"))
(load-symbol+ libm cos cos (fcn f64 (x f64)))
(load-symbol+ libm cos32 cosf (fcn f32 (x f32)))
(load-symbol+ libm sin sin (fcn f64 (x f64)))
(load-symbol+ libm sin32 sinf (fcn f32 (x f32)))
(load-symbol+ libm sqrt sqrt (fcn f64 (x f64)))
(load-symbol+ libm sqrt32 sqrtf (fcn f32 (x f32)))
(load-symbol+ libm fabs fabs (fcn f64 (x f64)))
(load-symbol+ libm ceilf ceilf (fcn f32 (x f32)))
(load-symbol+ libm ceil ceil (fcn f64 (x f64)))
(load-symbol+ libm floorf floorf (fcn f32 (x f32)))
(load-symbol+ libm floor floor (fcn f64 (x f64)))

(defvar pi 3.141592653)
(defvar 2pi (* pi 2))
(load "vec2.lisp")

(type (alias (ptr i32) thread-handle))
(type (alias i32 pthread-attr-t))
(defun new-thread-handle (thread-handle) 
  (let ((nthread (cast (alloc (size-of (type i32))) (ptr i32))))
    (setf (deref nthread) 0)
    (cast nthread thread-handle)))

(defvar pthread-lib (load-lib "/lib/x86_64-linux-gnu/libpthread.so.0"))
(load-symbol pthread-lib (quote pthread-attr-init) (quote pthread_attr_init) (type (fcn i32 (attr (ptr i32)))))
(load-symbol pthread-lib (quote pthread-create) (quote pthread_create) 
	     (type (fcn i32 
			(id thread-handle) 
			(attr i32) 
			(go-fcn (fcn (ptr void) (arg (ptr void))))
			(arg (ptr void)))))

(defun thread-launcher ((ptr void) (arg (ptr void)))
  (progn
    (print (cast arg i64))
    (print-newline)
    (let ((f (cast arg (ptr (fcn void))))) 
      (f)
      )
    null))

(defun launch (void (function (ptr (fcn void))))
  (let ((attr (cast (alloc0 64) (ptr i32)))
	(threadid (new-thread-handle)))
    (pthread-attr-init attr)
    (pthread-create threadid (deref attr) thread-launcher (cast function (ptr void)))
    threadid
    (noop)))


(defvar last-type :type (ptr type_def))
(defmacro show-type (exp)
  (progn
    (setf last-type (type-of exp))
    (expr last-type)))

(defmacro comment (_expr)
  (expr (noop)))

(defmacro print-rest(&rest exprs)
  (if (> (sub-expr.cnt exprs) 1)
      (let ((print-expr (expr print))
	    (progn-expr (expr progn)))

	(let ((sub-exprs (cast (alloc0 (* (+ 1 (sub-expr.cnt exprs)) (size-of (type (ptr expr)))))
			       (ptr (ptr expr)))))
	  (setf (deref sub-exprs) progn-expr)
	  (for it (cast 0 u64) (< it (sub-expr.cnt exprs)) (+ it 1)
	       (let ((sub-expr (cast (alloc0 (* 2 (size-of (type (ptr expr)))))
				     (ptr (ptr expr)))))
		 (setf (deref sub-expr) print-expr)
		 (setf (deref (+ sub-expr 1)) 
		       (sub-expr.expr exprs it))
		 (setf (deref (+ sub-exprs (+ 1 (cast it i64)))) 
		       (make-sub-expr sub-expr 2))
		 ))
	  (make-sub-expr sub-exprs (+ 1 (sub-expr.cnt exprs)))))
      (cast null (ptr expr))))

;; (printstr "??
;; ")
(defvar ms (cast (get-var (quote print-rest)) (ptr macro_store)))
 (print-macro-store ms)
;; (printstr "
;; ??
;; ")
(overload print print-rest)
(print 1 " " 2 " " 3 newline)
(print "hello" " " "world!" newline)

(defun randf(f64)
  (let ((rand-var (std:rand)))
    (/ (- (cast (% (cast rand-var i64) 20000) f64) 10000.0) 10000.0)))

(defun rand(i64)
  (bit-or (cast (std:rand) i64) 
	  (<< (cast (std:rand) i64) 32)))

(defmacro defstruct (name &rest fields)
  (let ((inner-name (symbol2expr (symbol-combine (quote ___) (expr2symbol name)))))
    (let ((structdef
    	   (unfold-body2 
    	    (expr (struct (unexpr inner-name)))
    	    fields)))
      (expr 
       (type 
	(alias 
	 (unexpr structdef)
	 (unexpr name)))))))


(defun clear-list (void (list (ptr (ptr void)))
		   (cnt (ptr i64)))
  (progn
    (dealloc (deref list))
    (setf (deref list) null)
    (setf (deref cnt) 0)
    ))


(defmacro clear-list+ (lst cnt)
  (let ((lst-type (type-of lst)))
    (assert (is-ptr-type? lst-type))
    (expr 
     (clear-list (cast (addrof (unexpr lst)) (ptr (ptr void)))
		 (addrof (unexpr cnt))))))
(type (alias i32 seek-mode))
(defvar seek-set (cast 0 seek-mode)) ; Seek from beginning of file.
(defvar seek-cur (cast 1 seek-mode)) ; Seek from current position.
(defvar seek-end (cast 2 seek-mode)) ; Seek from end of file.

(load-libc fopen (fcn (ptr void) (filename (ptr char)) (mode (ptr char))))
(load-libc fclose (fcn i32 (file (ptr void))))
(load-libc fseek (fcn i32 (file (ptr void)) (offset u64) (mode seek-mode)))
(load-libc ftell (fcn u64 (file (ptr void))))
(load-libc fread (fcn u64 (buffer (ptr void))  (size u64) (count u64) (file (ptr void))))
(load-libc fwrite (fcn u64 (data (ptr void)) (size u64) (count u64) (file (ptr void))))
(load-libc remove (fcn void (file (ptr char))))

(remove "test.txt")
(let ((f (fopen "test.txt" "a")))
  (fwrite (cast "hello?" (ptr void)) 1 6 f)
  (fclose f))

(defun print-seek (void (mode seek-mode))
  (if (eq mode seek-set)
      (printstr "seek beginning")
      (if (eq mode seek-cur)
	  (printstr "seek current")
	  (printstr "seek end"))))
(overload print print-seek)


(load-libc open_memstream (fcn (ptr void) (data (ptr (ptr void))) (cnt (ptr u64))))

(defun file:read-all-data ((ptr char) (file (ptr void)) (size (ptr u64)))
  (let ((out-buffer :type (ptr void)))
    (let ((pos (ftell file)))
      (fseek file 0 seek-end)
      (setf (deref size) (- (ftell file) pos))
      (fseek file pos seek-set)
      (setf out-buffer (alloc0 (+ (deref size) 1) ))) ;+1 for EOF. Alternative?
    (fread out-buffer (deref size) 1 file)
    (cast out-buffer (ptr char))))

(defun read-all-data ((ptr char) (path (ptr char)) (size (ptr u64)))
  (let ((file (fopen path "r"))
	(buffer (cast null (ptr char))))
    (when (not (eq file null))
      (setf buffer (file:read-all-data file size))
      (fclose file))
    buffer))

(let ((s (cast 0 u64)))
  (let ((alldata (read-all-data "/usr/include/stdio.h" (addrof s))))
    ;(print "Read: " s newline)
    ;(print s ":" newline alldata newline)
    (dealloc (cast alldata (ptr void)))))

(defmacro ptr-null? (_ptr)
  (expr (eq null (cast (unexpr _ptr) (ptr void)))))

(defmacro zeroize (item)
  (let ((item-type (type-of item)))
    (let ((size (size-of item-type)))
      (expr (memset 
	     (cast (addrof (unexpr item)) (ptr void)) 
	     0
	     (unexpr (number2expr (cast size i64))) )))))

