;; Builds on top of std.lisp and overload.lisp
(load "std.lisp")
(load "overload.lisp")

(defmacro incr (var amount)
  (expr
   (setf (unexpr var) (+ (unexpr var) (unexpr amount)))))

(defmacro range (var from to &rest body)
  (expr
   (let (((unexpr var) (unexpr from)))
     (while (not (eq (unexpr var) (unexpr to)))
       (unexpr (unfold-body (expr progn)
			    body))
       (incr (unexpr var) 1)
       ))))

(defvar libm (load-lib "libm.so"))
(load-symbol+ libm cos cos (fcn f64 (x f64)))
(load-symbol+ libm cos32 cosf (fcn f32 (x f32)))
(load-symbol+ libm sin sin (fcn f64 (x f64)))
(load-symbol+ libm sin32 sinf (fcn f32 (x f32)))
(load-symbol+ libm sqrt sqrt (fcn f64 (x f64)))
(load-symbol+ libm sqrt32 sqrtf (fcn f32 (x f32)))

(defvar pi 3.141592653)
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
    (f/ (f- (cast (i64% (cast rand-var i64) 20000) f64) 10000.0) 10000.0)))

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
