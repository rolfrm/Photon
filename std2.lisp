;; Builds on top of std.lisp and overload.lisp
(load "std.lisp")
(load "overload.lisp")

(defcmacro incr (var amount)
  (expr
   (setf (unexpr var) (+ (unexpr var) (unexpr amount)))))

(defcmacro range (var from to &rest body)
  (expr
   (let (((unexpr var) (unexpr from)))
     (while (not (eq (unexpr var) (unexpr to)))
       (unexpr (unfold-body (expr progn)
			    body))
       (incr (unexpr var) 1)
       ))))

(load "vec2.lisp")

(type (alias (ptr i32) thread-handle))

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
    (invoke (cast arg (fcn void)))
    null))

(defun launch (void (function (fcn void)))
  (let ((attr (cast (alloc0 64) (ptr i32)))
	(threadid (new-thread-handle)))
    (pthread-attr-init attr)
    (pthread-create threadid (deref attr) thread-launcher (cast function (ptr void)))
    threadid
    (noop)))
