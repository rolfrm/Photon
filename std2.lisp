;; The following code runs / compiles
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
