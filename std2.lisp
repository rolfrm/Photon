;; The following code runs / compiles
(load "std.lisp")
(load "overload.lisp")
(load "vec2.lisp")

(defcmacro incr (var amount)
  (expr
   (setf (unexpr var) (+ (unexpr var) (unexpr amount)))))

