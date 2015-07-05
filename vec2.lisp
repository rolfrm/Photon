(print-type (type (struct _vec2 (x f64) (y f64)))) ; this actually defines a struct named _vec2.
(type (alias _vec2 vec2)) ; defines vec2 as a _vec2 struct.

;numbers:three ;; 3
(progn
  (defvar xy :type vec2)
  (noop))

(defun makevec2 (vec2 (a f64) (b f64))
  (let ((out xy))
    (setf (member out x) a)
    (setf (member out y) b)
    out))

(defun +vec2 (vec2 (a f64) (b f64))
  (makevec2 a b))

(defcmacro vec2op (operator)
  (let ((name (symbol2expr (symbol-combine (quote vec2) (expr2symbol operator)))))
    (expr
     (progn
       (defun (unexpr name) (vec2 (a vec2) (b vec2))
	 (let ((out xy))
	   (setf (member out x) 
		 ((unexpr operator) (member a x) (member b x)))
	   (setf (member out y)
		 ((unexpr operator) (member a y) (member b y)))
	   out))
       (overload (unexpr operator) (unexpr name))))))
(vec2op *)(vec2op +) (vec2op /) (vec2op -)

(defun vec2scale (vec2 (a vec2) (b f64))
  (progn
    (setf (member a x) (* (member a x) b))
    (setf (member a y) (* (member a y) b))
    a))

(defun printvec2 (void (a vec2))
  (progn
    (printstr "(") 
    (print (member a x))
    (printstr ", ")
    (print (member a y))
    (printstr ")")))

(overload * vec2scale)
(overload print printvec2)
