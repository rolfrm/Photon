(type (alias
       (struct _vec2 (x f64) (y f64))
       vec2))

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

(type 
 (alias
  (struct _mat4
	  (m00 f64) (m01 f64) (m02 f64) (m03 f64) 
	  (m10 f64) (m11 f64) (m12 f64) (m13 f64) 
	  (m20 f64) (m21 f64) (m22 f64) (m23 f64) 
	  (m30 f64) (m31 f64) (m32 f64) (m33 f64))
  mat4))

(defvar mat4-default :type mat4)
(memset (cast (addrof mat4-default) (ptr void)) 0 (size-of (type mat4)))

(defvar eye mat4-default)
(setf (member eye m00) 1)
(setf (member eye m11) 1)
(setf (member eye m22) 1)
(setf (member eye m33) 1)

(defcmacro matop (op)
  (let ((fcn-name (symbol2expr (symbol-combine (quote mat4) (expr2symbol op) ))))
    (expr 
     (progn
       (defun (unexpr fcn-name) (mat4 (a mat4) (b mat4))
	 (let ((mout mat4-default))
	   (let ((aptr (cast (addrof a) (ptr f64)))
		 (bptr (cast (addrof b) (ptr f64)))
		 (optr (cast (addrof mout) (ptr f64))))
	     (range it 0 16
		    (setf (deref (ptr+ optr it))
			  ((unexpr op) (deref (ptr+ aptr it))
			   (deref (ptr+ bptr it))))))
	   mout))
       (overload (unexpr op) (unexpr fcn-name))))))

(matop +)
(matop *)
(matop /)
(matop -)

(defun mat4-scale (mat4 (mat mat4) (scalar f64))
  (let ((mout mat4-default))
    (let ((aptr (cast (addrof mat) (ptr f64)))
	  (optr (cast (addrof mout) (ptr f64))))
      (range it 0 16
	     (setf (deref (ptr+ optr it))
		   (* (deref (ptr+ aptr it)) scalar))))
    mout))



(overload * mat4-scale)

(defoverloaded dot)

(defun mat4-dot (mat4 (a mat4) (b mat4))
  (let ((mout mat4-default))
    (let ((aptr (cast (addrof a) (ptr f64)))
	  (bptr (cast (addrof b) (ptr f64)))
	  (optr (cast (addrof mout) (ptr f64))))
      (range c 0 4
	     (range r 0 4
		    (let ((cell (ptr+ optr (+ (* c 4) r))))
		      (range k 0 4
			     (incr (deref cell)
				   (* (deref (ptr+ aptr (+ (* c 4) k)))
				      (deref (ptr+ bptr (+ (* k 4) r))))))))))
    mout))

(overload dot mat4-dot)
	   

(defun mat4-print (void (mat mat4))
  (progn
    (print (member mat m00))
    (printstr " ")
    (print (member mat m01))
    (printstr " ")
    (print (member mat m02))
    (printstr " ")
    (print (member mat m03))
    (printstr "\n")
    (print (member mat m10))
    (printstr " ")
    (print (member mat m11))
    (printstr " ")
    (print (member mat m12))
    (printstr " ")
    (print (member mat m13))
    (printstr "\n")
    (print (member mat m20))
    (printstr " ")
    (print (member mat m21))
    (printstr " ")
    (print (member mat m22))
    (printstr " ")
    (print (member mat m23))
    (printstr "\n")    
    (print (member mat m30))
    (printstr " ")
    (print (member mat m31))
    (printstr " ")
    (print (member mat m32))
    (printstr " ")
    (print (member mat m33))))
(overload print mat4-print)
