(defstruct vec2  (x f64) (y f64))
(defstruct vec3  (x f64) (y f64) (z f64))
(defstruct vec4  (x f64) (y f64) (z f64) (w f64))
(defstruct vec2f  (x f32) (y f32))
(defstruct vec3f  (x f32) (y f32) (z f32))
(defstruct vec4f  (x f32) (y f32) (z f32) (w f32))

;numbers:three ;; 3
(defvar vec2-default :type vec2)
(memset (cast (addrof vec2-default) (ptr void)) 0 (size-of (type vec2)))
(defvar vec3-default :type vec3)
(memset (cast (addrof vec3-default) (ptr void)) 0 (size-of (type vec3)))
(defvar vec4-default :type vec4)
(memset (cast (addrof vec4-default) (ptr void)) 0 (size-of (type vec4)))

;; (defmacro decl-vec-type (n-elements type)
;;   ;(let ((
;;   (expr
;;    (defstruct (vec (unexpr n-lements) (unexpr type))
;;      (


(print (type vec2) newline)

(defmacro vec-dim (vec-expr)
  (let ((n (let ((t1 (type-of vec-expr)))
	     (when (eq (cast t1 (ptr void)) null)
	       (print "Error unable to evaluate " vec-expr  newline))
	     (if (eq t1 (type vec2))
		 2
		 (if (eq t1 (type vec3))
		     3
		     4)))))
    (number2expr n)))

(defun vec2-length (f64 (a vec2))
  (let ((x (member a x))
	(y (member a y)))
    (sqrt (+ (* x x) (* y y)))))

(defoverloaded aref)
(defun vec2-aref ((ptr f64) (a vec2) (idx i64))
  (ptr+ (cast (addrof a) (ptr f64)) idx))

(defun vec3-aref ((ptr f64) (a vec3) (idx i64))
  (ptr+ (cast (addrof a) (ptr f64)) idx))

(defun vec4-aref ((ptr f64) (a vec4) (idx i64))
  (ptr+ (cast (addrof a) (ptr f64)) idx))

(overload aref vec2-aref)
(overload aref vec3-aref)
(overload aref vec4-aref)

(defun makevec2 (vec2 (x f64) (y f64))
  (let ((out vec2-default))
    (setf (member out x) x)
    (setf (member out y) y)
    out))

(defun makevec3 (vec3 (x f64) (y f64) (z f64))
  (let ((out vec3-default))
    (setf (member out x) x)
    (setf (member out y) y)
    (setf (member out z) z)
    out))

(defun makevec4 (vec4 (x f64) (y f64) (z f64) (w f64))
  (let ((out vec4-default))
    (setf (member out x) x)
    (setf (member out y) y)
    (setf (member out z) z)
    (setf (member out w) w)
    out))

(defoverloaded vec)
(overload vec makevec2)
(overload vec makevec3)
(overload vec makevec4)

(print " " (vec-dim (vec 1 2 3)) newline)


(defmacro vec2op (operator)
  (let ((name (expr (vec2 (unexpr operator)))))
    (let ((r (expr
	     (progn
	       (defun (unexpr name) (vec2 (a vec2) (b vec2))
		 (let ((out vec2-default))
		   (setf (member out x) 
			 ((unexpr operator) (member a x) (member b x)))
		   (setf (member out y)
			 ((unexpr operator) (member a y) (member b y)))
		   out))
	       (overload (unexpr operator) (unexpr name))))))
      r)))
(vec2op *)
(vec2op +) (vec2op /) (vec2op -)

(defun vec2turn (vec2 (a vec2) (radians f64))
  (let ((sinr (sin radians))
	(cosr (cos radians))
	(x (member a x))
	(y (member a y)))   
    (vec (- (* x cosr) (* y sinr))
	 (+ (* y cosr) (* x sinr)))))

(defun vec2:eq (bool (a vec2) (b vec2))
  (and (eq (member a x) (member b x))
       (eq (member a y) (member b y))))

;(let (( a (vec2turn (vec2 1 1) pi)) (b (vec2:rot90 (vec2:rot90 (vec2 1 1)))))
;  (assert (eq 

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

(defun printvec3 (void (a vec3))
  (progn
    (printstr "(") 
    (print (member a x))
    (printstr ", ")
    (print (member a y))
    (printstr ", ")
    (print (member a z))
    (printstr ")")))


(defun printvec4 (void (a vec4))
  (progn
    (printstr "(") 
    (print (member a x))
    (printstr ", ")
    (print (member a y))
    (printstr ", ")
    (print (member a z))
    (printstr ", ")
    (print (member a w))
    (printstr ")")))

(overload * vec2scale)
(overload print printvec2)
(overload print printvec3)
(overload print printvec4)

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
(defun mat4-aref ((ptr f64) (a mat4) (row i64) (col i64))
  (ptr+ (cast (addrof a) (ptr f64)) 
	(the (+ (* col 4) row ) i64)))

(overload aref mat4-aref)

(defvar mat4-eye mat4-default)
(setf (member mat4-eye m00) 1)
(setf (member mat4-eye m11) 1)
(setf (member mat4-eye m22) 1)
(setf (member mat4-eye m33) 1)

(defmacro matop (op)
  (let ((fcn-name (expr (mat4 (unexpr op)))))
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

(defmacro vref (item)
  (expr (cast (addrof (unexpr item)) (ptr f64))))

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
		    (let ((cell (ptr+ optr (+ (* 4 c) r))))
		      (range k 0 4
			     (incr (deref cell) 
				   (* (deref (ptr+ aptr (+ (* c 4) k)))
				      (deref (ptr+ bptr (+ (* k 4) r))))))))))
    mout))

(defun mat4vec4-dot (vec4 (a mat4) (b vec4))
  (let ((vout vec4-default))
    (range i 0 4
	   (let ((cell (cast (addrof vout) (ptr f64))))
	     (setf (deref (ptr+ cell i))
		   (let ((v 0.0))
		     (range j 0 4 
			    (let ((from-b (deref (aref b j)))
				  (from-a (deref (aref a i j))))
			      (incr v (* from-b from-a))))
		     v))))
    vout))

(defun translation-matrix (mat4 (offset vec3))
  (let ((mat mat4-eye))
    (setf (member mat m03) (member offset x))
    (setf (member mat m13) (member offset y))
    (setf (member mat m23) (member offset z))
    mat))

(overload dot mat4-dot)
(overload dot mat4vec4-dot)	   

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

(defun vec2-normalize (vec2 (a vec2))
  (let ((len (vec2-length a)))
    (if (eq len 0.0)
	a
	(* a (/ 1.0 len)))))

(defun vec2:rot90 (vec2 (a vec2))
  (vec (- 0 (member a y)) (member a x)))

(defun vec2:rot-90 (vec2 (a vec2))
  (vec (member a y) (- 0 (member a x))))

(defun vec2:floor (vec2 (a vec2))
  (vec (floor (member a x)) (floor (member a y))))
