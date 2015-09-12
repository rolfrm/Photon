(defstruct vec2  (x f64) (y f64))
(defstruct vec3  (x f64) (y f64) (z f64))
(defstruct vec4  (x f64) (y f64) (z f64) (w f64))
(defstruct vec2f  (x f32) (y f32))
(defstruct vec3f  (x f32) (y f32) (z f32))
(defstruct vec4f  (x f32) (y f32) (z f32) (w f32))

(defvar vec2-default :type vec2)
(memset (cast (addrof vec2-default) (ptr void)) 0 (size-of (type vec2)))
(defvar vec3-default :type vec3)
(memset (cast (addrof vec3-default) (ptr void)) 0 (size-of (type vec3)))
(defvar vec4-default :type vec4)
(memset (cast (addrof vec4-default) (ptr void)) 0 (size-of (type vec4)))

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
  (let ((out :type vec2))
    (setf (member out x) x)
    (setf (member out y) y)
    out))

(defun makevec2f (vec2f (x f32) (y f32))
  (let ((out :type vec2f))
    (setf (member out x) x)
    (setf (member out y) y)
    out))

(defun makevec3 (vec3 (x f64) (y f64) (z f64))
  (let ((out vec3-default))
    (setf (member out x) x)
    (setf (member out y) y)
    (setf (member out z) z)
    out))

(defun makevec3f (vec3f (x f32) (y f32) (z f32))
  (let ((out :type vec3f))
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
(overload vec makevec2f)
(overload vec makevec3f)
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
(vec2op *) (vec2op +) (vec2op /) (vec2op -)

(defun vec2turn (vec2 (a vec2) (radians f64))
  (let ((sinr (sin radians))
	(cosr (cos radians))
	(x (member a x))
	(y (member a y)))   
    (vec (- (* x cosr) (* y sinr))
	 (+ (* y cosr) (* x sinr)))))

(defun vec2:eq (bool (a vec2) (b vec2))
  (macrolet ((ax (member a x)) (ay (member a y))
	     (bx (member b x)) (by (member b y)))
    (and (eq ax bx) (eq ay by))))

(defun vec2scale (vec2 (a vec2) (b f64))
  (vec (* (member a x) b)  (* (member a y) b)))

(defun printvec2 (void (a vec2))
  (print "(" (member a x) " " (member a y) ")"))

(defun printvec3 (void (a vec3))
  (print "(" (member a x) " " (member a y) " " (member a z) ")"))

(defun printvec4 (void (a vec4))
  (print "(" (member a x) " " (member a y) " " (member a z) " " (member a w) ")"))

(overload * vec2scale)
(overload print printvec2)
(overload print printvec3)
(overload print printvec4)

(defstruct mat4
  (m00 f64) (m01 f64) (m02 f64) (m03 f64) 
  (m10 f64) (m11 f64) (m12 f64) (m13 f64) 
  (m20 f64) (m21 f64) (m22 f64) (m23 f64) 
  (m30 f64) (m31 f64) (m32 f64) (m33 f64))

(defstruct mat4f
  (m00 f32) (m01 f32) (m02 f32) (m03 f32) 
  (m10 f32) (m11 f32) (m12 f32) (m13 f32) 
  (m20 f32) (m21 f32) (m22 f32) (m23 f32) 
  (m30 f32) (m31 f32) (m32 f32) (m33 f32))

(defun mat4-to-mat4f (mat4f (m mat4))
  (let ((out :type mat4f))
    (let ((inptr (cast (addrof m) (ptr f64)))
	  (optr (cast (addrof out) (ptr f32))))
      (range it 0 16
	     (setf (deref (+ optr it)) (cast (deref (+ inptr it)) f32))))
    out))
	   
(defvar mat4-default :type mat4)
(zeroize mat4-default)

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
				  (from-a (deref (aref a j i))))
			      (incr v (* from-b from-a))))
		     v))))
    vout))

(defun translation-matrix (mat4 (offset vec3))
  (let ((mat mat4-eye))
    (setf (member mat m03) (member offset x))
    (setf (member mat m13) (member offset y))
    (setf (member mat m23) (member offset z))
    mat))

(defun projection-matrix (mat4 (width f64) (height f64) (near f64) (far f64))
  (let ((r (* width 0.5))
	(t (* height 0.5)))
    (let ((o mat4-default))
      (setf (member o m00) (/ near r))
      (setf (member o m11) (/ near t))
      (setf (member o m22) (/ (* (+ near far) -1) (- far near)))
      (setf (member o m23) (/ (* far near -2) (- far near)))
      (setf (member o m32) -1)
      o)))

(overload dot mat4-dot)
(overload dot mat4vec4-dot)	   

(defun mat4-print (void (mat mat4))
  (progn
    (print (member mat m00) (member mat m01) (member mat m02) (member mat m03) newline)
    (print (member mat m10) (member mat m11) (member mat m12) (member mat m13) newline)
    (print (member mat m20) (member mat m21) (member mat m22) (member mat m23) newline)
    (print (member mat m30) (member mat m31) (member mat m32) (member mat m33) newline)
    ))
(overload print mat4-print)

(defun homogenize (vec3 (a vec4))
  (vec (/ (member a x) (member a w))
       (/ (member a y) (member a w))
       (/ (member a z) (member a w))))

(defun mat4-rot-x (mat4 (a f64))
  (let ((m mat4-eye)
	(c (cos a))
	(s (sin a)))
    (setf (member m m11) c)
    (setf (member m m21) (- 0 s))
    (setf (member m m12) s)
    (setf (member m m22) c)
    m))

(defun mat4-rot-y (mat4 (a f64))
  (let ((m mat4-eye)
	(c (cos a))
	(s (sin a)))
    (setf (member m m00) c)
    (setf (member m m20) (- 0 s))
    (setf (member m m02) s)
    (setf (member m m22) c)
    m))

(defun mat4-rot-z (mat4 (a f64))
  (let ((m mat4-eye)
	(c (cos a))
	(s (sin a)))
    (setf (member m m00) c)
    (setf (member m m10) (- 0 s))
    (setf (member m m01) s)
    (setf (member m m11) c)
    m))

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

(defmacro . (a b)
  (expr (dot (unexpr a) (unexpr b))))


;; (defvar p (projection-matrix 10.0 10.0 0.1 10.1 ))
;; (print p)
;; (print (homogenize (dot p (vec 0 0 10 1))) newline)
;; (mat4-to-mat4f p)
;;(print (dot (mat4-rot-y (* pi 0.5)) (vec 1 0 0 1)) newline)
;;(exit 0)
