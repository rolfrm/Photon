(load "std2.lisp")
(load "glfw.lisp")
(load "gl.lisp")
(load "gl-ext.lisp")
;(load "truetype.lisp")
;(exit 0)
(glfw:init)
(defvar win (glfw:create-window 400 400 "Flowery!" null null))
(glfw:make-current win)
(defvar prog (gl:create-program))
(defvar frag (gl:create-shader gl:fragment-shader))
(defvar vert (gl:create-shader gl:vertex-shader))

(defvar frag-src "
uniform vec4 color;
void main(){
  gl_FragColor = color;
}
")

(defvar vert-src "
#version 130
in vec2 vertex_position;
uniform vec2 offset;
uniform vec2 size;
uniform vec2 cam;
uniform vec2 cam_size;
void main(){
  gl_Position = vec4((vertex_position * size + offset - cam) / cam_size * 2.0,0.0,1.0);
}
")

(defvar frag-src-len (cast (strlen frag-src) u32))
(defvar vert-src-len (cast (strlen vert-src) u32))
(gl:shader-source frag 1 (addrof frag-src) (addrof frag-src-len))
(gl:compile-shader frag)
(defvar glstatus (cast 0 u32))
(gl:get-shader-info frag gl:compile-status (addrof glstatus))
(if (eq glstatus gl:true)
      (print "success!")
      (print "fail!"))
(print newline)
(gl:shader-source vert 1 (addrof vert-src) (addrof vert-src-len))
(gl:compile-shader vert)
(gl:get-shader-info vert gl:compile-status (addrof glstatus))
(if (eq glstatus gl:true)
      (write-line "success!")
      (write-line "fail!"))
(print newline)
(defvar buffer (cast (alloc 1000) (ptr char)))
(defvar length (cast 0 u32))
(gl:get-shader-info-log vert 1000 (addrof length) buffer)
length
(write-line "--- INFO LOG ---")
(write-line buffer)
(write-line "----------------")
(gl:attach-shader prog frag)
(gl:attach-shader prog vert)
(gl:bind-attrib-location prog 0 "vertex_position")
(gl:link-program prog)
(gl:get-program-info prog gl:link-status (addrof glstatus))
(print "Shader status: " glstatus newline)
(gl:use-program prog)

(defvar offset-loc (gl:get-uniform-location prog "offset"))
(defvar size-loc (gl:get-uniform-location prog "size"))
(defvar color-loc (gl:get-uniform-location prog "color"))
(defvar cam-loc (gl:get-uniform-location prog "cam"))
(defvar cam-size-loc (gl:get-uniform-location prog "cam_size"))

(defvar points (cast null (ptr vec2)))
(defvar points-cnt  0)

(defvar vbo (cast 0 u32))
(gl:gen-buffers 1 (addrof vbo))
(gl:bind-buffer gl:array-buffer vbo)

(defun load-points(void)
  (let ((buf (cast (alloc (* 2 (size-of (type f32)) (cast points-cnt u64))) (ptr f32))))
    (gl:bind-buffer gl:array-buffer vbo)
    (range it 0 points-cnt
	   (let ((point (deref (+ points it))))
	     (setf (deref (+ buf (* it 2)) )
		   (cast (member point x) f32))
	     (setf (deref (+ buf (+ 1 (* it 2))))
		   (cast (member point y) f32))))
    (gl:buffer-data gl:array-buffer (cast (* (cast points-cnt u64) (size-of (type f32)) 2) u32)
		    (cast buf (ptr void)) gl:static-draw)
    (dealloc (cast buf (ptr void)))
    ))

	  
(defvar vbo-circle (cast 0 u32))
(gl:gen-buffers 1 (addrof vbo-circle))
(gl:bind-buffer gl:array-buffer vbo-circle)

(defvar circ-pts (cast 64 i64))
(let ((buf2 (cast (alloc (* 2 (size-of (type f32)) (cast circ-pts u64))) (ptr f32))))
  (range it 0 (cast circ-pts i64)
	 (setf (deref (+ buf2 (* it 2)))
	       (sin32 (* 0.1 (cast it f32))))
     (setf (deref (+ buf2 (* it 2) 1))
	   (cos32 (* 0.1 (cast it f32)))))
  (let ((s (* (size-of (type f32)) (cast circ-pts u64) (cast 2 u64))))

    (gl:buffer-data gl:array-buffer (cast s u32) (cast buf2 (ptr void)) gl:static-draw)
    (dealloc (cast buf2 (ptr void)))))

(defstruct rect
  (upper-left vec2)
  (size vec2))
(defvar rect-default :type rect)
(memset (cast (addrof rect-default) (ptr void)) 0 (size-of (type rect)))


(defvar vbo-grass-boxes (cast 0 u32))
(gl:gen-buffers 1 (addrof vbo-grass-boxes))

(defvar grass-rects (cast null (ptr rect)))
(defvar grass-rect-cnt 0)

(defun make-rect (rect (upper-left vec2) (size vec2))
  (let ((r rect-default))
    (setf (member r upper-left) upper-left)
    (setf (member r size) size)
    r))


(defun load-boxes(void)
  (let ((size (cast (* grass-rect-cnt (cast (size-of (type f32)) i64) 4 2) u64)))
  (let ((buf (cast (alloc size) (ptr f32))))
    ; n rects x 4 vertices x 2 dimensions
    (range it 0 grass-rect-cnt
	   (let ((offset (+ buf (* it 4 2)))
		 (rect1 (deref (+ grass-rects it))))
	     (let ((p1 (member rect1 upper-left))
		   (p2 (+ (member rect1 upper-left) (member rect1 size))))
	       (let ((v1 p1)
		     (v2 (vec (member p1 x) (member p2 y)))
		     (v3 p2)
		     (v4 (vec (member p2 x) (member p1 y))))
		 (setf (deref (+ offset 0)) (cast (member v1 x) f32))
		 (setf (deref (+ offset 1)) (cast (member v1 y) f32))
		 (setf (deref (+ offset 2)) (cast (member v2 x) f32))
		 (setf (deref (+ offset 3)) (cast (member v2 y) f32))
		 (setf (deref (+ offset 4)) (cast (member v3 x) f32))
		 (setf (deref (+ offset 5)) (cast (member v3 y) f32))
		 (setf (deref (+ offset 6)) (cast (member v4 x) f32))
		 (setf (deref (+ offset 7)) (cast (member v4 y) f32))))))
    (gl:bind-buffer gl:array-buffer vbo-grass-boxes)
    (gl:buffer-data gl:array-buffer (cast size u32) (cast buf (ptr void)) gl:static-draw)
    (dealloc (cast buf (ptr void)))
    )))


(gl:enable-vertex-attrib-array 0) 
(defvar player-pos (vec 0 0))
(defvar player-dir (vec 0 1))
(defvar new-pos (vec 0 0))
(defvar cam-pos (vec 0 50))
(defvar cam-size (vec 50 50))
(defvar circles (cast null (ptr vec3)))
(defvar circle-cnt 0)
(defvar speed 1.0)

(defun pt-dist(f64 (a vec2) (b vec2))
  (vec2-length (- a b)))

(defun within-unit (bool (a f64))
  (and (> a 0)
       (< a 1)))

(defun pt-rect-collision(bool (a rect) (b vec2))
  (let ((offset (- b (member a upper-left))))
    (let ((norm (/ offset (member a size))))
      (and (within-unit (member norm x))
	   (within-unit (member norm y))))))

(defun scroll-cb (void (win (ptr void)) (scroll-x f64) (scroll-y f64))
  (let ((amount (+ (* 0.1 scroll-y) 1.0)))
    (setf cam-size (* cam-size amount))))
(glfw:set-scroll-callback win scroll-cb)
(gl:line-width 2.0)

(defun unjitter-axis(f64 (amount f64))
  (let ((dead-zone 0.15))
    (if (> amount dead-zone)
	(- amount dead-zone)
	(if (< amount (- 0 dead-zone))
	    (+ amount dead-zone)
	    0.0))))
  
      

(defun load-game(void)
  (progn
    (print "Reload!" newline)
    (setf player-pos (vec 0 0))
    (setf player-dir (vec 0 1))
    (setf new-pos (vec 0 0))
    (setf cam-pos (vec 0 50))
    ;(setf cam-size (vec 50 50))
    (setf circles (cast null (ptr vec3)))
    (setf circle-cnt 0)
    (setf speed 0.12)

    (clear-list+ grass-rects grass-rect-cnt)
    (add-to-list+ grass-rects grass-rect-cnt (make-rect (vec -500 -500) (vec 1000 500)))

    
    (clear-list+ circles circle-cnt)
    (add-to-list+ circles circle-cnt (vec 8 60 18))
    (add-to-list+ circles circle-cnt (vec -10 95 10))
    (add-to-list+ circles circle-cnt (vec -32 60 20))
    (add-to-list+ circles circle-cnt (vec -10 30 10))
    (add-to-list+ circles circle-cnt (vec 20 30 10))
    (add-to-list+ circles circle-cnt (vec -10 40 8.5))
    (load-boxes)
    (gl:clear-color 0.8 0.8 1.0  1.0 )
    (clear-list+ points points-cnt)
    (add-to-list+ points points-cnt (vec 0 0))
    (add-to-list+ points points-cnt (vec 0 10.0))

    (load-points)
    ))

(load-game)
;(exit 0)
(let ((iteration 0)

      (height (cast 0.0 f64))
      (alive true)
      (dead-timer 0))
  (while (< iteration 40000)
    (let ((ts (timestamp)))
      (let ((cam-len (member cam-size x)))
	(gl:line-width (cast (/ 100 cam-len) f32)))
      (unless alive
	(when (> dead-timer 100)
	  (load-game)
	  (setf alive true)
	  (setf dead-timer 0))
	(incr dead-timer 1)
	)
      (when alive
	(let ((lastpt (deref (+ points (cast (- points-cnt 1) i64)))))
	  (add-to-list+ points points-cnt 
			(+ lastpt 
					;(* (vec (randf) (randf)) speed 1.0)
			   (* player-dir speed)))
	  (let ((turn (if (or (glfw:get-key win glfw:key-a)
			      (glfw:get-key win glfw:key-left))
			  1.0
			  (if (or (glfw:get-key win glfw:key-d)
				  (glfw:get-key win glfw:key-right))
			      -1.0
			      0.0))))
	    (when (and (glfw:joystick-present? 0)
		     (eq turn 0.0))
		(let ((axes-count 0))
		  (let ((axes (glfw:get-joystick-axes 0 (cast (addrof axes-count) (ptr i32)))))
		    (setf turn (unjitter-axis (- 0.0 (cast (deref axes) f64))))
		    (scroll-cb win 0.0 (unjitter-axis (cast (deref (+ axes 4)) f64)))
		    )))
	    (setf player-dir (vec2turn player-dir (* turn 0.1))))
	  (setf cam-pos (deref (+ points (cast (- points-cnt 1) i64))))
	  (load-points))
	
	
	(let ((cheight (deref (+ points (cast (- points-cnt 1) i64)))))
	  (when (> (member cheight y) height)
	    (setf height (member cheight y))
	    (print "height: " height newline))
	  ))
      
      (setf speed (* 0.9995 speed))
      (gl:clear gl:color-buffer-bit)
      (gl:uniform cam-size-loc cam-size)
      (gl:uniform cam-loc cam-pos)

      (gl:uniform color-loc 0.0 0.0 0.0 1)      
      (gl:bind-buffer gl:array-buffer vbo-circle)
      (gl:vertex-attrib-pointer 0 2 gl:float gl:false 0 null) 
      (range it 0 (cast circle-cnt i64)
	     (let ((circle (deref (+ circles it))))
	       (let ((dist (pt-dist cam-pos (vec (member circle x) (member circle y)))))
		 (when (< dist (member circle z))
		   (setf speed (* 0.997 speed))))
	       (gl:uniform offset-loc (vec (member circle x) (member circle y)))
	       (gl:uniform size-loc (vec (member circle z) (member circle z)))
	       (gl:draw-arrays gl:polygon 0 (cast circ-pts u32))))


      (let ((r (deref grass-rects)))
	(setf (member r upper-left)
	    (+ (member r upper-left) (vec 0 0.095)))
	(setf (deref grass-rects) r))
      
      (when alive
	(range it 0 grass-rect-cnt
	       (let ((r (deref (+ grass-rects it))))
		 (when (pt-rect-collision r (deref (+ points (cast (- points-cnt 1) i64))))
		   (gl:clear-color 1.0 0.0 0.0 0.0)
		   (setf alive false)
		   (print "dead." newline)
		   ))))


      (gl:bind-buffer gl:array-buffer vbo)
      (gl:vertex-attrib-pointer 0 2 gl:float gl:false 0 null) 
      (gl:uniform offset-loc (vec 0.0 0.0))
      (gl:uniform size-loc (vec 1 1))
      (gl:uniform color-loc 0.5 0.8 0.3 1)
      (gl:draw-arrays gl:line-strip 0 (cast points-cnt u32))
      
      (load-boxes)
      (gl:bind-buffer gl:array-buffer vbo-grass-boxes)
      (gl:vertex-attrib-pointer 0 2 gl:float gl:false 0 null) 
      (gl:uniform offset-loc (vec 0 0))
      (gl:uniform size-loc (vec 1 1))
      (gl:uniform color-loc 0.2 0.4 0.2 1)
      (gl:draw-arrays gl:quads 0 (cast (* 4 grass-rect-cnt) u32))

      (gl:bind-buffer gl:array-buffer vbo-circle)
      (gl:vertex-attrib-pointer 0 2 gl:float gl:false 0 null) 

      (range it 0 5
      	     (let ((phase (* (cast it f64) (/ 1.0 5.0) pi 2.0)))
      	       (let ((offset (vec (cos phase) (sin phase))))
		 
      		  (gl:uniform offset-loc (+ cam-pos (* offset 0.5)))      
      		  (gl:uniform color-loc 1.0 1.0 0.0 1)      
      		  (gl:uniform size-loc 0.25 0.25)
       		  (gl:draw-arrays gl:polygon 0 (cast circ-pts u32))
		  )))
      
      (gl:uniform offset-loc cam-pos)      
      (gl:uniform color-loc 1.0 1.0 1.0 1.0)      
      (gl:uniform size-loc 0.3 0.3)
      (gl:draw-arrays gl:polygon 0 (cast circ-pts u32))
      
      (gl:uniform offset-loc (vec 0.0 0.0))      
      (setf iteration (+ iteration 1))

      (glfw:swap-buffers win)
      ;(print (- (timestamp) ts) " µs cnt:" points-cnt newline))
      )
    (glfw:poll-events)    

    (usleep 10000)
    ))
