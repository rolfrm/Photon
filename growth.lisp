(load "std2.lisp")
(load "glfw.lisp")
(load "gl.lisp")
(load "gl-ext.lisp")

(glfw:init)
(defvar win (glfw:create-window 512 512 "test.." null null))
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

(defvar points (cast (alloc0 (* 0 (size-of (type vec2)))) (ptr vec2)))
(defvar points-cnt (cast 0 u64))
(add-to-list+ points points-cnt (vec 0 10))
(add-to-list+ points points-cnt (vec 2 20))
(add-to-list+ points points-cnt (vec -2 30))
(add-to-list+ points points-cnt (vec -10 40))
	  (add-to-list+ points points-cnt (vec 0 50))
(range it 0 (cast points-cnt i64)
       (print it (deref (ptr+ points it)) newline))
(print points-cnt newline)

(defvar vbo (cast 0 u32))
(gl:gen-buffers 1 (addrof vbo))
(gl:bind-buffer gl:array-buffer vbo)

(defun load-points(void)
  (let ((buf (cast (alloc (* 2 (size-of (type f32)) points-cnt)) (ptr f32))))
    (range it 0 (cast points-cnt i64)
	   (let ((point (deref (ptr+ points it))))
	     (setf (deref (ptr+ buf (* it 2)) )
		   (cast (member point x) f32))
	     (setf (deref (ptr+ buf (+ 1 (* it 2))))
		   (cast (member point y) f32))))
    (gl:buffer-data gl:array-buffer (cast (* points-cnt (size-of (type f32)) 2) u32)
		    (cast buf (ptr void)) gl:static-draw)))
(load-points)
(gl:enable-vertex-attrib-array 0)
(gl:bind-buffer gl:array-buffer vbo)
(gl:vertex-attrib-pointer 0 2 gl:float gl:false 0 null) 
(gl:clear-color 0.0  0.0 0.0  1.0 )
(defvar player-pos (vec 0 0))
(defvar player-dir (vec 0 1))
(defvar new-pos (vec 0 0))
(defvar cam-pos (vec 0 50))
(defvar cam-size (vec 100 100))
(let ((iteration 0) )
  (while (< iteration 4000)
    (let ((ts (timestamp)))
      (let ((lastpt (deref (ptr+ points (cast (- points-cnt 1) i64)))))
	(add-to-list+ points points-cnt 
		      (+ lastpt player-dir))
	(let ((turn (if (or (glfw:get-key win glfw:key-a)
			    (glfw:get-key win glfw:key-left))
			1.0
			(if (or (glfw:get-key win glfw:key-d)
				(glfw:get-key win glfw:key-right))
			    -1.0
			    0.0))))
	  (setf player-dir (vec2turn player-dir (* turn 0.1)))) 
	(setf cam-pos (deref (ptr+ points (cast (- points-cnt 1) i64))))
	(load-points))
      (gl:uniform cam-size-loc cam-size)
      (gl:uniform cam-loc cam-pos)
      (setf iteration (+ iteration 1))
					
      (gl:clear gl:color-buffer-bit)
      (gl:uniform offset-loc player-pos)
      (gl:uniform size-loc (vec 1 1))
      (gl:uniform color-loc 0.5 0.6 0.7 1)
      (gl:draw-arrays gl:line-strip 0 (cast points-cnt u32))
      (glfw:swap-buffers win)
      (print (- (timestamp) ts) " µs cnt:" points-cnt newline))

    (glfw:poll-events)    

    (usleep 10000)
    ))
