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

(defvar points (cast (alloc0 (* 0 (size-of (type vec2)))) (ptr vec2)))
(defvar points-cnt (cast 0 u64))
(add-to-list+ points points-cnt (vec 0 10))
(add-to-list+ points points-cnt (vec 2 20))
(add-to-list+ points points-cnt (vec -2 30))
(range it 0 (cast points-cnt i64)
       (print it (deref (ptr+ points it)) newline))
(print points-cnt newline)

(defvar vbo (cast 0 u32))
(gl:gen-buffers 1 (addrof vbo))
(gl:bind-buffer gl:array-buffer vbo)

(defun load-points(void)
  (let ((buf (cast (alloc (* 2 (size-of (type f32)) points-cnt)) (ptr f32))))
    (range it 0 points-cnt
	   (let ((point (deref (ptr+ points it))))
	     (setf (ptr+ buf (* it 2)) 
		   (member point x))
	     (setf (ptr+ buf (+ 1 (* it 2))) 
		   (member point y))))
    (gl:buffer-data gl:array-buffer (* points-cnt (size-of (type f32)) 2) 
		    (cast points (ptr void)) gl:static-draw)))
