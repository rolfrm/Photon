;; The following code runs / compiles
(load "std2.lisp")
(load "glfw.lisp")
(load "gl.lisp")

(defun gl-uniform-vec2 (void (location i32) (v2 vec2))
  (gl:uniform location (cast (member v2 x) f32) (cast (member v2 y) f32)))

(overload gl:uniform gl-uniform-vec2)

(glfw:init)
(defvar win (glfw:create-window 512 512 "test.." null null))
(glfw:make-current win)
(glfw:set-clipboard-string win "clipboard test!")
(defvar sleeptime (cast 30000 i32))
(defvar r 0.0)
;;; -- Load Shader Program -- ;;;
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
void main(){
  gl_Position = vec4(vertex_position * size + offset - cam,0.0,1.0);
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
(gl:shader-source vert 1 (addrof vert-src) (addrof vert-src-len))
(gl:compile-shader vert)
(gl:get-shader-info vert gl:compile-status (addrof glstatus))
(if (eq glstatus gl:true)
      (write-line "success!")
      (write-line "fail!"))
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
(write-line "status:")
glstatus
(gl:use-program prog)

;;; -- Load Vertex Buffer Object -- ;;;
(defvar vbo (cast 0 u32))
(defvar vbo-data (cast (alloc0 (u64* 8 4)) (ptr f32))) ; 4 floats
(setf (deref (ptr+ vbo-data 2)) 0.25)
(setf (deref (ptr+ vbo-data 4)) 0.25)
(setf (deref (ptr+ vbo-data 5)) 0.25)
(setf (deref (ptr+ vbo-data 7)) 0.25)
(gl:gen-buffers 1 (addrof vbo))
(gl:bind-buffer gl:array-buffer vbo)

(gl:buffer-data gl:array-buffer (u32* 8 4) (cast vbo-data (ptr void)) gl:static-draw)

(gl:enable-vertex-attrib-array 0)
(gl:bind-buffer gl:array-buffer vbo)
(gl:vertex-attrib-pointer 0 2 gl:float gl:false 0 null) 
(gl:get-error)

  
(defvar pts (cast 4 u32))
(defvar drawtype gl:quads)
(defvar offset-loc (gl:get-uniform-location prog "offset"))
(defvar size-loc (gl:get-uniform-location prog "size"))
(defvar color-loc (gl:get-uniform-location prog "color"))
(defvar cam-loc (gl:get-uniform-location prog "cam"))
(defvar iteration 0)
(defun mouse-callback (void (win-ptr (ptr void)) (button i32) (action i32) (mods i32))
  (write-line "mouse callback!"))
(defvar player-pos (vec 0 0))
(defvar cam-pos (vec 0 0))
(defvar cam-size (vec 10 10))

(defun key-callback (void (win-ptr (ptr void)) (key i32)(scancode i32) (action i32) (mods i32))
  (let ((k64 (cast key i64)))
    (when (eq k64 glfw:key-up)
      (incr (member player-pos y) 0.1))
    (when (eq k64 glfw:key-down)
      (incr (member player-pos y) -0.1))
    (when (eq k64 glfw:key-left)
      (incr (member player-pos x) -0.1))
    (when (eq k64 glfw:key-right)
      (incr (member player-pos x) 0.1))
    (when (eq k64 glfw:key-space)
      (setf cam-pos player-pos))
    (print "Key: ")
    (print key)
    (print "\n")
    ))

(defvar mpos (makevec2 0 0))

(defun cursor-pos-callback (void (win-ptr (ptr void)) (x f64) (y f64))
  (progn
    (print (setf mpos (makevec2 x y)))
    (print "\n")))

(defun error-callback (void (code i32) (str (ptr char)))
  (write-line str))

(defun cursor-enter (void (win (ptr void)) (enter i32))
  (if (eq enter 1)
      (write-line "ENTER")
      (write-line "LEAVE")))

(defun close-window (void (win (ptr void)))
  (progn
    (printstr "Shutdown..")
    (exit 0)))

(glfw:set-mouse-button-callback win mouse-callback)
(glfw:set-key-callback win  key-callback)
(glfw:set-cursor-pos-callback win cursor-pos-callback)
(glfw:set-error-callback error-callback)
(glfw:set-cursor-enter-callback win  cursor-enter)
(glfw:set-window-close-callback win close-window)
(glfw:joystick-present? 1)

;; Game play

(defvar tiles-height 100)
(defvar tiles-width 1000)
(defvar tiles (cast (alloc0 (cast (* tiles-height tiles-width) u64)) (ptr i8))) 

(defun get-tile((ptr i8) (x i64) (y i64))
  (ptr+ tiles (+ x (* tiles-width y))))

(for it2 0 (< it2 100) (i64+ it2 1)
     (for it 0 (not (eq it 10)) (i64+ it 1)
	  (setf (deref (get-tile it it2)) (cast (+ 1 (i64% (+ it it2) 3)) i8))))


(defun render-tiles-in-view (void)
  (let ((cam-left (cast (member cam-pos x) i64))
	(cam-right (cast (+ (member cam-pos x) (member cam-size x)) i64))
	(cam-top (cast (member cam-pos y) i64))
	(cam-bottom (cast (+ (member cam-pos y) (member cam-size y)) i64)))
    (gl:uniform size-loc 0.4 0.4)
    (gl:uniform color-loc 1 1 1 1)
    (for row (max 0 cam-top) (< row (min cam-bottom tiles-height)) (i64+ row 1)
	 (for col (max 0 cam-left) (< col (min cam-right tiles-width)) (i64+ col 1)
	      (let ((fx (* 0.1 (cast row f32)))
		    (fy (* 0.1 (cast col f32))))
		(let ((tile (deref (get-tile col row))))
		  (unless (eq 0 tile)
		    (gl:uniform color-loc 1 0 0 1)
		    (when (eq tile 1)
		      (gl:uniform color-loc 1 1 1 1))
		    (when (eq tile 2)
		      (gl:uniform color-loc 1 0 1 1))
		    (gl:uniform offset-loc fx fy)
		    (gl:draw-arrays drawtype 0 pts)))
	      )))))

(while (< iteration 2000)
  (gl:uniform cam-loc cam-pos)
  (setf iteration (+ iteration 1))
  (gl:clear-color 0.0  0.2 0.0  1.0 )
  (gl:clear gl:color-buffer-bit)
  (render-tiles-in-view)
  (gl:uniform offset-loc player-pos)
  (gl:uniform size-loc (vec 0.2 0.2))
  (gl:uniform color-loc 0.5 0.6 0.7 1)
  
  (gl:draw-arrays drawtype 0 pts)
  (glfw:swap-buffers win)
  (glfw:poll-events)    
  (usleep sleeptime))

