;; The following code runs / compiles
(load "std2.lisp")
(load "glfw.lisp")
(load "gl.lisp")

(defun gl-uniform-vec2 (void (location i32) (v2 vec2))
  (gl:uniform location 
	      (cast (member v2 x) f32) (cast (member v2 y) f32)))
(defun gl-uniform-vec3 (void (location i32) (v2 vec3))
  (gl:uniform location 
	      (cast (member v2 x) f32) (cast (member v2 y) f32) 
	      (cast (member v2 z) f32)))
(defun gl-uniform-vec4 (void (location i32) (v2 vec4))
  (gl:uniform location 
	      (cast (member v2 x) f32) (cast (member v2 y) f32) 
	      (cast (member v2 z) f32) (cast (member v2 w) f32)))
(overload gl:uniform gl-uniform-vec2)
(overload gl:uniform gl-uniform-vec3)
(overload gl:uniform gl-uniform-vec4)

(glfw:init)
(defvar win (glfw:create-window 512 512 "test.." null null))
(glfw:make-current win)
(glfw:set-clipboard-string win "clipboard test!")
(defvar sleeptime (cast 10000 i32))
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

;;; -- Load Vertex Buffer Object -- ;;;
(defvar vbo (cast 0 u32))
(defvar vbo-data (cast (alloc0 (u64* 8 4)) (ptr f32))) ; 4 floats
(setf (deref (ptr+ vbo-data 2)) 1.0)
(setf (deref (ptr+ vbo-data 4)) 1.0)
(setf (deref (ptr+ vbo-data 5)) 1.0)
(setf (deref (ptr+ vbo-data 7)) 1.0)
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
(defvar cam-size-loc (gl:get-uniform-location prog "cam_size"))
(defvar iteration 0)
(defun mouse-callback (void (win-ptr (ptr void)) (button i32) (action i32) (mods i32))
  (write-line "mouse callback!"))
(defvar player-pos (vec 500 500))
(defvar new-pos (vec 500 500))
(defvar cam-pos (vec 500 500))
(defvar cam-size (vec 50 50))

(defun key-callback (void (win-ptr (ptr void)) (key i32)(scancode i32) (action i32) (mods i32))
  (let ((k64 (cast key i64)))
    (when (eq action 1)
      (setf new-pos player-pos)
      (when (eq k64 glfw:key-up)
	(incr (member new-pos y) 1))
      (when (eq k64 glfw:key-down)
	(incr (member new-pos y) -1))
      (when (eq k64 glfw:key-left)
	(incr (member new-pos x) -1))
      (when (eq k64 glfw:key-right)
	(incr (member new-pos x) 1))
      (when (eq k64 glfw:key-space)
	(setf cam-pos new-pos)))
    (print "Key: ")
    (print key)
    (print " ")
    (print action)
    (print newline)
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
(defvar tiles-height 1000)
(defvar tiles-width 1000)
(defvar tiles (cast (alloc0 (cast (* tiles-height tiles-width) u64)) (ptr i8))) 

(defun get-tile((ptr i8) (x i64) (y i64))
  (ptr+ tiles (+ x (* tiles-width y))))

;; (for it2 0 (< it2 100) (i64+ it2 1)
;;      (for it 0 (not (eq it 10)) (i64+ it 1)
;; 	  (setf (deref (get-tile it it2)) (cast (+ 1 (i64% (+ it it2) 3)) i8))))


(defvar color-lut (cast (alloc (* (size-of (type vec4)) 16)) (ptr vec4)))
(setf (deref color-lut) (vec 0 0 0 0))
(setf (deref (ptr+ color-lut 1)) (vec 1 0 0 1))
(setf (deref (ptr+ color-lut 2)) (vec 0 1 0 1))
(setf (deref (ptr+ color-lut 3)) (vec 0 0 1 1))

(defun render-tiles-in-view (void)
  (let ((cam-left (cast (- (member cam-pos x) (member cam-size x)) i64))
	(cam-right (cast (+ (member cam-pos x) (member cam-size x)) i64))
	(cam-top (cast (- (member cam-pos y) (member cam-size y)) i64))
	(cam-bottom (cast (+ (member cam-pos y) (member cam-size y)) i64)))
    (gl:uniform size-loc 1.0 1.0)
    (gl:uniform color-loc 1 1 1 1)
    
    (for row (max 0 cam-top) (<= row (min cam-bottom tiles-height)) (i64+ row 1)
	 (for col (max 0 cam-left) (<= col (min cam-right tiles-width)) (i64+ col 1)
	      (let ((fx (cast col f32))
		    (fy (cast row f32)))
		(let ((tile (deref (get-tile col row))))
		  (unless (eq 0 tile)
		    (gl:uniform color-loc (deref (ptr+ color-lut (cast tile i64))))
		    (gl:uniform offset-loc fx fy)
		    (gl:draw-arrays drawtype 0 pts)))
	      )))))

(defun vec2-eq(bool (a vec2) (b vec2))
  (and
   (eq (member a x) (member b x))
   (eq (member a y) (member b y))))

(defun update-player(void (data (ptr void)))
  (while true
    (unless (vec2-eq player-pos new-pos)
      (let ((x (cast (member new-pos x) i64))
	    (y (cast (member new-pos y) i64)))
      
	(when (and (>= x 0)
		   (>= y 0))
	  (let ((tile (get-tile x y)))
	    
	    (unless (eq (deref tile) 0)
	      (setf player-pos new-pos)
	      ;(setf (deref tile) 0)
	      ))))
      (setf new-pos player-pos))
    (ccyield)))


(defvar positions (cast (alloc (* (size-of (type vec2)) 16)) (ptr vec2)))
(defvar cc (ccstart))

(ccthread cc update-player null)

(defun update-cell(void (col i64) (row i64))
  (let (( tile (get-tile col row)))
    (setf (deref tile) (cast (i64+ 1 (i64% (i64+ (cast (deref tile) i64) 1) 3)) i8))))

(defun ccwait(void (seconds f64))
  (let ((start (timestamp)))
    (let ((end (+ start (cast (* 1000000 seconds) i64))))
      (while (<= (timestamp) end)
	(ccyield)))))

(defvar offsets (cast (alloc (* (size-of (type vec2)) 16)) (ptr vec2)))
(setf (deref offsets) (vec 500 500))

(defun tileupdater-move (void (dx i64) (dy i64) (x (ptr i64)) (y (ptr i64)))
  (progn
    (incr (deref x) dx)
    (incr (deref y) dy)
    (incr (deref (get-tile (deref x) (deref y))) (cast 1 i8))
    (ccwait 0.01)
    ))

(defvar tileupdater 
  (lambda (void (arg (ptr void)))
    (let ((col 0) (row 0)
	  (offset (vec 500 500)))
      (setf col 500)
      (setf row 500)
      (tileupdater-move 0 1 (addrof col) (addrof row))
      (tileupdater-move 1 0 (addrof col) (addrof row))

      (for d 2 (< d 100) (+ d 1)
	   (range i 0 (+ 1 d)
		  (tileupdater-move 0 1 (addrof col) (addrof row)))
	   (range i 0 (+ d 1)
		(tileupdater-move -1 0 (addrof col) (addrof row)))
	   (setf d (+ 1 d))
	   (range i 0 d
		(tileupdater-move 0 -1 (addrof col) (addrof row)))
	   (range i 0 d
		(tileupdater-move 1 0 (addrof col) (addrof row)))
	   (tileupdater-move 0 1 (addrof col) (addrof row))
	   (tileupdater-move 1 0 (addrof col) (addrof row))
	))))

(defun bleep (void (arg (ptr void)))
  (progn
    (ccwait 1)
    (ccwait 1)))

(ccthread cc  (deref tileupdater) null)
(while (< iteration 40000)
  (gl:uniform cam-size-loc cam-size)
  (gl:uniform cam-loc cam-pos)
  (setf iteration (+ iteration 1))
  (gl:clear-color 0.0  0.0 0.0  1.0 )
  (gl:clear gl:color-buffer-bit)
  (render-tiles-in-view)
  (gl:uniform offset-loc player-pos)
  (gl:uniform size-loc (vec 1 1))
  (gl:uniform color-loc 0.5 0.6 0.7 1)
  
  (gl:draw-arrays drawtype 0 pts)
  (glfw:swap-buffers win)
  (glfw:poll-events)    
  (ccstep cc)
  (usleep sleeptime)
  )


