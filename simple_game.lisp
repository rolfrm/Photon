;; The following code runs / compiles
(load "std2.lisp")
(printstr "loading gl..")
(load "glfw.lisp")
(load "gl.lisp")
(printstr "starting game..")


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
void main(){
  gl_Position = vec4(vertex_position + offset,0.0,1.0);
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
(defvar vbo-data (cast (alloc (u64* 8 4)) (ptr f32))) ; 4 floats
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
(defvar uloc (gl:get-uniform-location prog "offset"));
(defvar iteration 0)
(defun mouse-callback (void (win-ptr (ptr void)) (button i32) (action i32) (mods i32))
  (write-line "mouse callback!"))

(defun key-callback (void (win-ptr (ptr void)) (key i32)(scancode i32) (action i32) (mods i32))
  (printf "KEY: %c\n" (cast key i64)))

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

(glfw:set-mouse-button-callback win (addrof mouse-callback))
(glfw:set-key-callback win (addrof key-callback))
(glfw:set-cursor-pos-callback win (addrof cursor-pos-callback))
(glfw:set-error-callback (addrof error-callback))
(glfw:set-cursor-enter-callback win (addrof cursor-enter))
(glfw:joystick-present? 1)

(while (not (eq iteration 2))
  (let ((m (* (- (makevec2 256 256) mpos) 0.004)))
    (setf iteration (+ iteration 1))
    (gl:clear-color 0.0  0.2 0.0  1.0 )
    (gl:clear gl:color-buffer-bit)
    (gl:uniform uloc (cast (- 0 (member m x)) f32) (cast (member m y) f32))
    (gl:uniform (gl:get-uniform-location prog "color") 1 0 0 1)
    (gl:draw-arrays drawtype 0 pts)
    (glfw:swap-buffers win)
    (glfw:poll-events)    
    (usleep sleeptime)))


