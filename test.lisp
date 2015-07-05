;; The following code runs / compiles
(load "std2.lisp")
(assert (eq false (eq 0 1)))

;; Constants
1 
"hello world!"

;; Function calls
(i64+ 10 10)

;(defvar i65+ (cast (addrof i64+) (fcn i8 (a i8) (b i8))))

;; Defining functions
(defun add (f64 (a f64) (b f64)) (f+ a b))
(add 45 67)

;; Variables and functions can be named any unicode thing, except things starting with '('/')'
;; and they cannot contain whitespace either

;; Defining / using variables
(defvar test1 1)
(defvar test2 (i64+ test1 1))
(defvar test3 (i64+ test1 test2))
(defvar test4 (i64+ test3 test2))
(defvar test5 (i64+ test4 test3))
(defvar test6 (i64+ test5 test4))
(defvar test7 (i64+ test6 test5))
(defvar test8 (i64+ test7 test6))

(i64+ test8 test8)
(setf test8 100) ; Setting a variable
(i64+ test8 test7)

;; Shallow comparison
(defvar a 1)
(defvar b 2)
(eq a b)
(eq b b)


;; Types
(type f64) ; returns the type of f64 (double).
(type (ptr f64)) ; a pointer to an f64. (double *)

(type (fcn (ptr f64))) ;a function that returns a pointer t an f64 and takes no args.

(type (fcn f64 (a f64) (b f64))) ; a function that returns a f64 and takes two f64s. 
;; Types can be compared
(write_line "Should be true..")
(eq (type (ptr _vec2)) (type (ptr _vec2)))
(write_line "Should be false..")
(eq (type (ptr _vec2)) (type vec2))

;; Casting variables to different type
(cast "asd" (ptr i32))
(cast 0.001 f32) ; Currently the only valid way of creating a float
(cast 2.5 f64)

;; everything is symbol based
(quote hello) ; creats a symbol named 'hello'
(get-symbol "hello") ; creates a symbol from a string.

;; files can be loaded and executed
(load "test2.lisp")

(var ((a (expr "?/hello??")))
     (print-expr (expr (write_line (unexpr a)))))
(write_line "done")

(defcmacro one_expr (expr1) expr1)
(expand one_expr (write_line "??"))
(defcmacro two-expr (expr1 expr2)
  expr1)
(expr 1)
(expr (write_line "???"))
(defcmacro no-expr ()
   (expr (progn (write_line "???") (write_line "!!!")))) 
(expand no-expr)
(expand no-expr)



;; a macro is really just a function that takes exprs and compiles them to code.
;; unexpr is evaluated at 
(defcmacro fun-expr (expr1 expr2)
  (expr (progn
	  (write_line (unexpr expr1))
	  (unexpr expr2))))

(expand fun-expr "hello" (write_line "WORLD!"))
(write_line "gets here")
(if (eq 2 1) 2 3)

(defvar a 0)

(write_line "gets here")
(while (not (eq a 10))
  (progn 
    (setf a (+ a 1))
    (if (eq a 5)
	(progn (write_line "aaa") 1)
	(progn (write_line "bbb") 2))
    a))

(deref "asd")

(defvar teststr "asdaasd")
(defvar testarray (alloc 10))
(memcpy testarray (cast teststr (ptr void)) 8)
(write_line (cast testarray (ptr char)))

(defvar add-test (cast null (ptr i64)))
(defvar add-test-cnt (cast 0 u64))
(defvar to-add (cast 15 i64))
(add-to-list (cast (addrof add-test) (ptr (ptr void)))
	     (addrof add-test-cnt)
	     (cast (addrof to-add) (ptr void))
	     (size-of (type i64)))
(setf to-add 20)
(add-to-list (cast (addrof add-test) (ptr (ptr void)))
	     (addrof add-test-cnt)
	     (cast (addrof to-add) (ptr void))
	     (size-of (type i64)))
(setf to-add 30)
(add-to-list (cast (addrof add-test) (ptr (ptr void)))
	     (addrof add-test-cnt)
	     (cast (addrof to-add) (ptr void))
	     (size-of (type i64)))
(write_line "new deref:")
(deref add-test)
(deref (ptr+ add-test 1))
(deref (ptr+ add-test 2))
add-test-cnt
(defcmacro show-type (exp)
  (progn
    (print-type (type-of exp))
    (expr (write_line "..."))))

(show-type (addrof to-add))

(add-to-list+ add-test add-test-cnt to-add)
(add-to-list+ add-test add-test-cnt to-add)
(add-to-list+ add-test add-test-cnt to-add)
(add-to-list+ add-test add-test-cnt to-add)
(add-to-list+ add-test add-test-cnt to-add)
(add-to-list+ add-test add-test-cnt to-add)
(write_line "asd")
(printf "test: %i\n" (cast add-test-cnt i64))

(defcmacro comment (_expr)
  (expr (write_line "lol..")))

(write_line "the following works if libglfw is installed")

(load "glfw.lisp")
(load "gl.lisp")
(glfw:init)

(defvar win (glfw:create-window 512 512 "test.." null null))

(glfw:make-current win)
(glfw:set-clipboard-string win "clipboard test!")
(defvar sleeptime (cast 30000 i32))
(defvar r 0.0)
;;; -- Loa Shader Program -- ;;;
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
(defun key-callback (void (win-ptr (ptr void)) (key i32) (scancode i32) (action i32) (mods i32))
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

(defvar a 0)
(while (not (eq a 1000))
  (progn
    (setf a (+ a 1))
    (dealloc (realloc (alloc 1000) 2000))
    10
    ))


(while (not (eq iteration 4000))
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


