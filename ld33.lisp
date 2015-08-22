(load "std2.lisp")
(load "truetype.lisp")
(load "stb_image.lisp")
(load "glfw.lisp")


(glfw:init)
(defvar win (glfw:create-window 800 800 "Bloddy!" null null))
(glfw:make-current win)
(defstruct rect
  (upper-left vec2)
  (size vec2))
(load "gl.lisp")
(load "gl-ext.lisp")
(load "textbox-shader.lisp")

(defstruct point
  (x i64)
  (y i64))

(defun make-point (point (x i64) (y i64))
  (let ((pt :type point))
    (setf (member pt x) x)
    (setf (member pt y) y)
    pt))

(defun print-point (void (pt point))
  (print "(P: "(member pt x) " " (member pt y) ")"))

(overload print print-point)

(defun point-to-vec2 (vec2 (pt point))
  (vec (cast (member pt x) f64)
       (cast (member pt y) f64)))

(defun vec2-to-point( point (a vec2))
  (make-point (cast (member a x) i64) 
	      (cast (member a y) i64)))



(defun analyze-image (void (im im:image))
  (let ((buf (cast (alloc0 (* 256 4)) (ptr i32)))
	(data (member im data)))
    (let ((size (cast (* (member im width) (member im height)) i64)))
      (range it 0 size
	     (incr (deref (+ buf (cast (deref (+ data it)) i64))) 1))
      (range it 0 256
	     (let ((bv (deref (+ buf it))))
	       (when (not (eq bv 0))
		 (print it ":" bv newline))))
      (dealloc (cast buf (ptr void))))))


(defvar lv (im:load-image "level1.png"))
(defvar lv2 (im:load-image "level1.png"))
(print "Image: " (member lv width) " " (member lv height) " " (member lv bpp) newline)
(analyze-image lv)
(print newline)

(defun find-levels (void (level u8) (im im:image) (outpts (ptr (ptr point))) (outpts-cnt (ptr i64)))
  (let ((width (cast (member im width) i64))
	(height (cast (member im height) i64))
	(outpts2 (cast null (ptr point)))
	(outpts2-cnt 0))
    (setf (deref outpts-cnt) 0)
    (range row 0 height
	   (range col 0 width
		  (let ((index (+ col (* row width))))
		    (let ((v (deref (+ (member im data) index))))
		      (when (eq v level)
			(add-to-list+ outpts2 outpts2-cnt (make-point col row)))))))
    (setf (deref outpts) outpts2)
    (setf (deref outpts-cnt) outpts2-cnt )))
 
(defmacro gameid (name value)
  (expr (defvar (unexpr name) (cast (unexpr value) u8))))

(gameid gameid:player 128)
(gameid gameid:player-next 120)
(gameid gameid:enemy 104)
(gameid gameid:nothing 0)
(gameid gameid:wall 255)

(defvar player-pos :type vec2)
(defvar player-next-pos :type point)
(defvar player-dir :type vec2)
(defvar enemy-pos (cast null (ptr point)))
(defvar lookup-pos (cast null (ptr point)))
(defvar cnt 0)
(find-levels gameid:player lv (addrof lookup-pos) (addrof cnt))
(assert (eq cnt 1))
(setf player-pos (point-to-vec2 (deref lookup-pos)))
(setf cnt 0)
(find-levels gameid:player-next lv (addrof lookup-pos) (addrof cnt))
(assert (eq cnt 1))
(setf player-next-pos (deref lookup-pos))
(print player-pos " " player-next-pos newline)
;(exit 0)
(setf player-dir (vec2-normalize (- (point-to-vec2  player-next-pos) player-pos )))
(print "dir: " player-dir newline)

(defun lookup ((ptr u8) (x i64) (y i64) (im im:image))
  (let ((w (cast (member im width) i64))
	(h (cast (member im height) i64)))
    (let ((index (+ (.% x w)
		    (* (.% y h) w))))
      (ptr+ (member im data) index))))

(defun lookup-point ((ptr u8) (pt point) (im im:image))
  (lookup (member pt x) (member pt y) im))

(defun get-flow-points (bool (pos point) (dir vec2) (lv im:image) 
			(lower (ptr point)) 
			(upper (ptr point)))
  (let ((tpt (point-to-vec2 pos))
	(t1 (vec2:rot90 dir))
	(found-lower false)
	(found-upper false))
    (range it -50 50
	   (let ((itf (cast it f64)))
	     (let ((pt (vec2-to-point (+ tpt (* t1 itf 0.5)))))
	       (let ((value (deref (lookup-point pt lv))))
		 (when (eq value gameid:wall)
		   (when (< it 0)
		     ;(setf it 0)
		     (setf (deref lower) pt)
		     (setf found-lower true)
		     )
		   (when (> it 0)
		     (setf (deref upper) pt)
		     (setf found-upper true)
		     (setf it 49)
		   
		     ))))))
    (and found-lower found-upper)))
		     
(defvar upper-pt :type point)
(defvar lower-pt :type point)
(defvar success (get-flow-points (vec2-to-point player-pos) player-dir lv 
				 (addrof upper-pt) (addrof lower-pt)))
(print success " " upper-pt " " lower-pt newline)

(defun pt-dst-sqrt (i64 (a point) (b point))
  (let ((x (- (member a x) (member b x)))
	(y (- (member a y) (member b y))))
    (+ (* x x) (* y y))))

(defun find-closer-point (point (center point) (base point) (lv im:image) (radius i64))
  (let ((out-pt base)
	(start-x (- (member base x) radius))
	(end-x (+ (member base x) radius 1))
	(start-y (- (member base y) radius))
	(end-y (+ (member base y) radius 1))
	(current-d (pt-dst-sqrt center base)))
	  
    (range y start-y end-y
	   (range x start-x end-x
		  (let ((value (deref (lookup x y lv))))
		    (when (eq value gameid:wall)
		      (let ((pt (make-point x y)))

			(let ((d (pt-dst-sqrt center pt )))

			  (when (< d current-d)
			    (setf out-pt pt)
			    (setf current-d d))))))))
    out-pt))
;; (setf upper-pt (find-closer-point player-pos upper-pt lv 3))
;; (print newline)
;; (setf lower-pt (find-closer-point player-pos lower-pt lv 3))
;; (print success " " upper-pt " " lower-pt newline)
;; (setf upper-pt (find-closer-point player-pos upper-pt lv 3))
;; (print newline)
;; (setf lower-pt (find-closer-point player-pos lower-pt lv 3))
;; (print success " " upper-pt " " lower-pt newline)
;; (setf upper-pt (find-closer-point player-pos upper-pt lv 3))
;; (print newline)
;; (setf lower-pt (find-closer-point player-pos lower-pt lv 3))
;; (print success " " upper-pt " " lower-pt newline)
;; (setf upper-pt (find-closer-point player-pos upper-pt lv 3))
;; (print newline)
;; (setf lower-pt (find-closer-point player-pos lower-pt lv 3))
;; (print success " " upper-pt " " lower-pt newline)

;; (defvar lower-vec (point-to-vec2 lower-pt))
;; (defvar upper-vec (point-to-vec2 upper-pt))
;; (defvar perp-vec (vec2-normalize (vec2:rot90 (- upper-vec lower-vec))))
;; (print perp-vec " " player-dir newline)

(defun game-it
    (let ((upper-pt :type point)
	  (lower-pt :type point)
	  (ppos (vec2-to-point player-pos)))
      (let ((success (get-flow-points ppos player-dir lv 
				      (addrof lower-pt) (addrof upper-pt))))
	(when success
	  (setf upper-pt (find-closer-point ppos upper-pt lv 1))
	  (setf lower-pt (find-closer-point ppos lower-pt lv 1))
	  (setf (deref (lookup-point upper-pt lv2)) 200)
	  (setf (deref (lookup-point lower-pt lv2)) 200)
	  (let ((lower-vec (point-to-vec2 lower-pt))
		(upper-vec (point-to-vec2 upper-pt)))
	    (let ((perp-vec (vec2-normalize (vec2:rot90 (- lower-vec upper-vec )))))
	      (when (> (vec2-length perp-vec) 0)
		(setf player-dir perp-vec)))
	  )))
      (incr player-pos (* player-dir 0.2))
      ))


(defvar fs-blit:loaded false)
(defvar fs-blit:shader :type gl:shader-program)
(defvar fs-blit:shader:tex :type gl:uniform-loc)
(defun fs-blit:load
    (unless fs-blit:loaded
      (let (
	    (frag (gl:create-shader gl:fragment-shader))
	    (vert (gl:create-shader gl:vertex-shader))
	    (frag-src "
#version 130
uniform sampler2D tex;
in vec2 uv;
void main(){
  gl_FragColor = texture(tex, vec2(uv.x, 1.0 - uv.y));
}
")
	  (vert-src "
#version 130
out vec2 uv;
void main(){
  vec2 id = vec2(-1,-1);
  if(gl_VertexID == 1){
     id = vec2(1,-1);
  }else if (gl_VertexID == 2){
     id = vec2(1,1);
  }else if (gl_VertexID == 3){
     id = vec2(-1,1);
  }
  uv = id * 0.5 + vec2(0.5, 0.5);
  gl_Position = vec4(id,0.0,1.0);
}
"))
	(setf fs-blit:loaded true)
	(setf fs-blit:shader (gl:create-program))
	(let ((frag-src-len (cast (strlen frag-src) u32)))
	  (gl:shader-source frag 1 (addrof frag-src) (addrof frag-src-len)))
	(let ((vert-src-len (cast (strlen vert-src) u32)))
	  (gl:shader-source vert 1 (addrof vert-src) (addrof vert-src-len)))
	(gl:compile-shader frag)
	(gl:compile-shader vert)
	
	(print "**** frag ****" newline)
	(text-box:print-shader-errors frag)
	(print "**** vert ****" newline)
	(text-box:print-shader-errors vert)
	
	(gl:attach-shader fs-blit:shader frag)
	(gl:attach-shader fs-blit:shader vert)
	(gl:link-program fs-blit:shader)
	(setf fs-blit:shader:tex
	      (gl:get-uniform-location text-box:shader "tex"))
	
	)))
(fs-blit:load)
(defun fs-blit:load-image (gl:tex (image im:image))
  (let ((tex :type gl:tex))
    (gl:gen-textures 1 (addrof tex))
    (gl:bind-texture gl:texture-2d tex)
    (gl:tex-parameter gl:texture-2d gl:texture-min-filter gl:nearest)
    (gl:tex-parameter gl:texture-2d gl:texture-mag-filter gl:nearest)
    (gl:tex-parameter gl:texture-2d gl:texture-wrap-s gl:clamp-to-border)
    (gl:tex-parameter gl:texture-2d gl:texture-wrap-t gl:clamp-to-border)
    (gl:tex-image-2d gl:texture-2d 0 gl:rgb 
		     (cast (member image width) i64)
		     (cast (member image height) i64)
		     0 gl:red gl:ubyte 
		     (cast (member image data) (ptr void)))
    tex))

(defun fs-blit:reload-image (void (tex gl:tex) (image im:image))
  (progn


    (gl:bind-texture gl:texture-2d tex)
    (gl:tex-image-2d gl:texture-2d 0 gl:rgb 
		     (cast (member image width) i64) 
		     (cast (member image height) i64)
		     0 gl:red gl:ubyte 
		     (cast (member image data) (ptr void)))))

(defun fs-blit:render-image (void (tex gl:tex))
  (progn
    (gl:use-program fs-blit:shader)
    (gl:bind-texture gl:texture-2d tex)
    (gl:draw-arrays gl:quads 0 4)))
    
(defvar game-tex (fs-blit:load-image lv2))
(defun run-game
    (let ((iteration 0))
      (while (< iteration 1000)
	(setf (deref (lookup-point (vec2-to-point player-pos) lv2)) 255)
	(fs-blit:reload-image game-tex lv2)

	(game-it)
	(let ((pc 
	       (if (glfw:get-key win glfw:key-a)
		   1.0
		   (if (glfw:get-key win glfw:key-d)
		       -1.0
		       0.0))))
	  (setf player-pos (+ (* (vec2:rot90 player-dir) pc 0.1) player-pos)))
	(fs-blit:render-image game-tex)
	(glfw:swap-buffers win)
	(glfw:poll-events)
	(usleep 10000))))
(run-game)


;(range it 0 cnt
;       (print "player at:" (deref (+ player-pos it)) newline))




;(find-levels 120 (addrof player-pos) (addrof cnt))
;(defvar player-pos 

;(defun load-level (void (path (ptr char)))
  
