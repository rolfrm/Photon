(defstruct text-box
  (texture gl:tex)
  (bounds rect) 
  )

(defun load-font ((ptr tt:fontinfo) (path (ptr char)))
  (let ((s :type u64))
    (let ((baked (cast (alloc0 100) (ptr tt:fontinfo)))
	  (data (read-all-data path (addrof s))))
      (tt:init-font baked data (tt:get-font-offset-for-index data 0))
      baked)))

(defun text-box:create (text-box (str (ptr char)) (font-size i32) (font (ptr tt:fontinfo)))
  (let ((s :type size)
	(box :type text-box))
    (setf (member s width) 0)
    (setf (member s height) 0)
    (gl:gen-textures 1 (addrof (member box texture)))
    (gl:bind-texture gl:texture-2d (member box texture))
    (tt:iterate str font-size 200 font rect-calc-callback (cast (addrof s) (ptr void)))
    (let ((item :type rect-draw-item))
      (setf (member item buffer) (cast (alloc0 (cast (size-area s) u64)) (ptr char)))
      (setf (member item font) font)
      (setf (member item s) s)
      (tt:iterate str font-size 200 font rect-draw-callback (cast (addrof item) (ptr void)))
      (gl:tex-parameter gl:texture-2d gl:texture-min-filter gl:nearest)
      (gl:tex-parameter gl:texture-2d gl:texture-mag-filter gl:nearest)
      (gl:tex-parameter gl:texture-2d gl:texture-wrap-s gl:clamp-to-border)
      (gl:tex-parameter gl:texture-2d gl:texture-wrap-t gl:clamp-to-border)
      (gl:tex-image-2d gl:texture-2d 0 gl:rgb (member s width) (member s height) 0 gl:red gl:ubyte 
		       (cast (member item buffer) (ptr void)))
      (let ((pos :type rect))
	(zeroize pos)
	(setf (member (member pos size) x) (cast (member s width) f64))
	(setf (member (member pos size) y) (cast (member s height) f64))
	(setf (member box bounds) pos))
      (dealloc (cast (member item buffer) (ptr void)))
      )
    box
    ))

(defun text-box:delete (void (txtbox text-box))
  (gl:delete-textures 1 (addrof (member txtbox texture))))

(defvar text-box:shader :type gl:shader-program)
(defvar text-box:shader:offset :type gl:uniform-loc)
(defvar text-box:shader:size :type gl:uniform-loc)
(defvar text-box:shader:fg-color :type gl:uniform-loc)
(defvar text-box:shader:bg-color :type gl:uniform-loc)
(defvar text-box:loaded false)

(defun text-box:print-shader-errors (void (shader gl:shader))
  (let ((glstatus (cast 0 u32))
	(log (cast (alloc0 1000) (ptr char))))
    (gl:get-shader-info shader gl:compile-status (addrof glstatus))
    (when (eq glstatus gl:false)
      (let ((length :type u32))
	(gl:get-shader-info-log shader 1000 (addrof length) log)
	(print "**** Shader Info Log ****" newline log newline "********" newline)))))
    
(defun text-box:load
    (unless text-box:loaded
      (let (
	    (frag (gl:create-shader gl:fragment-shader))
	    (vert (gl:create-shader gl:vertex-shader))
	    (frag-src "
uniform vec4 bg_color;
uniform vec4 fg_color;
uniform sampler2D tex;
in vec2 uv;
void main(){
  float i = texture(tex, uv);
  gl_FragColor = bg_color * (1 - i) + fg_color * i;
}
")
	  (vert-src "
#version 130
uniform vec2 offset;
uniform vec2 size;
out vec2 uv;
void main(){
  vec2 id = vec2(0,0);
  if(gl_VertexID == 1){
     id = vec2(1,0);
  }else if (gl_VertexID == 2){
     id = vec2(1,1);
  }else if (gl_VertexID == 3){
     id = vec2(0,1);
  }
  uv = id;
  gl_Position = vec4(id * size + offset,0.0,1.0);
}
"))
	(setf text-box:loaded true)
	(setf text-box:shader (gl:create-program))
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
	
	(gl:attach-shader text-box:shader frag)
	(gl:attach-shader text-box:shader vert)
	(gl:link-program text-box:shader)
	(setf text-box:shader:offset
	      (gl:get-uniform-location text-box:shader "offset"))
	(setf text-box:shader:size
	      (gl:get-uniform-location text-box:shader "size"))
	(setf text-box:shader:bg-color
	      (gl:get-uniform-location text-box:shader "bg_color"))
	(setf text-box:shader:fg-color
	      (gl:get-uniform-location text-box:shader "fg_color"))
	
	)))
      
  
(defstruct color
  (r u8)
  (g u8)
  (b u8)
  (a u8))

(defun from-rgba (color (red u8) (green u8) (blue u8) (alpha u8))
  (let ((col :type color))
    (setf (member col r) red)
    (setf (member col g) green)
    (setf (member col b) blue)
    (setf (member col a) alpha)
    col))
      
(defun uniform-color (void (loc gl:uniform-loc) (col color))
  (gl:uniform-4f loc 
		 (/ (cast (member col r) f32) 255.0)
		 (/ (cast (member col g) f32) 255.0)
		 (/ (cast (member col b) f32) 255.0)
		 (/ (cast (member col a) f32) 255.0)))
(overload gl:uniform uniform-color)

(defun text-box:draw (void (image text-box) (fg-color color) (bg-color color))
  (progn
    (gl:use-program text-box:shader)
    (gl:bind-texture gl:texture-2d (member image texture))
    (gl:uniform text-box:shader:fg-color fg-color)
    (gl:uniform text-box:shader:bg-color bg-color)
    (let ((r (member image bounds)))
      (gl:uniform text-box:shader:offset (member r upper-left))
      (gl:uniform text-box:shader:size  (member r size)))
    (gl:draw-arrays gl:quads 0 4)
    ))
