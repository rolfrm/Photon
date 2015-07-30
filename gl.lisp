(defvar libgl (load-lib "libGL.so"))
(defmacro gl-load (name cname type)
  (expr (load-symbol+ libgl (unexpr name) (unexpr cname) (unexpr type))))

(gl-load gl:clear glClear  (fcn void (mask i32)))
(gl-load gl:clear-color  glClearColor (fcn void (r f32) (g f32) (b f32) (a f32)))
(write-line "create-shader")
(defvar gl:fragment-shader (cast 0x8b30 u32))
(defvar gl:vertex-shader (cast 0x8b31 u32))
(defvar gl:color-buffer-bit (cast 0x4000 i32))
;; gl shader
(gl-load gl:get-error glGetError (fcn u32))

(gl-load gl:create-shader glCreateShader  (fcn u32 (type u32)))
(gl-load gl:shader-source glShaderSource 
	      (fcn void 
		   (shader u32) 
		   (count u32) 
		   (shader-string (ptr (ptr char)))
		   (length (ptr u32))))
(gl-load gl:create-program glCreateProgram (fcn u32))
(gl-load gl:compile-shader glCompileShader (fcn void (shader u32)))
(gl-load gl:get-shader-info-log glGetShaderInfoLog 
	      (fcn void (shader u32) (maxlength u32) (length (ptr u32)) (buffer (ptr char))))
(gl-load gl:attach-shader glAttachShader (fcn void (program u32) (shader u32)))
(gl-load gl:get-shader-info glGetShaderiv (fcn void (shader u32) (pname u32) (params (ptr u32))))
(gl-load gl:link-program glLinkProgram (fcn void (program u32)))
(gl-load gl:get-program-info glGetProgramiv (fcn void (program u32) (enum u32) (params (ptr u32))))
(gl-load gl:use-program glUseProgram (fcn void (program u32)))
(gl-load gl:bind-attrib-location glBindAttribLocation (fcn void (program u32) (index u32) (name (ptr char))))
(gl-load gl:draw-arrays glDrawArrays (fcn void (mode u32) (first u32) (count u32)))

;; gl vbo
(gl-load gl:gen-buffers glGenBuffers (fcn void (count u32) (buffer-ptr (ptr u32))))
(gl-load gl:bind-buffer glBindBuffer (fcn void (type u32) (buffer u32)))
(gl-load gl:buffer-data glBufferData (fcn void (type u32) (byte-size u32) (data (ptr void)) (mode u32)))
(gl-load gl:vertex-attrib-pointer glVertexAttribPointer 
	 (fcn void (index u32) (size u32) (type u32)
	      (normalized u32) (stride u32) (ptr (ptr void))))
(gl-load gl:enable-vertex-attrib-array glEnableVertexAttribArray
	 (fcn void (index u32)))
(gl-load gl:disable-vertex-attrib-array glDisableVertexAttribArray
	 (fcn void (index u32)))							     

;; GL Uniform
(gl-load gl:get-uniform-location glGetUniformLocation (fcn i32 (program u32) (name (ptr char))))
(gl-load gl:uniform-1f glUniform1f (fcn void (location i32) (v1 f32)));
(gl-load gl:uniform-2f glUniform2f (fcn void (location i32) (v1 f32) (v2 f32)));
(gl-load gl:uniform-3f glUniform3f (fcn void (location i32) (v1 f32) (v2 f32) (v3 f32)));
(gl-load gl:uniform-4f glUniform4f (fcn void (location i32) (v1 f32) (v2 f32) (v3 f32) (v3 f32)));

(gl-load gl:uniform-1i glUniform1i (fcn void (location i32) (v1 i32)));
(gl-load gl:uniform-2i glUniform2i (fcn void (location i32) (v1 i32) (v2 i32)));
(gl-load gl:uniform-3i glUniform3i (fcn void (location i32) (v1 i32) (v2 i32) (v3 i32)));
(gl-load gl:uniform-4i glUniform4i (fcn void (location i32) (v1 i32) (v2 i32) (v3 i32) (v3 i32)));


(defoverloaded gl:uniform)
(overload gl:uniform gl:uniform-1f)
(overload gl:uniform gl:uniform-2f)
(overload gl:uniform gl:uniform-3f)
(overload gl:uniform gl:uniform-4f)

(overload gl:uniform gl:uniform-1i)
(overload gl:uniform gl:uniform-2i)
(overload gl:uniform gl:uniform-3i)
(overload gl:uniform gl:uniform-4i)

;; defines
;;GL_SHADER_TYPE, GL_DELETE_STATUS, GL_COMPILE_STATUS, GL_INFO_LOG_LENGTH, GL_SHADER_SOURCE_LENGTH.
(defmacro glvar (name value)
  (expr (defvar (unexpr name) (cast (unexpr value) u32))))
(defvar gl:shader-type (cast 0x8B4F u32))
(defvar gl:delete-status (cast 0x8B80 u32))
(defvar gl:compile-status (cast 0x8B81 u32))
(defvar gl:info-log-length (cast 0x8B84 u32))
(defvar gl:shader-source-length (cast 0x8B88 u32))

(defvar gl:link-status (cast 0x8B82 u32))

;(defvar gl:array-buffer (cast 0x8892 u32))
(glvar gl:array-buffer 0x8892)

(glvar gl:stream-draw 0x88E0)
(glvar gl:static-draw 0x88E4)
(glvar gl:dynamic-draw 0x88E8)

;; #define GL_STREAM_DRAW                    0x88E0
;; #define GL_STREAM_READ                    0x88E1
;; #define GL_STREAM_COPY                    0x88E2
;; #define GL_STATIC_DRAW                    0x88E4
;; #define GL_STATIC_READ                    0x88E5
;; #define GL_STATIC_COPY                    0x88E6
;; #define GL_DYNAMIC_DRAW                   0x88E8
;; #define GL_DYNAMIC_READ                   0x88E9
;; #define GL_DYNAMIC_COPY                   0x88EA

(defvar gl:true (cast 1 u32))
(defvar gl:false (cast 0 u32))


;; #define GL_BYTE					0x1400
;; #define GL_UNSIGNED_BYTE			0x1401
;; #define GL_SHORT				0x1402
;; #define GL_UNSIGNED_SHORT			0x1403
;; #define GL_INT					0x1404
;; #define GL_UNSIGNED_INT				0x1405
;; #define GL_FLOAT				0x1406
;; #define GL_2_BYTES				0x1407
;; #define GL_3_BYTES				0x1408
;; #define GL_4_BYTES				0x1409
;; #define GL_DOUBLE				0x140A
; -- gl types --
(glvar gl:byte 0x1400)
(glvar gl:ubyte 0x1401)
(glvar gl:short 0x1402)
(glvar gl:ushort 0x1403)
(glvar gl:int 0x1404)
(glvar gl:uint 0x1405)
(glvar gl:float 0x1406)
(glvar gl:2bytes 0x1407)
(glvar gl:3bytes 0x1408)
(glvar gl:4bytes 0x1409)
(glvar gl:double 0x140a)

;; /* Primitives */
;; #define GL_POINTS				0x0000
;; #define GL_LINES				0x0001
;; #define GL_LINE_LOOP				0x0002
;; #define GL_LINE_STRIP				0x0003
;; #define GL_TRIANGLES				0x0004
;; #define GL_TRIANGLE_STRIP			0x0005
;; #define GL_TRIANGLE_FAN				0x0006
;; #define GL_QUADS				0x0007
;; #define GL_QUAD_STRIP				0x0008
;; #define GL_POLYGON				0x0009

; -- gl primitives -- ;
(glvar gl:points 0)
(glvar gl:lines 1)
(glvar gl:line-loop 2)
(glvar gl:line-strip 3)
(glvar gl:triangles 4)
(glvar gl:triangle-strip 5)
(glvar gl:triangle-fan 6)
(glvar gl:quads 7)
(glvar gl:quad-strip 8)
(glvar gl:polygin 9)
