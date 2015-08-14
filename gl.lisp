(defvar libgl (load-lib "libGL.so"))
(type (alias u32 gl:enum))

(defmacro gl-load (name cname type)
  (expr (load-symbol+ libgl (unexpr name) (unexpr cname) (unexpr type))))

(defmacro glvar (name value)
  (expr (defvar (unexpr name) (cast (unexpr value) gl:enum))))

;; misc
(gl-load gl:clear glClear  (fcn void (mask gl:enum)))
(gl-load gl:clear-color  glClearColor (fcn void (r f32) (g f32) (b f32) (a f32)))
(gl-load gl:line-width glLineWidth (fcn void (width f32)))
(gl-load gl:enable glEnable (fcn void (glenum gl:enum)))
(gl-load gl:disable glDisable (fcn void (glenum gl:enum)))
;; gl shader
(gl-load gl:get-error glGetError (fcn u32))

(gl-load gl:create-shader glCreateShader  (fcn u32 (type gl:enum)))
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
(gl-load gl:get-shader-info glGetShaderiv (fcn void (shader u32) (pname gl:enum) (params (ptr u32))))
(gl-load gl:link-program glLinkProgram (fcn void (program u32)))
(gl-load gl:get-program-info glGetProgramiv (fcn void (program u32) (enum gl:enum) (params (ptr u32))))
(gl-load gl:use-program glUseProgram (fcn void (program u32)))
(gl-load gl:bind-attrib-location glBindAttribLocation (fcn void (program u32) (index u32) (name (ptr char))))
(gl-load gl:draw-arrays glDrawArrays (fcn void (mode gl:enum) (first u32) (count u32)))

;; gl vbo
(gl-load gl:gen-buffers glGenBuffers (fcn void (count u32) (buffer-ptr (ptr u32))))
(gl-load gl:bind-buffer glBindBuffer (fcn void (type gl:enum) (buffer u32)))
(gl-load gl:buffer-data glBufferData (fcn void (type gl:enum) (byte-size u32) (data (ptr void)) (mode gl:enum)))
(gl-load gl:vertex-attrib-pointer glVertexAttribPointer 
	 (fcn void (index u32) (size u32) (type gl:enum)
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
(type (alias u32 gl:tex))
;void glGenTextures(GLsizei n,GLuint * textures);
(gl-load gl:gen-textures glGenTextures (fcn void (size i64) (textures (ptr gl:tex))))
;void glTexImage2D(GLenum target​, GLint level​, GLint internalFormat​, GLsizei width​, 
;                  GLsizei height​, GLint border​, GLenum format​, GLenum type​, const GLvoid * data​);
(gl-load gl:tex-image-2d glTexImage2D (fcn void (target gl:enum) (level i32) (internal-format gl:enum)
					    (width i64) (height i64) (border i32) (format gl:enum)
					    (type gl:enum) (data (ptr void))))

(gl-load gl:bind-textures glBindTextures (fcn void (first gl:enum) (count i64) (textures (ptr gl:tex))))
(gl-load gl:bind-textures glBindTexture (fcn void (target gl:enum) (texture gl:tex)))
(gl-load gl:tex-parameter glTexParameteri (fcn void (target gl:enum) (name gl:enum) (value gl:enum)))
(gl-load gl:active-texture glActiveTexture (fcn void (active gl:enum)))

(gl-load gl:blend-func glBlendFunc (fcn void (src gl:enum) (dst gl:enum)))

;GL_TEXTURE_MIN_FILTER,GL_LINEAR, GL_TEXTURE_MAG_FILTER, GL_TEXTURE_WRAP_S, GL_TEXTURE_WRAP_T,GL_CLAMP_TO_BORDER




;; #define GL_TEXTURE_MAG_FILTER			0x2800
;; #define GL_TEXTURE_MIN_FILTER			0x2801
;; #define GL_LINEAR				0x2601
;; #define GL_NEAREST				0x2600
;; #define GL_TEXTURE_WRAP_S			0x2802
;; #define GL_TEXTURE_WRAP_T			0x2803
;; #define GL_CLAMP_TO_BORDER			0x812D
(glvar gl:texture-mag-filter 0x2800)
(glvar gl:texture-min-filter 0x2801)
(glvar gl:nearest 0x2600)
(glvar gl:linear 0x2601)
(glvar gl:texture-wrap-s 0x2802)
(glvar gl:texture-wrap-t 0x2803)
(glvar gl:clamp-to-border 0x812d)

;; defines

(glvar gl:texture-1d 0x0DE0)
(glvar gl:texture-2d 0x0DE1)
(glvar gl:depth-component 0x1902)
(glvar gl:red 0x1903)
(glvar gl:rgb 0x1907)
(glvar gl:rgba 0x1908)
(glvar gl:color-buffer-bit 0x4000)
(glvar gl:fragment-shader 0x8b30)
(glvar gl:vertex-shader 0x8b31)
(glvar gl:texture-0 0x84C0)
(glvar gl:shader-type 0x8B4F)
(glvar gl:delete-status 0x8B80)
(glvar gl:compile-status 0x8B81)
(glvar gl:info-log-length 0x8B84)
(glvar gl:shader-source-length 0x8B88)
(glvar gl:link-status 0x8B82)
(glvar gl:array-buffer 0x8892)
(glvar gl:stream-draw 0x88E0)
(glvar gl:static-draw 0x88E4)
(glvar gl:dynamic-draw 0x88E8)
(glvar gl:blend 0xbe2)
;; #define GL_BLEND				0x0BE2
;; #define GL_BLEND_SRC				0x0BE1
;; #define GL_BLEND_DST				0x0BE0
;; #define GL_ZERO					0
;; #define GL_ONE					1
;; #define GL_SRC_COLOR				0x0300
;; #define GL_ONE_MINUS_SRC_COLOR			0x0301
;; #define GL_SRC_ALPHA				0x0302
;; #define GL_ONE_MINUS_SRC_ALPHA			0x0303
;; #define GL_DST_ALPHA				0x0304
;; #define GL_ONE_MINUS_DST_ALPHA			0x0305
;; #define GL_DST_COLOR				0x0306
;; #define GL_ONE_MINUS_DST_COLOR			0x0307
;; #define GL_SRC_ALPHA_SATURATE			0x0308

(glvar gl:src-alpha 0x0302)
(glvar gl:one-minus-src-alpha 0x0303)
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
(glvar gl:polygon 9)

(glvar gl:depth-test 0x0B71)
