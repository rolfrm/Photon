(defvar libtt (load-lib "./truetype.so"))
;stbtt_BakeFontBitmap(ttf_buffer,0, 32.0, temp_bitmap,512,512, 32,96, cdata); // no guarantee this fits!
;fread(ttf_buffer, 1, 1<<20, fopen("c:/windows/fonts/times.ttf", "rb"));
;stbtt_GetBakedQuad(cdata, 512,512, *text-32, &x,&y,&q,1);//1=opengl & d3d10+,0=d3d9
;bitmap = stbtt_GetCodepointBitmap(&font, 0,stbtt_ScaleForPixelHeight(&font, s), c, &w, &h, 0,0);

(defstruct tt:baked-char
  (x0 u16)
  (y0 u16)
  (x1 u16)
  (y1 u16)
  (xoff f32)
  (yoff f32)
  (xadvance f32))

(defstruct tt:aligned-quad
  (x0 f32) (y0 f32) (s0 f32) (t0 f32)
  (x1 f32) (y1 f32) (s1 f32) (t1 f32))


(load-symbol+ libtt tt:bake-font-bitmap stbtt_BakeFontBitmap (fcn i32 (data (ptr char)) (offset i32) (pixel_height f32) (pixels (ptr char)) (pw i32) (ph i32) (first-char i32) (num-chars i32) (chardata (ptr tt:baked-char))))


;stbtt_GetBakedQuad(stbtt_bakedchar *chardata, int pw, int ph, int char_index, float *xpos, float *ypos, stbtt_aligned_quad *q, int opengl_fillrule)

(load-symbol+ libtt tt:get-baked-quad stbtt_GetBakedQuad (fcn void (chardata (ptr tt:baked-char)) (pw i32) (ph i32) (char_index i32) (xpos (ptr f32)) (ypos (ptr f32)) (q (ptr tt:aligned-quad)) (opengl-fillrule i32)))

(type (alias (opaque-struct tt:_fontinfo) tt:fontinfo))

(load-symbol+ libtt tt:init-font stbtt_InitFont (fcn i32 (info (ptr tt:fontinfo)) (data (ptr char)) (offset i32))) 

(defun test(void)
  (let ((baked (cast (alloc0 100) (ptr tt:fontinfo))))
    (print "Testing ttf" newline)))
(test)
    
