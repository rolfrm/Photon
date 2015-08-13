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
(load-symbol+ libtt tt:get-font-offset-for-index stbtt_GetFontOffsetForIndex (fcn i32 (data (ptr char)) (offset i32)))
(load-symbol+ libtt tt:get-codepoint-bitmap stbtt_GetCodepointBitmap (fcn (ptr char) (info (ptr tt:fontinfo)) (scale-x f32) (scale-y f32) (codepoint i32) (width (ptr i32)) (height (ptr i32)) (xoff (ptr i32)) (yoff (ptr i32)))) 

;float stbtt_ScaleForPixelHeight(const stbtt_fontinfo *info, float pixels);
(load-symbol+ libtt tt:scale-for-pixel-height stbtt_ScaleForPixelHeight 
	      (fcn f32 (info (ptr tt:fontinfo)) (pixels f32)))
;void stbtt_GetFontVMetrics(const stbtt_fontinfo *info, int *ascent, int *descent, int *lineGap);
(load-symbol+ libtt tt:get-font-v-metrics stbtt_GetFontVMetrics
	      (fcn void (info (ptr tt:fontinfo)) (ascent (ptr i32)) (descend (ptr i32)) (line-gap (ptr i32))))
;void stbtt_GetCodepointHMetrics(const stbtt_fontinfo *info, int codepoint, int *advanceWidth, int *leftSideBearing);
(load-symbol+ libtt tt:get-codepoint-h-metrics stbtt_GetCodepointHMetrics 
	      (fcn void (info (ptr tt:fontinfo)) (codepoint i32)
		   (advance-width (ptr i32)) (left-side-bearing (ptr i32))))

;stbtt_GetCodepointBitmapBoxSubpixel
;void stbtt_GetCodepointBitmapBoxSubpixel(const stbtt_fontinfo *font, int codepoint, float scale_x, float scale_y, float shift_x, float shift_y, int *ix0, int *iy0, int *ix1, int *iy1)
(load-symbol+ libtt tt:get-codepoint-bitmap-box-subpixel stbtt_GetCodepointBitmapBoxSubpixel
	      (fcn void (info (ptr tt:fontinfo)) (codepoint i32) (scale-x f32) (scale-y f32)
		   (shift-x f32) (shift-y f32) (ix0 (ptr i32)) (iy0 (ptr i32)) 
		   (ix1 (ptr i32)) (iy1 (ptr i32))))

;STBTT_DEF void stbtt_MakeCodepointBitmapSubpixel(const stbtt_fontinfo *info, unsigned char *output, int out_w, int out_h, int out_stride, float scale_x, float scale_y, float shift_x, float shift_y, int codepoint);
(load-symbol+ libtt tt:make-codepoint-bitmap-subpixel stbtt_MakeCodepointBitmapSubpixel
	      (fcn void (info (ptr tt:fontinfo)) (output (ptr char)) (out-w i32) (out-h i32)
		   (out-stride i32) (scale-x f32) (scale-y f32) (shift-x f32) (shift-y f32) (codepoint i32)))
			      
;stbtt_MakeCodepointBitmapSubpixel
;stbtt_GetCodepointKernAdvance

(defun read-utf8-codepoint ((ptr char) (string (ptr char)) (out-pt (ptr i32)))
;; eeh lets see..
;; UTF8 char str to codepoint conversion.
;; 0xxxxxxx 6                               0-127
;; 110xxxxx 10xxxxxx 11                     (127 + 64) - (127 + 64 + 16)
;; 1110xxxx 10xxxxxx 10xxxxxx 16
;; 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx 21
;; 111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 26
;; 1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 31
;; 11111110 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 36 ;;this probably does not exist.
  (let ((ustr (cast string (ptr u8))))
    (let ((first (deref ustr)) 
	  (nchars 0))
      (if (< first 0b10000000)
	  (progn
	    (setf nchars 0)
	    (noop))
	  (if (< first 0b11000000)
	      (noop) ; error: read started in middle of codepoint
	      (if (< first 0b11100000)
		  (progn
		    (setf nchars 1)
		    (setf first (.- first (cast 0b11000000 u8))))
		  (if (< first 0b11110000)
		      (progn
			(setf nchars 2)
			(setf first (.- first 0b11100000)))
		      (if (< first 0b11111000)
			  (progn
			    (setf nchars 3)
			    (setf first (.- first 0b11110000)))
			  (if (< first 0b11111100)
			      (progn
				(setf nchars 4)
				(setf first (.- first 0b11111000)))
			      (progn
				(setf nchars 5)
				(setf first (.- first 0b11111100)))
			      ))))))

      ;(print "nchars: " nchars newline)
       (if (eq nchars 0)
	   (progn
	     (setf (deref out-pt) (cast first i32))
	     (noop))
	   (let ((out (cast first i32)))
	     (setf out (cast (<< (cast out i64) (cast (* nchars 6) i64))
			     i32))
	     (print "out:" out newline)
	     (range it 1 (.+ nchars 1)
       		    (let ((chr (cast (deref (ptr+ ustr it)) i64)))
		      (setf out
			    (.+ out 
				(cast 
				 (<< (cast (bit-and chr 0b00111111)  i64)
				     (cast (.* (- nchars it 1) 6) i64))
				 i32)
				)))
		    (print "out:" out newline)
		    )
	     
      	    (setf (deref out-pt) out)))1
      (+ string (+ nchars 1)))))

(defstruct size
  (width i64)
  (height i64))

(defun print-size(void (s size))
  (print "(size width: " (member s width) " height: " (member s
							      height)
	 ")" newline))
(overload print print-size)
(defun size-area (i64 (s size))
  (* (member s width) (member s height)))

(defstruct tt:iterate-data
  (x0 i32)
  (x1 i32)
  (y0 i32)
  (y1 i32)
  (advance i32)
  (codepoint i32)
  (x-shift f32)
  (scale f32)
  (xpos f32)
  (baseline i32))

(defun print-tt:iterate-data (void (it-data tt:iterate-data))
  ;; Print display information about iterate-data.
  (progn
    (print "tt x0:" (member it-data x0) " x1:" (member it-data x1) " y0:" (member it-data y0) " y1:" (member it-data y1))
    (print " pt:" (member it-data codepoint) " x-sh:" (member it-data x-shift) " s:" (member it-data scale))
    (print " xpos:" (member it-data xpos) " baseline:" (member it-data baseline) " ")
  ))
(overload print print-tt:iterate-data)

(defun tt:iterate (void (text (ptr char))
		   (font-size i32) (max-width i32) (font (ptr tt:fontinfo))
		   (callback (fcn void (userdata (ptr void)) (status tt:iterate-data)))
		   (userdata (ptr void))) 
  (let ((len (cast (strlen text) i64))
	(ascent :type i32)
	(status :type tt:iterate-data))
    (memset (cast (addrof status) (ptr void)) 0 (size-of (type tt:iterate-data)))
    (setf (member status xpos) 2.0)
    (setf (member status scale) (tt:scale-for-pixel-height font (cast font-size f32)))
    (tt:get-font-v-metrics font (addrof ascent) (cast 0 (ptr i32)) (cast 0 (ptr i32)))
    (setf (member status baseline) (cast (* (cast ascent f32) (member status scale)) i32))
    (setf (member status advance) 0)
    (while! (not (eq (cast (deref text) i32) 0))
	   (let ((lsb :type i32))
	     (if! (eq (deref text) (deref newline))
		  (progn
		    (incr (member status baseline) (cast (* (cast ascent f32) (member status scale)) i32))
		    (setf (member status xpos) 2)
		    (setf text (+ text 1))
		    )
		  (progn
		    (setf text (read-utf8-codepoint text (addrof (member status codepoint))))
	       
		    (tt:get-codepoint-h-metrics font 
						(member status codepoint) 
						(addrof (member status advance)) 
						(addrof lsb))
		    (tt:get-codepoint-bitmap-box-subpixel font (member status codepoint)
							  (member status scale)
							  (member status scale) 
							  (member status x-shift) 
							  0 
							  (addrof (member status x0))
							  (addrof (member status y0))
							  (addrof (member status x1))
							  (addrof (member status y1)))
		    
		    
		    (when (>  (cast (+ (member status xpos) 
				       (* (member status scale) 
					  (cast (member status advance) f32))) 
				    i32)
			      max-width)
		      (setf (member status xpos) 0)
		      (incr (member status baseline) (cast (* (cast ascent f32) (member status scale)) i32))
		      (tt:get-codepoint-bitmap-box-subpixel font (member status codepoint)
							    (member status scale)
							    (member status scale) 
							    (member status x-shift) 
							    0 
							    (addrof (member status x0))
							    (addrof (member status y0))
							    (addrof (member status x1))
							    (addrof (member status y1)))
		      
		      )
		    (callback userdata status)
		    (setf (member status xpos) 
			  (+ (member status xpos) (* (member status scale) (cast (member status advance) f32))))
		    ))))))

(defun rect-calc-callback (void (userdata (ptr void)) (status tt:iterate-data))
  (var! ((rect (cast userdata (ptr size)))
	(right (cast ;(ceilf 
		(+  (cast  (member status x1) f32) 
		    (member status xpos)
		    ) i64))
	 (bottom (cast (+ (member status y1) (member status baseline)) i64)))
	(progn
	  (setf (member (deref rect) width) (max (member (deref rect) width) right))
	  (setf (member (deref rect) height) (max (member (deref rect) height) bottom)))
	))

(defstruct rect-draw-item
  (buffer (ptr char))
  (s size)
  (font (ptr tt:fontinfo)))

(defun rect-draw-callback (void (draw-user-data (ptr void)) (status tt:iterate-data))
  (let ((draw-item (deref (cast draw-user-data (ptr rect-draw-item)))))
    (let ((s (member draw-item s)))
      (let ((buffer-pt 
	     (ptr+ (member draw-item buffer)
		   (cast (+ (cast (member status xpos) i32) 
			    (member status x0)
			    (* (+ (member status y0) (member status baseline)) (cast (member s width) i32))
			    ) i64))))
	(tt:make-codepoint-bitmap-subpixel (member draw-item font) 
					   buffer-pt 
					   (- (member status x1) (member status x0)) 
					   (- (member status y1) (member status y0))
					   (cast (member s width) i32)
					   (member status scale)
					   (member status scale)
					   (member status x-shift)
					   0
					   (member status codepoint))))))

(defun get-level-conv(char (code char))
  (let ((code2 (>> (cast (cast code u8) i64) 5))) ;1 2 4 8 16 32 64 128 256
    (deref (ptr+ " .:ioVM@" code2))))
    

 (defun test
     (let ((baked (cast (alloc0 100) (ptr tt:fontinfo)))
	   (fontpath "/usr/share/fonts/truetype/freefont/FreeMono.ttf")
	   (s (cast 0 u64))
	   (str "Antialized text
In Truetype 
format:


'abcdef
ghijklm
opqrstu
vxyz'.

[ ... ]"))
       (let ((data (read-all-data fontpath (addrof s))))
	(if (ptr-null? data)
	    (print "Error!" newline)
	    (let ((xpos (cast 0.0 i32)) (ypos (cast 0.0 i32))
		  (width (cast 0 i32)) (height (cast 0 i32)))
	      (tt:init-font baked data (tt:get-font-offset-for-index data 0))
	      (let ((h (tt:scale-for-pixel-height baked 15))
		    (codept (cast 0xb5 i32)))
		(print newline "codepoint:" codept newline)
		(let ((s2 :type size))
		  (setf (member s2 width) 0)
		  (setf (member s2 height) 0)
		  (tt:iterate str 16 150 baked rect-calc-callback (cast (addrof s2) (ptr void)))
		  (print s2 newline)
		  (print s newline)
		  (let ((item :type rect-draw-item))
		    (setf (member item buffer) (cast (alloc0 (cast (size-area s2) u64)) (ptr char)))
		    (setf (member item font) baked)
		    (setf (member item s) s2)
		    (tt:iterate str 16 150 baked rect-draw-callback (cast (addrof item) (ptr void)))
		    (range row 0 (member s2 height)
			   (range col 0 (member s2 width)
				  (let ((offset (+ (* row (member s2 width)) col)))
				    (let ((v (deref (ptr+ (member item buffer) offset))))
				      (printchar (get-level-conv v))
				      )))
			   (printstr newline))
		  
		  ))))))))

(defun defer-test (void (a (fcn void (userdata (ptr void)))) (userdata (ptr void)))
  (a userdata))

(defun printhi (void (a (ptr void)))
    (print "Hi, im davey! " (cast a (ptr char)) newline))
;(defer-test printhi (cast (+ " hello ddd-dd-davey? " 2) (ptr void)))
;(test)
;(exit 0)
