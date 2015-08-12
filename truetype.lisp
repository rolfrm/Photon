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

      (print "nchars: " nchars newline)
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
(let ((s :type size))
  (setf (member s width) 10)
  (setf (member s height) (+ (cast 1 i64) 1))
  (print s))
;(exit 0)
(defun get-text-size (size (text (ptr char)) (font-size i32) (max-width i32) (font (ptr tt:fontinfo)))
  (let ((len (cast (strlen text) i64))
	(ascent :type i32)
	(xpos (the 2.0 f32))
	(max-width-reached (the 0 i64))
	(scale (tt:scale-for-pixel-height font (cast font-size f32))))
    (tt:get-font-v-metrics font (addrof ascent) (cast 0 (ptr i32)) (cast 0 (ptr i32)))
    (let ((baseline (* ascent scale)))
       (range it 0 len ;x0,y0,x1,y1
      	     (let ((advance :type i32)
      		   (lsb :type i32)
      		   (x0 :type i32)
      		   (x1 :type i32)
      		   (y0 :type i32)
      		   (y1 :type i32)
      		   (x-shift (the 0.0 f32))
		   (codepoint (cast (deref (+ text it)) i32)))
	       
	       (tt:get-codepoint-h-metrics font codepoint (addrof advance) (addrof lsb))
	       (tt:get-codepoint-bitmap-box-subpixel font codepoint 
						     scale scale x-shift 0 (addrof x0) (addrof y0)
						     (addrof x1)
						     (addrof y1))
	       (when (> (cast (+ xpos (* scale (cast advance f32))) i32) max-width)
		 (setf xpos 0)
		 (incr baseline (cast (* (cast advance f32) scale) i32))
	       )
       	     (setf xpos (+ xpos (* scale (cast advance f32))))
       	     (setf max-width-reached (max max-width-reached (cast xpos i64)))
	     
	       ))
      ;(print (+ baseline 1))
      (let ((s :type size))
	(setf (member s width) (+ max-width-reached 10))
	(setf (member s height) (cast (+ baseline 10) i64))
	s))))

(defun size-area (i64 (s size))
  (* (member s width) (member s height)))

(defun write-text-to-buffer ((ptr char) (s size) (text (ptr char))
			     (font-size i32) (max-width i32) (font (ptr tt:fontinfo)))
  (let ((len (cast (strlen text) i64))
	(ascent :type i32)
	(xpos (the 2.0 f32))
	(buffer (cast (alloc0 (cast (size-area s) u64)) (ptr char)))
	(scale (tt:scale-for-pixel-height font (cast font-size f32))))
    (tt:get-font-v-metrics font (addrof ascent) (cast 0 (ptr i32)) (cast 0 (ptr i32)))
    (let ((baseline (* ascent scale)))
       (range it 0 len ;x0,y0,x1,y1
      	     (let ((advance :type i32)
      		   (lsb :type i32)
      		   (x0 :type i32)
      		   (x1 :type i32)
      		   (y0 :type i32)
      		   (y1 :type i32)
      		   (x-shift (the 0.0 f32))
		   (codepoint (cast (deref (+ text it)) i32))		   
		   )
	       
	       (tt:get-codepoint-h-metrics font codepoint (addrof advance) (addrof lsb))
	       (tt:get-codepoint-bitmap-box-subpixel font codepoint 
						     scale scale x-shift 0 (addrof x0) (addrof y0)
						     (addrof x1)
						     (addrof y1))
	       (let ((buffer-pt (ptr+ buffer (cast (+ (cast xpos i32) x0
						      (* (+ y0 baseline) (cast (member s width) i32))
						      ) i64))))
					;)))
		     
		 (tt:make-codepoint-bitmap-subpixel font buffer-pt (- x1 x0) (- y1 y0) (cast (member s width) i32)
						    scale scale x-shift 0 codepoint)
		 )
						      
						      
	       (when (> (cast (+ xpos (* scale (cast advance f32))) i32) max-width)
		 (setf xpos 0)
		 (incr baseline (cast (* (cast ascent f32) scale) i32))
	       )
       	     (setf xpos (+ xpos (* scale (cast advance f32))))
	       )))
    buffer))
	  
	     
;; (defun write-text ((ptr char) (out-size (ptr size)) (text (ptr char)))
;;   (let ((len (cast (strlen text) i64)))
;;     (range it 0 len ;x0,y0,x1,y1
;; 	   (let ((advance :type i32)
;; 		 (lsb :type i32)
;; 		 (x0 :type i32)
;; 		 (x1 :type i32)
;; 		 (y0 :type i32)
;; 		 (y1 :type i32))
		 
;;   "" )

 (defun test
     (let ((baked (cast (alloc0 100) (ptr tt:fontinfo)))
	   (fontpath "/usr/share/fonts/truetype/freefont/FreeMono.ttf")
	   (s (cast 0 u64)))
       (let ((data (read-all-data fontpath (addrof s))))
	(if (ptr-null? data)
	    (print "Error!" newline)
	    (let ((xpos (cast 0.0 i32)) (ypos (cast 0.0 i32))
		  (width (cast 0 i32)) (height (cast 0 i32)))
	      (tt:init-font baked data (tt:get-font-offset-for-index data 0))
	      (let ((h (tt:scale-for-pixel-height baked 15))
		    (codept (cast 0xb5 i32)))
		(print newline "codepoint:" codept newline)
		(let ((s (get-text-size "hello!" 16
				      100 baked)))
		  (print "Writing bitmap..
")
		  (let ((r (write-text-to-buffer s "hello!"
					16 100 baked)))
		    (print (cast r i64) newline)
		    (range y 0 (member s height)
			   (range x 0 (member s width)
				  (let ((v (deref (+ r (+ x (* y (member s width)))))))
				    ;(print (cast v i64) newline)
				    (if (< (cast v i64) 0)
					(print " ")
					(print "#")
					)
				  ))
			   (print newline))
		))))))))
;(test)
;(exit 0)
	  ;(exit 0)

;; 		(let ((bitmap (tt:get-codepoint-bitmap baked 0 0.05 codept  (addrof width) (addrof height) (addrof xpos) (addrof ypos))))
;; 		  (range y 0 (cast height i64)
;; 			 (range x 0 (cast width i64)
;; 				(let ((v (cast (deref (ptr+ bitmap (+ x (* y (cast width i64)))))
;; 					       i64)))
;; 				  (if (eq v -1)
;; 				      (print " ")
;; 				      (print "#"))))
;; 			 (print newline))
		  
;; 		  (print newline width " " height " " "inited.." newline))))))))
;; ;;     (print "Testing ttf" newline)))
;; 					;(let ((cpt (cast 0 i32)))
;; 					;  (read-utf8-codepoint "µ" (addrof cpt))
;; 					;  (print-hex (cast cpt i64))
;; 					;  (print newline))

