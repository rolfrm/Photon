(defvar  stb-im:lib (load-lib (if (is-linux?) "./stb_image.so" "./stb_image.dll")))

(load-symbol+ stb-im:lib im:load stbi_load 
	      (fcn (ptr u8) (filename (ptr char)) 
		   (x (ptr i32)) (y (ptr i32)) (comp (ptr i32)) (req_comp i32)))

(load-symbol+ stb-im:lib im:free stbi_image_free (fcn void (image (ptr u8))))

(defun im:example-of-use(void (file (ptr char)))
    (let ((im-width :type i32)
	  (im-height :type i32)
	  (im-bpp :type i32))
      (let ((bg-im (im:load file  (addrof im-width) (addrof im-height) (addrof im-bpp) 0)))
	(print im-width " " im-height " " im-bpp newline)
	(im:free bg-im))))

(defstruct im:image
  (width i32)
  (height i32)
  (bpp i32)
  (data (ptr u8)))

(defun im:load-image (im:image (filename (ptr char)))
  (let ((out-im :type im:image))
    (setf 
     (member out-im data)
     (im:load filename (addrof (member out-im width)) 
	      (addrof (member out-im height))
	      (addrof (member out-im bpp))
	      0))
    out-im))

(defun im:free-image (void (image im:image))
  (im:free (member image data)))
	     
		     
(defun im:make (im:image (width i32) (height i32) (bpp i32))
  (let ((data (cast (alloc (cast (* width height bpp) u64)) (ptr u8)))
	(out :type im:image))
    (setf (member out width) width)
    (setf (member out height) height)
    (setf (member out bpp) bpp)
    (setf (member out data) data)
    out))
    
