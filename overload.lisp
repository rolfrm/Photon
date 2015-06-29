
(type 
 (alias 
  (struct _overload-info
	  (sym (ptr symbol))
	  (arg-types (ptr (ptr type_def)))
	  (arg-cnt u64))
  overload-info))

(type
 (alias
  (struct _overload
	  (members (ptr overload-info))
	  (member-cnt i64))
  overload))

(printstr "size: ") (size-of (type overload-info))
(assert (eq (size-of (type overload-info)) (cast 24 u64)))

(progn
  (defvar prototype :type overload)
  (defvar ol-item :type overload-info))

(memset (cast (addrof prototype) (ptr void)) 0 (size-of (type overload)))
(memset (cast (addrof ol-item) (ptr void)) 0 (size-of (type overload-info)))

(defun mk-ol-item (overload-info (a (ptr symbol)))
  (let ((item ol-item) (cnt 0) (fcn-type (var-type a)))
    (assert (not (eq fcn-type (cast null (ptr type_def)))))
    (setf (member item sym) a)
    (setf (member item arg-types) (fcn-arg-types fcn-type))
    (setf (member item arg-cnt) (fcn-arg-cnt fcn-type))
    item))

(defun find-overload ((ptr symbol) (ol overload) (arg-types (ptr (ptr type_def))) (arg-cnt u64))
  (let ((i 0) (out (cast null (ptr symbol)))
	(cnt (member ol member-cnt))
	(mems (member ol members)))
    
    (while (and (eq out (cast null (ptr symbol)))
		(not (eq i cnt)))
      (let ((mem (deref (ptr+ mems i))))
	(let ((types (member mem arg-types))
	      (types-cnt (member mem arg-cnt)))
	  (if (eq types-cnt arg-cnt)
	      (let ((j (cast 0 u64))
		    (same true))
		(while (not (eq j arg-cnt))
		  (progn
		    (if (eq (deref (ptr+ types (cast j i64))) 
			    (deref (ptr+ arg-types (cast j i64))))
			(noop)
			(setf same false))
		    (setf j (u64+ j 1))))
		(if same
		    (progn
		      (setf out (member mem sym))
		      (noop))
		    (noop)))
	      (noop)))
	  (setf i (i64+ i 1))))
    out))

(defcmacro defoverloaded (fcn-name)
  (let ((s (symbol2expr (symbol-combine (expr2symbol fcn-name) (quote -info)))))
    (expr 
     (progn
       (defcmacro (unexpr fcn-name) (&rest d)
	 (let ((arg-type (cast (alloc (u64* (sub-expr.cnt d) (size-of (type (ptr type_def)))))
			       (ptr (ptr type_def))))
	       (_i (cast 0 u64)))
	
	   (while (not (eq _i (sub-expr.cnt d)))
	     (progn
	       (setf (deref (ptr+ arg-type (cast _i i64)))
		     (type-of (sub-expr.expr d _i)))

	       (setf _i (u64+ _i 1))))

	   (let ((ol (find-overload  (unexpr s) arg-type (sub-expr.cnt d))))
	     (unfold-body (symbol2expr ol) d)
	     )))
       
       (defvar (unexpr s) prototype)
       (noop)
       ))))


(defcmacro overload (name fcn)
  (var ((s (symbol2expr (symbol-combine (expr2symbol name) (quote -info)))))
       (expr 
	(let ((item (mk-ol-item (quote (unexpr fcn)))))
	  (add-to-list 
	   (cast (addrof (member (unexpr s) members)) (ptr (ptr void)))
	   (cast (addrof (member (unexpr s) member-cnt)) (ptr u64))
	   (cast (addrof item) (ptr void))
	   24)))))



(defoverloaded +)       

(defun add3i64 (i64 (a i64) (b i64) (c i64))
  (i64+ a (i64+ b c)))

(overload + i64+)
(overload + add3i64)
(overload + u64+)
(overload + u32+)
(overload + f+)
(overload + symbol-combine)
(overload + string-concat)
(+ (cast 1 u32) (cast 2 u32))
(+ (cast 1 u64) (cast 2 u64))
(+ "hello " "world")
(+ (cast 1.0 f64) (cast 2.0 f64))
(+ (quote something) (quote -else))
(+ 1 2 3)
;; (overload + i32+)
;; (overload + i16+)
;; (overload + i8+)

;; (overload + u32+)
;; (overload + u16+)
;; (overload + u8+)

(exit 0)
