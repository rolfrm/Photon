
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

(defun mk-ol-item (overload-info (a (ptr symbol)) (fcn_type (ptr type_def)))
  (var ((item ol-item) (cnt 0))
       (progn
	 (assert (not (eq fcn_type (cast null (ptr type_def)))))
	 ;(printstr "lol\n")
	 (setf (member item sym) a)
	 (setf (member item arg-types) (fcn-arg-types fcn_type))
	 (setf (member item arg-cnt) (fcn-arg-cnt fcn_type))
	 ;(printi64 (cast (member item arg-cnt) i64))
	 ;(printstr "lololo\n")
	 item)))
(progn
  ;(defvar ol-item-test (mk-ol-item (quote i32) (cast null (ptr type_def))))
  ;(print-symbol (member ol-item-test sym))
  (noop))

(defun find-overload ((ptr symbol) (ol overload) (a1 (ptr type_def)) (a2 (ptr type_def)))
  (let ((i 0) (out (cast null (ptr symbol)))
	(cnt (member ol member-cnt))
	(mems (member ol members)))
    ;(printi64 cnt)(printstr "happened A \n")
    
    (while (and (eq out (cast null (ptr symbol)))
		(not (eq i cnt)))
      (progn
	;(printi64 (cast i i64))(printstr "happened B \n")
	      (let ((mem (deref (ptr+ mems i))))
		(let ((types (member mem arg-types)))
		  ;(printstr "\n")(printi64 (cast (member mem arg-cnt) i64)) (printstr "<-- arg count \n")
		  (if (and 
		       (eq (deref types) a1)
		       (eq (deref (ptr+ types 1)) a2))
		      (progn
			;(printstr "Gets here!")
			(setf out (member mem sym))
			(noop))
		      (noop))))
	      (setf i (i64+ i 1))))
    ;(printstr "DONE\n")
    ;(printi64 (cast out i64))
    ;(printstr "low\n")
    out))

(defcmacro defoverloaded (fcn-name)
  (let ((s (symbol2expr (symbol-combine (expr2symbol fcn-name) (quote -info)))))
    (expr 
     (progn
       (defcmacro (unexpr fcn-name) (a b)
	 (let ((a-type (type-of a))
	       (b-type (type-of b)))
	   (let ((ol (find-overload  (unexpr s) a-type b-type)))
	     (expr ((unexpr (symbol2expr ol)) (unexpr a) (unexpr b)))
	     )))
       (defvar (unexpr s) prototype)
       (noop)
       ))))


(defcmacro overload (name fcn)
  (var ((s (symbol2expr (symbol-combine (expr2symbol name) (quote -info)))))
       (expr 
	(let ((item (mk-ol-item (quote (unexpr fcn)) (var-type (quote (unexpr  fcn))))))
	  (add-to-list 
	   (cast (addrof (member (unexpr s) members)) (ptr (ptr void)))
	   (cast (addrof (member (unexpr s) member-cnt)) (ptr u64))
	   (cast (addrof item) (ptr void))
	   24)))))

(defoverloaded +)       
(overload + i64+)
(overload + u64+)
(overload + f+)
(overload + symbol-combine)
(overload + string-concat)
(+ 1 2)
(+ (cast 1 u64) (cast 2 u64))
(+ "hello " "world")
(+ (cast 1.0 f64) (cast 2.0 f64))
(+ (quote something) (quote -else))
;; (overload + i32+)
;; (overload + i16+)
;; (overload + i8+)

;; (overload + u32+)
;; (overload + u16+)
;; (overload + u8+)

(exit 0)
