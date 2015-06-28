
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
	 (printstr "lol\n")
	 (setf (member item sym) a)
	 (setf (member item arg-types) (fcn-arg-types fcn_type))
	 (setf (member item arg-cnt) (fcn-arg-cnt fcn_type))
	 (printi64 (cast (member item arg-cnt) i64))
	 (printstr "lololo\n")
	 item)))
(progn
  ;(defvar ol-item-test (mk-ol-item (quote i32) (cast null (ptr type_def))))
  ;(print-symbol (member ol-item-test sym))
  (noop))

(defun find-overload ((ptr symbol) (ol overload) (a1 (ptr type_def)) (a2 (ptr type_def)))
  (let ((i 0) (out (cast null (ptr symbol)))
	(cnt (member ol member-cnt))
	(mems (member ol members)))
    (printi64 cnt)(printstr "happened A \n")
    
    (while (and (eq out (cast null (ptr symbol)))
		(not (eq i cnt)))
      (progn
	      (printi64 (cast i i64))(printstr "happened B \n")
	      (let ((mem (deref (ptr+ mems i))))
		(let ((types (member mem arg-types)))
		  (printstr "\n")(printi64 (cast (member mem arg-cnt) i64)) (printstr "<-- arg count \n")
		  (if (and 
		       (eq (deref types) a1)
		       (eq (deref (ptr+ types 1)) a2))
		      (progn
			(printstr "Gets here!")
			(setf out (member mem sym))
			(noop))
		      (noop))))
	      (setf i (i64+ i 1))))
    (printstr "DONE\n")
    (printi64 (cast out i64))
    (printstr "low\n")
    out))

(defcmacro defoverloaded (fcn-name)
  (let ((s (symbol-combine (expr2symbol fcn-name) (quote -info))))
    (expr (progn
	    (defcmacro (unexpr fcn-name) (a b)
	      (let ((a-type (type-of a))
		    (b-type (type-of b)))
		(expr (noop))))
	      (defvar (unexpr (symbol2expr s)) prototype)
	      (noop)))))


(defcmacro overload (name fcn)
  (var ((s (symbol2expr (symbol-combine (expr2symbol name) (quote -info)))))
       (expr 
	(let ((item (mk-ol-item (quote (unexpr fcn)) (var-type (quote (unexpr  fcn))))))
	  (printi64 (cast (member item arg-cnt) i64))
	  (printstr "\n")
	  (add-to-list 
	   (cast (addrof (member (unexpr s) members)) (ptr (ptr void)))
	   (cast (addrof (member (unexpr s) member-cnt)) (ptr u64))
	   (cast (addrof item) (ptr void))
	   24)))))

(defoverloaded +)       
(printstr "size: ")

(overload + i64+)
;; (overload + i32+)
;; (overload + i16+)
;; (overload + i8+)
(overload + u64+)
;; (overload + u32+)
;; (overload + u16+)
;; (overload + u8+)
(progn
  (defvar mem1 (deref (member +-info members)))
  (noop))
(member  mem1 arg-cnt)
(progn
  (find-overload +-info (type i64) (type i64))
  (noop))
(exit 0)



(exit 0)








(member +-info member-cnt)

(exit 0)
(expr
   (defvar (symbol-combine (quote  fcn-name)) (quote -info))
     (var ((overload prototype))
	  overload))))
(defoverloaded +test)
(exit 0)


(symbol-name (quote asd))
(quote omg)



(exit 0)

(defcmacro overload (name type body)
  (expr
   (let ((overload (addrof (unexpr (expr (symbol-combine (quote (unexpr name))
							 (quote -info))))))
	 (newoverload (make overload-info :type (type (unexpr type)) :body (unexpr body))))
     (let ((it_ptr (member (deref overload) members))
	   (it_cnt (member (deref overload) member-cnt))
	   (found (cast null (ptr overload-info))))
       (while (not (eq -1 it_cnt))
	 (progn
	   (setf it_cnt (i64- it_cnt 1))
	   (if (overload-eq (deref it_ptr) newoverload)
	       (progn
		 (setf found it_ptr)
		 (setf it_cnt 0))
	       1)
	   (setf it_ptr (cast (i64+ (cast it_ptr i64) 1) (ptr overload-info)))))
       (if (eq found (cast null (ptr overload-info)))
	   (progn
	     (add-to-list (addrof (member (deref overload) members))
			  (addrof (member (deref overload) member-cnt))
			  (addrof overload)
			  (size-of overload-info))
			  
			  
	     
	   

(defoverloaded +)

(overload + (i64 (a i64) (b i64))
  (i64+ a b))
(overload + (i32 (a i32) (b i32))
  (i32+ a b))

(+ 1 2)
(+ (cast 1 i32) (cast 2 i32))
