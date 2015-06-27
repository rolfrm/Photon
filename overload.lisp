
(type 
 (alias 
  (struct _overload-info
	  (type (ptr type_def))
	  (body (ptr expr)))
  overload-info))

(type
 (alias
  (struct _overload
	  (members (ptr overload-info))
	  (member-cnt i64))
  overload))

(defun string-concat ((ptr char) (a (ptr char)) (b (ptr char)))
  (var ((a-len (cast (strlen a) u64))
	(b-len (cast (strlen b) u64)))
       (var ((buffer (alloc0 (u64+ 1 (u64+ a-len b-len)))))
	    (progn
	      (memcpy buffer (cast a (ptr void)) a-len)
	      (memcpy (ptr+ buffer (cast a-len i64)) (cast b (ptr void)) b-len)
	      (cast buffer (ptr char))))))

(string-concat "hello" "world")


(defun symbol-combine ((ptr symbol) (a (ptr symbol)) (b (ptr symbol)))
  (var ((aname (symbol-name a)) (bname (symbol-name b)))
       (var ((combined (string-concat aname bname)))
	    (var ((sym (get-symbol combined)))
		 (progn
		   (dealloc (cast combined (ptr void)))
		   sym)))))

(symbol-combine (quote a) (quote b))


;(print_type (type overload))

(defvar prototype :type overload)
(memset (cast (addrof prototype) (ptr void)) 0 (size-of (type overload)))

(defcmacro defoverloaded (fcn-name)
  (var ((s (symbol-combine (expr2symbol fcn-name) (quote -info))))
       (progn
	 (printstr "|")
	 (printstr (symbol-name s))
	 (printstr "|\n")
	 (expr (progn
		 (defcmacro (unexpr fcn-name) (a b)
		   (expr (noop)))
		 (defvar (unexpr (symbol2expr s)) prototype)
		 (noop))))))

(defcmacro overload (name type body)
  (expr (noop)))

(defoverloaded +)       

(overload + (fcn i64 (a i64) (b i64))
	  (i64+ a b))
(overload + (fcn i32 (a i32) (b i32))
	  (i32+ a b))
(+ 1 2)
(+ (cast 1 i32) (cast 2 i32))

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
