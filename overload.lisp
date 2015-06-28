
(type 
 (alias 
  (struct _overload-info
	  (sym (ptr symbol)))
  overload-info))

(type
 (alias
  (struct _overload
	  (members (ptr overload-info))
	  (member-cnt i64))
  overload))


(defvar prototype :type overload)
(defvar ol-item :type overload-info)
(memset (cast (addrof prototype) (ptr void)) 0 (size-of (type overload)))
(memset (cast (addrof ol-item) (ptr void)) 0 (size-of (type overload-info)))

(defun mk-ol-item (overload-info (a (ptr symbol)))
  (var ((item ol-item))
       (progn
	 (setf (member item sym) a)
	 item)))
(progn
  (defvar ol-item-test (mk-ol-item (quote i32)))
  (print-symbol (member ol-item-test sym))
  (noop))

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

(defcmacro overload (name fcn)
  (var ((s (symbol2expr (symbol-combine (expr2symbol name) (quote -info)))))
       (expr 
	(var ((item (mk-ol-item (quote (unexpr fcn)))))
	     (add-to-list+ (member (unexpr s) members)
			   (cast (member (unexpr s) member-cnt) u64)
			   item)))))

(defoverloaded +)       

(overload + i64+)
(overload + i32+)
(overload + i16+)
(overload + i8+)
(overload + u64+)
(overload + u32+)
(overload + u16+)
(overload + u8+)
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
