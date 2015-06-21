
(type 
 (alias 
  (struct _overload-info
	  (type (ptr type_def))
	  (body (ptr expr))
  overload-info))

(type
 (alias
  (struct _overload
	  (members (ptr overload-info))
	  (member-cnt i64))
  overload))


(defcmacro defoverloaded (fcn-name)
  (expr
   (defvar (symbol-combine (quote (unexpr fcn-name)) (quote -info))
     (make overload :members (cast null (ptr overload-info)) :member-cnt 0))))

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
