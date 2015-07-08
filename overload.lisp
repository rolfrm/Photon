
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
	  (member-cnt i64)
	  (default (ptr symbol))
	  )
  overload))

(printstr "size: ") (size-of (type overload-info))
(assert (eq (size-of (type overload-info)) (cast 24 u64)))

(progn
  (defvar prototype :type overload)
  (defvar ol-item :type overload-info)
  (noop))

(memset (cast (addrof prototype) (ptr void)) 0 (size-of (type overload)))
(memset (cast (addrof ol-item) (ptr void)) 0 (size-of (type overload-info)))

(defun mk-ol-item (overload-info (a (ptr symbol)))
  (let ((item ol-item) (cnt 0) (fcn-type (var-type a)))
    (assert (not (eq fcn-type (cast null (ptr type_def)))))
    (setf (member item sym) a)
    (setf (member item arg-types) (fcn-arg-types fcn-type))
    (setf (member item arg-cnt) (fcn-arg-cnt fcn-type))
    item))

(defun find-overload ((ptr symbol) (ol overload) (call-types (ptr (ptr type_def))) (arg-cnt u64) (exprs (ptr expr)))
  (let ((i 0) (out (cast null (ptr symbol)))
	(cnt (member ol member-cnt))
	(mems (member ol members)))
    (assert (is-sub-expr exprs))
    ;(print-expr exprs)
    ;(printstr "\n")
    (while (and (eq out (cast null (ptr symbol)))
		(not (eq i cnt)))
      (let ((mem (deref (ptr+ mems i))))
	(let ((types (member mem arg-types))
	      (types-cnt (member mem arg-cnt)))
	  (when (eq types-cnt arg-cnt)
	    (let ((j (cast 0 u64))
		  (same true))
	      (while (and same (not (eq j arg-cnt)))
		(unless (is-type-compatible 
			 (deref (ptr+ call-types (cast j i64))) 
			 (deref (ptr+ types (cast j i64)))
			 (sub-expr.expr exprs j)
			 )
		  (setf same false))
		(setf j (u64+ j 1)))
	      (when same
		(setf out (member mem sym))
		)))))
      (setf i (i64+ i 1)))
    (if (eq (cast null (ptr symbol)) out)
	(member ol default)
	out)))

(defun get-overloaded-expr ((ptr expr) (ol-info overload) (d (ptr expr)))
  (let ((call-type (cast (alloc (u64* (sub-expr.cnt d) (size-of (type (ptr type_def)))))
			 (ptr (ptr type_def))))
	(_i (cast 0 u64)))
    (while (not (eq _i (sub-expr.cnt d)))
      (progn
	(setf (deref (ptr+ call-type (cast _i i64)))
	      (type-of (sub-expr.expr d _i)))
	(setf _i (u64+ _i 1))))
    
    (let ((ol (find-overload ol-info  call-type (sub-expr.cnt d) d)))
      (if (eq null (cast ol (ptr void)))
	  (progn
	    (printstr "Error no matching overload found for '")
	    ;(print-symbol (quote (unexpr fcn-name)))
	    (printstr "' ")
		   (let ((j (cast 0 u64)))
		     (while (not (eq j (sub-expr.cnt d)))
		       (progn
			 (print-type (deref (ptr+ call-type (cast j i64))))
			 (printstr " ")
			 (setf j (u64+ j 1)))))
		   (printstr "\n")
		   (expr error))
	  (unfold-body (symbol2expr ol) d)
	  ))))


(defcmacro defoverloaded (fcn-name)
  (let ((s (symbol2expr (symbol-combine (expr2symbol fcn-name) (quote -info)))))
    (expr 
     (progn
       (defcmacro (unexpr fcn-name) (&rest d)
	 (get-overloaded-expr (unexpr s)  d)
	 )
       
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

(defcmacro overload-default (name macro)
  (let ((s (symbol2expr (symbol-combine (expr2symbol name) (quote -info)))))
    (expr (setf (member (unexpr s) default) (quote (unexpr macro))))))

(defoverloaded +)       
(defoverloaded -)
(defoverloaded /)
(defoverloaded *)
(defoverloaded print)

(defun add3i64 (i64 (a i64) (b i64) (c i64))
  (i64+ a (i64+ b c)))

(overload + symbol-combine)
(overload + string-concat)

(overload + add3i64)

(overload * i64*)
(overload / i64/)
(overload - i64-)
(overload + i64+)

(overload + u64+)
(overload * u64*)
(overload - u64-)
(overload / u64/)

(overload + f+)
(overload - f-)
(overload * f*)
(overload / f/)

(overload + u32+)
(overload - u32-)
(overload * u32*)
(overload / u32/)

(overload + f32+)
(overload - f32-)
(overload * f32*)
(overload / f32/)

(overload print printi64)
(overload print printi32)
(overload print printi16)
(overload print printi8)
(overload print printu64)
(overload print printu32)
(overload print printu16)
(overload print printu8)
(overload print printf64)
(overload print printf32)
(overload print printstr)
(overload print print-symbol)
(overload print print-type)

(defun print-bool (void (x bool))
  (if x
      (printstr "true")
      (printstr "false")))
(overload print print-bool)

(defcmacro print-default (body)
  (expr
   (progn
     (unexpr body)
     (printstr "--- unable to print result ---"))))
    
(overload-default print print-default)

(defcmacro printnl (body)
  (expr
   (progn
     (print (unexpr body))
     (printstr "\n"))))

(defcmacro no-print (body)
  (expr
   (progn 
     (unexpr body)
     (noop))))
			 
(set-printer (quote no-print))
