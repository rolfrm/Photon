
(type 
 (alias 
  (struct _overload-info
	  (sym (ptr symbol))
	  (type (ptr type_def)))
  overload-info))

(type
 (alias
  (struct _overload-macro
	  (sym (ptr symbol))
	  (arg-cnt i64))
  overload-macro))

(type
 (alias
  (struct _overload
	  (members (ptr overload-info))
	  (member-cnt i64)
	  (default (ptr symbol))
	  (macros (ptr overload-macro))
	  (macro-cnt u64)
	  (name (ptr symbol))
	  )
  overload))

(assert (eq (size-of (type overload-info)) (cast 16 u64)))

(progn
  (defvar prototype :type overload)
  (defvar ol-item :type overload-info)
  (defvar overload-macro-default :type overload-macro)
  (noop)
  )

(memset (cast (addrof prototype) (ptr void)) 0 (size-of (type overload)))
(memset (cast (addrof ol-item) (ptr void)) 0 (size-of (type overload-info)))
(memset (cast (addrof overload-macro-default) (ptr void)) 0 (size-of (type overload-macro)))

(setf (member prototype name) (quote nothing))
(member prototype name)

(defun mk-ol-item (overload-info (a (ptr symbol)))
  (let ((item ol-item) (cnt 0) (fcn-type (var-type a)))
    (assert (not (eq fcn-type (cast null (ptr type_def)))))
    (assert (is-fcn-type? fcn-type))
    (setf (member item sym) a)
    (setf (member item type) fcn-type)
    item))

(defun find-overload ((ptr symbol) (ol overload)
		      (return-type (ptr type_def))
		      (exprs (ptr expr)))
  (let ((i 0) (out (cast null (ptr symbol)))
	(cnt (member ol member-cnt))
	(mems (member ol members)))
    (assert (is-sub-expr exprs))
    (while (and (eq out (cast null (ptr symbol)))
		(not (eq i cnt)))
      (let ((mem (deref (ptr+ mems i))))
	(let ((type (member mem type)))
	  (when (and (eq (fcn-arg-cnt type) (sub-expr.cnt exprs))
		     (eq return-type (fcn-ret-type type)))
	    
	    (let ((j (cast 0 u64))
		  (same true)
		  (arg-cnt (fcn-arg-cnt type))
		  (args (fcn-arg-types type)))
	      (while (and same (not (eq j arg-cnt)))
		(let ((this-type (deref (ptr+ args j))))
		  (printstr "OK?
")
	  	  (unless
		      ;; does this create an error?
		      
		      (eq (type-of2 this-type (sub-expr.expr exprs j))
			  this-type)
		    (setf same false)))
		(setf j (u64+ j 1)))
	      (when same
		(setf out (member mem sym))
		)))))
      (setf i (i64+ i 1)))
    out))

(defun expand-macro2 ((ptr expr) (sym (ptr symbol)) (expr2 (ptr expr)))
  (let ((v (cast (get-var sym) (ptr macro_store))))
    (let ((r (expand-macro v expr2)))
      r)))

(defun find-overload-macro ((ptr expr)  (ol overload) (return-type (ptr type_def)) (exprs (ptr expr)))
  (let ((i 0) (out (cast null (ptr expr)))
	(cnt (member ol macro-cnt))
	(macs (member ol macros))
	(arg-cnt (sub-expr.cnt exprs)))
    (while (and (eq out (cast null (ptr expr)))
		(not (eq (cast i u64) cnt)))
      (let ((macro (deref (ptr+ macs i))))
	(when (or (eq (member macro arg-cnt) -1)
		  (eq (member macro arg-cnt) (cast arg-cnt i64)))
	  (setf out (expand-macro2 (member macro sym) exprs))
	  ))
      (setf i (i64+ i 1)))
    out))
    
(defun get-overloaded-expr ((ptr expr) (return-type (ptr type_def)) (ol-info overload) (d (ptr expr)))
  (let ((ol (find-overload ol-info return-type d)))
    (if (eq null (cast ol (ptr void)))
	(let ((maco (find-overload-macro ol-info return-type d)))
	    (if (eq maco (cast null (ptr expr)))
		(let ((def (member ol-info default)))
		  (if (eq def (cast null (ptr symbol)))
		      (progn
			(printstr "Error no matching overload found for '")
			(print-symbol (member ol-info name))
			(printstr "'.\n Argument types:\n ")
			  (let ((j (cast 0 u64)))
			    (while (not (eq j (sub-expr.cnt d)))
			      (progn
				;(print-type (deref (ptr+ call-type (cast j i64))))
				(printstr " ")
				(setf j (u64+ j 1)))))
			  (printstr "\n")
			  (cast null (ptr expr)))
		      (unfold-body (symbol2expr def) d)))
		maco))
	  
	    (unfold-body (symbol2expr ol) d)
	    
	  ))))

(defmacro defoverloaded (fcn-name)
  (let ((s (symbol2expr (symbol-combine (expr2symbol fcn-name) (quote -info)))))
    (expr 
     (progn
       (defvar (unexpr s) prototype)
       (setf (member (unexpr s) name) (quote (unexpr fcn-name)))
       (defmacro (unexpr fcn-name) (&type a d)
	 (get-overloaded-expr a (unexpr s) d))
	  
       ))))

(defmacro overload (&type t namefcn)
  (progn
    (assert (eq (sub-expr.cnt namefcn) 2))
    (let ((name (sub-expr.expr namefcn 0))
	  (fcn (sub-expr.expr namefcn 1)))
      (let ((s (symbol2expr (symbol-combine (expr2symbol name) (quote -info)))))	      
	(let ((fcn-type (var-type (expr2symbol fcn))))
	  (if (is-fcn-type? fcn-type)
	      (expr 
	       (let ((item (mk-ol-item (quote (unexpr fcn)))))
		 (add-to-list 
		  (cast (addrof (member (unexpr s) members)) (ptr (ptr void)))
		  (cast (addrof (member (unexpr s) member-cnt)) (ptr u64))
		  (cast (addrof item) (ptr void))
		  (size-of (type overload-info))
		  )
		 (noop)
		 ))
	      (if (eq fcn-type (type (ptr macro_store)))
		  (expr 
		   (let ((macroitem overload-macro-default))
		     (setf (member macroitem sym) (quote (unexpr fcn)))
		     (setf (member macroitem arg-cnt) (unexpr 
						       (number2expr (macro-store-args 
								     (cast 
								      (get-var (expr2symbol fcn))
								      (ptr macro_store))))))
		     (add-to-list (cast (addrof (member (unexpr s) macros)) (ptr (ptr void)))
				  (cast (addrof (member (unexpr s) macro-cnt)) (ptr u64))
				  (cast (addrof macroitem) (ptr void))
				  (size-of (type overload-macro)))
		     (noop)))
		  (expr (noop)))))))
  ))

(defmacro overload-default (name macro)
  (let ((s (symbol2expr (symbol-combine (expr2symbol name) (quote -info)))))
    (expr (setf (member (unexpr s) default) (quote (unexpr macro))))))

(defoverloaded +)       
(defoverloaded -)
(defoverloaded /)
(defoverloaded *)
(defoverloaded %)
(defoverloaded print)

(overload + symbol-combine)

(overload + string-concat)
(+ "asd" "dsa")

;(exit 0)
;(overload + add3i64)

(defun u8+ (u8 (a u8) (b u8))
  (.+ a b))

(defun u8- (u8 (a u8) (b u8))
  (.- a b))

(defun u8* (u8 (a u8) (b u8))
  (.* a b))

(defun u8/ (u8 (a u8) (b u8))
  (./ a b))

(overload * i64*)
(overload / i64/)
(overload - i64-)
(overload + i64+)
(overload % i64%)

(overload * i8*)
(overload / i8/)
(overload - i8-)
(overload + i8+)

(overload * u8*)
(overload / u8/)
(overload - u8-)
(overload + u8+)

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

;; (defmacro .-2 (arg1 arg2)
;;   (expr (.- (unexpr arg1) (unexpr arg2))))

;; (defmacro +2 (arg1 arg2)
;;   (expr (.+ (unexpr arg1) (unexpr arg2))))

;; (defmacro *2  (arg1 arg2)
;;   (expr (.* (unexpr arg1) (unexpr arg2))))
 
;; (defmacro /2 (arg1 arg2)
;;   (expr (.* (unexpr arg1) (unexpr arg2))))
(overload + ptr+)

;; (overload - .-2)
;; (overload + +2)
;; (overload * *2)
;; (overload / /2)




;; (* 1 2 3 4 5) -> (* (* 1 2) (* 3 (* 4 5)))
;;               -> (* (* (* (* 1 2) 3 ) 4 ) 5)
;;               -> (* 1 (* 2 (* 3 (* 4 5))))
;; 4 multiplications gives 4 mult applications.
;; (- 1 2 3 4 5) -> (- 1 (+ 2 3 4 5))
;; (/ 1 2 3 4 5) -> (/ 1 (+ 2 3 4 5))
;; (+ 1 2 3 4 5) -> (+ 1 2 3 4 5);;
;; 1. make (* 4 5)
;; 2. make (* 3 (* 4 5))
;; 3. make (* 2 (* 3 (* 4 5)))
(defun expand-multi-arg ((ptr expr) (fcn (ptr expr)) (values (ptr expr)))
  (if (< (sub-expr.cnt values) 3)
      null-expr
      (let ((top (sub-expr.expr values (- (sub-expr.cnt values) 1))))
	(range it (cast (- (sub-expr.cnt values) 2) i64) -1
	       (let ((buffer (cast 
			      (alloc (*  (size-of (type (ptr expr))) 3))
			      (ptr (ptr expr)))))
		 (setf (deref (+ buffer 0)) fcn)
		 (setf (deref (+ buffer 1)) (sub-expr.expr values (cast it u64)))
		 (setf (deref (+ buffer 2)) top)
		 (setf top (make-sub-expr buffer 3))))
	top)))

(defmacro +any (&rest args)

  (expand-multi-arg (expr +) args))

(defmacro *any (&rest args)
  (expand-multi-arg (expr *) args))

(defmacro -/any (&rest args)
  (if (< (sub-expr.cnt args) 3)
      null-expr
      (expr
       (/ (unexpr (sub-expr.expr args 0))
	  (unexpr (unfold-body (expr +) 
			       (sub-expr.skip args 1)))))))

(defmacro -any (&rest args)
  (if (< (sub-expr.cnt args) 3)
      null-expr
      (expr
       (- (unexpr (sub-expr.expr args 0))
	  (unexpr (unfold-body (expr +) 
			       (sub-expr.skip args 1)))))))

(overload + +any)
(overload * *any)
(overload / -/any)
(overload - -any)

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

(defmacro print-default (body)
  (expr
   (progn
     (unexpr body)
     (printstr "--- unable to print result ---"))))
    
(overload-default print print-default)

(defmacro printnl (body)
  (expr
   (progn
     (print (unexpr body))
     (printstr newline))))

(defmacro no-print (body)
  (expr
   (progn 
     (unexpr body)
     (noop))))
			 
(set-printer (quote no-print))

(print "Hello world overload
")
