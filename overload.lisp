
(type 
 (alias 
  (struct _overload-info
	  (sym (ptr expr))
	  (type (ptr type_def)))
  overload-info))

(type
 (alias
  (struct _overload-macro
	  (sym (ptr expr))
	  (arg-cnt i64))
  overload-macro))

(type
 (alias
  (struct _overload
	  (members (ptr overload-info))
	  (member-cnt i64)
	  (default (ptr expr))
	  (macros (ptr overload-macro))
	  (macro-cnt u64)
	  (name (ptr expr))
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

(setf (member prototype name) (expr nothing))
(member prototype name)


(defun mk-ol-item (overload-info (a (ptr expr)))
  (let ((item ol-item) (cnt 0) (fcn-type (var-type a)))
    (assert (not (eq fcn-type (cast null (ptr type_def)))))
    (assert (is-fcn-type? fcn-type))
    (setf (member item sym) a)
    (setf (member item type) fcn-type)
    item))

(defun find-overload ((ptr expr) (ol overload)
		      (return-type (ptr type_def))
		      (exprs (ptr expr)))
  (macrolet ((cnt (member ol member-cnt))
	     (mems (member ol members)))
    (let ((i 0) (out (cast null (ptr expr))))
    (assert (is-sub-expr exprs))
    (while (and (eq out (cast null (ptr expr)))
		(not (eq i cnt)))
      (let ((mem (deref (ptr+ mems i))))
	(let ((type (member mem type)))
	  (when (and (eq (fcn-arg-cnt type) 
			 (sub-expr.cnt exprs))
		     (or (eq (cast return-type i64) 0)
			 (eq return-type (fcn-ret-type type))))
	    (let ((j 0)
		  (same true)
		  (arg-cnt (cast (fcn-arg-cnt type) i64))
		  (args (fcn-arg-types type)))
	      
	      (while (and same (not (eq j arg-cnt)))
		(let ((this-type (deref (ptr+ args j))))
		  (let ((ntype (type-of2 this-type (sub-expr.expr
						    exprs (cast j u64)))))
		    
		    (when
			;; does this create an error?
			(or
			 (not (eq ntype this-type))
			 (eq error-type ntype))
		      
		      (setf same false))))
		(setf j (.+ j 1)))
	      (when same
		(setf out (member mem sym))
		)))))
      (setf i (.+ i 1)))
    out)))

(defun expand-macro2 ((ptr expr) (sym (ptr expr)) (expr2 (ptr expr)))
  (let ((v (cast (get-var sym) (ptr macro_store))))
    (let ((r (expand-macro v expr2)))
      r)))

(defun find-overload-macro ((ptr expr)  (ol overload) (return-type (ptr type_def)) (exprs (ptr expr)))
  (let ((i 0) (out null-expr)
	(cnt (cast (member ol macro-cnt) i64))
	(macs (member ol macros))
	(arg-cnt (cast (sub-expr.cnt exprs) i64)))
    (while (and (eq out null-expr)
		(not (eq i cnt)))
      (let ((macro (deref (ptr+ macs i))))
	(when (or (eq (member macro arg-cnt) -1)
		  (eq (cast (member macro arg-cnt) i64) arg-cnt))
	  (setf out (expand-macro2 (member macro sym) exprs))
	  ))
      (setf i (.+ i 1)))
    out))
    
(defun get-overloaded-expr ((ptr expr) (return-type (ptr type_def)) (ol-info overload) (d (ptr expr)))
  (let ((ol (find-overload ol-info return-type d)))
    (if (eq null (cast ol (ptr void)))
	(let ((maco (find-overload-macro ol-info return-type d)))
	    (if (eq maco (cast null (ptr expr)))
		(let ((def (member ol-info default)))
		  (if (eq def (cast null (ptr expr)))
		      ;(progn ;; Consider enabling error msg.
			;(print-type return-type)
			;(print-expr d)
			;(printstr "Error no matching  overload found for '")
			;(print-symbol (member ol-info name))
			;(printstr "'.\n Argument types:\n ")
			;  (let ((j (cast 0 u64)))
			;    (while (not (eq j (sub-expr.cnt d)))
			;      (progn
				;(print-type (deref (ptr+ call-type (cast j i64))))
			;	(printstr " ")
			;	(setf j (u64+ j 1)))))
			;  (printstr "\n")
			  null-expr
		      (unfold-body def d)))
		maco))
	  
	    (unfold-body ol d)
	  )))

(defvar overload:current :type (ptr expr))
(defmacro defoverloaded (fcn-name)
  (progn
    (setf fcn-name (intern fcn-name))
    (let ((s (expr ((unexpr fcn-name) info)))
	  (n (intern (expr (nameof (unexpr fcn-name))))))
      (when (eq null (get-var s))
	(def n (type (ptr expr)) (cast fcn-name (ptr void)))
	(eval! (expr 
		(progn
		  (defvar (unexpr s) prototype)
		  (setf (member (unexpr s) name) (unexpr n))
		  (defmacro (unexpr fcn-name) (&type a d)
		    (get-overloaded-expr a (unexpr s) d))))))
      (expr (noop)))))

(defmacro overload (&type t namefcn)
  (progn
    (assert (eq (sub-expr.cnt namefcn) 2))
    (let ((name (sub-expr.expr namefcn 0))
	  (fcn (intern (sub-expr.expr namefcn 1))))
      (setf overload:current fcn)
  
      (let ((s (expr ((unexpr name) info))))
	(let ((fcn-type (var-type fcn)))
	  (assert (not (eq (cast fcn-type (ptr void)) null)))
	  (if (is-fcn-type? fcn-type)
	      (expr 
	       (let ((item (mk-ol-item overload:current)))

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
		     (setf (member macroitem sym) overload:current)
		     (setf (member macroitem arg-cnt) (unexpr 
						       (number2expr (macro-store-args 
								     (cast 
								      (get-var fcn)
								      (ptr macro_store))))))
		     (add-to-list (cast (addrof (member (unexpr s) macros)) (ptr (ptr void)))
				  (cast (addrof (member (unexpr s) macro-cnt)) (ptr u64))
				  (cast (addrof macroitem) (ptr void))
				  (size-of (type overload-macro)))
		     (noop)))
		  (expr (noop)))))))
  ))

(defmacro overload-default (name macro)
  (let ((s (expr  ((unexpr name) info))))
    (setf overload:current macro)
    (expr (setf (member (unexpr s) default) overload:current))))

(defoverloaded +)       
(defoverloaded -)
(defoverloaded /)
(defoverloaded *)
(defoverloaded %)
(defoverloaded print)
(builtin-print-str "now overloading..
")
;(overload + symbol-combine)
(overload + string-concat)
(+ "asd" "dsa")

(defmacro any+ (a b)
  (expr (.+ (unexpr a) (unexpr b))))

(defmacro any- (a b)
  (expr (.- (unexpr a) (unexpr b))))

(defmacro any* (a b)
  (expr (.* (unexpr a) (unexpr b))))

(defmacro any/ (a b)
  (expr (./ (unexpr a) (unexpr b))))

(defmacro any% (a b)
  (expr (.% (unexpr a) (unexpr b))))

(overload + ptr+)
(overload + any+)
(overload - any-)
(overload * any*)
(overload / any/)
(overload % any%)


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
      (the 
       (let ((top (sub-expr.expr values (- (sub-expr.cnt values) 1))))
	 (range it (cast (- (sub-expr.cnt values) 2) i64) -1
		(let ((buffer (cast 
			       (alloc (*  (size-of (type (ptr expr))) 3))
			       (ptr (ptr expr)))))
		  (setf (deref (+ buffer 0)) fcn)
		  (setf (deref (+ buffer 1)) (sub-expr.expr values (cast it u64)))
		  (setf (deref (+ buffer 2)) top)
		  (setf top (make-sub-expr buffer 3))))
	top)
       (ptr expr))))

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
(overload print printchar)
(overload print printstr)
;(overload print print-symbol)
(overload print print-type)
(overload print print-expr)

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
(set-printer (expr no-print))

