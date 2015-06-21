
(defvar false (cast 0 bool))
(defvar true (cast 1 bool))
(defun not (bool (x bool)) (eq false x))
(defvar null (cast 0 (ptr void)))

(defun write-line (void (str (ptr char)))
  (write_line str))

(defvar libm (load-lib "libm.so"))
(load-symbol libm (quote cos) (quote cos) (type (fcn f64 (x f64))))
(load-symbol libm (quote cosf) (quote cosf) (type (fcn f32 (x f32))))
(cos 3.14)
(cosf 3.14)

(defcmacro ptradd (ptr offset)
  ptr)

    ;; (var ((size_expr (number2expr (size-of (type-of ptr)))))
    ;; 	 (progn
    ;; 	   (write_line "expr:")
    ;; 	   (print-expr size_expr)
    ;; 	   size_expr))))
    
