;; The following code compiles
;"hello world!"
;(write_line "hello world!")

;(defun + (i64 (a i64) (b i64))
;  (i64_add a b))

(defun + ((ptr char) (a (ptr char)) (b (ptr char)))
  b)

;(defun plus (i64 (a i64) (b i64))
;  (i64_add a b))

;(defun +plus2 (i64 (a i64)) (+ 2 a))
(write_line "print +")
;(+ 10 10)
(+ "asd" "dsa")
(defvar test1 10)
(+ test1 5)
