;; The following code compiles
;"hello world!"
;(write_line "hello world!")

(defun + (i64 (a i64) (b i64))
  (i64_add a b))

;(defun + ((ptr char) (a (ptr char)) (b (ptr char)))
;  b)

;(defun plus (i64 (a i64) (b i64))
;  (i64_add a b))

;(defun +plus2 (i64 (a i64)) (+ 2 a))
(write_line "print +")
(+ 10 10)
;(+ "asd" "dsa")
(defvar test1 1)
(defvar test2 (+ test1 1))
(defvar test3 (+ test1 test2))
(defvar test4 (+ test3 test2))
(defvar test5 (+ test4 test3))
(defvar test6 (+ test5 test4))
(defvar test7 (+ test6 test5))
(defvar test8 (+ test7 test6))

(+ test8 test8)
(setf test8 100)
(+ test8 test7)
