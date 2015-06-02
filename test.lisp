;; The following code compiles
"hello world!"
(write_line "hello world!")

(defun + (i64 (a i64) (b i64))
  (i64_add a b))

(defun plus (i64 (a i64) (b i64))
  (i64_add a b))

(defun +plus2 (i64 (a i64)) (+ 2 a))

(+ 2 (plus 5 10))
(+plus2 111)
