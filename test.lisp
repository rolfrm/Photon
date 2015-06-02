;; The following code compiles
"hello world!"
(write_line "hello world!")

; It is possible to compile functions
(defun adds (i64 (a i64) (b i64) (c i64))
  (progn
    (i64_add a (i64_add b c))))

(adds 3 4 5) ; this code should print 12
