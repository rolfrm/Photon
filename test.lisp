2
(defun adds (i64 (a i64) (b i64) (c i64))
  (progn
    ;(write_line "hello?")
    
    (i64_add a (i64_add b c))))

(defun superadd (i64)
  (adds (adds 1 2 3) (adds 3 4 5) (adds 10 20 30)));

(superadd) (superadd) (superadd) (superadd)
(superadd) (superadd) (superadd) (superadd) (cast "hello" i64)
