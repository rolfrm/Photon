
(defvar false (cast 0 bool))
(defvar true (cast 1 bool))
(defun not (bool (x bool)) (eq false x))
(defvar null (cast 0 (ptr void)))

(defun write-line (void (str (ptr char)))
  (write_line str))
