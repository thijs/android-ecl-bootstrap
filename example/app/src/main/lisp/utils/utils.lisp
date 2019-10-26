
(defun makekw (name)
  (intern (string-upcase name) "KEYWORD"))


(defun slurp-file (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))
