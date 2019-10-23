(in-package #:module)

(defun dummy ()
  (format nil "dummy~%"))


(defun dummy-bt ()
  (format nil "thread: ~a" (bt:make-thread (lambda () (sleep 10000)) :name "fake-bt-thread")))
