
(defvar *target*)

(let* ((cmd-args (ext:command-args))
       (target-pos (position "--target" cmd-args :test #'string=)))
  (if target-pos
      (let ((target (nth (+ target-pos 1) cmd-args)))
        (setf *target* (subseq target 0 (- (length target) 4))))
      (error "need to specify --target on command-line")))

(format t "*target* = ~s~%" *target*)

(defparameter *files-file* (format nil "~a.flist" *target*))

;;;
;;; (1) generate (recursive) file list for ASDF system
;;;

(let ((ecl (format nil "~a/bin/ecl" (si:getenv "HOST_ECL"))))
  (ext:run-program ecl `("--shell" "../utils/make-ASDF" "--target" ,*target*) :output t :error t))

;;;
;;; (2) cross-compile (see 'files.txt')
;;;

(load "../utils/helpers")

(load "../utils/cross-compile-64")

(require :cmp)

;(setf c::*compile-print* t)
(require :asdf)

(require :sb-bsd-sockets)

;;(setf *break-on-signals* 'error)

(setf *load-verbose* t)
(setf *compile-verbose* t)
(setf c::*suppress-compiler-warnings* nil)
(setf c::*suppress-compiler-notes* nil)

(setf c::*compile-in-constants* t)

;(trace c::builder) ; print out all the object files involved

(pushnew :release *features*)

(defparameter *files* (with-open-file (s *files-file* :direction :input)
                        (loop :for line = (read-line s nil nil)
                              :while line :collect line)))

(defparameter *force-compile* (find "-f" (ext:command-args) :test 'string=))

(format t "*files*: ~s~%" *files*)

(dolist (file *files*)
  (let ((src (x:cc file ".lisp"))
        (obj (x:cc file ".o")))
    (format t "~s~%" src)

    (if (find (pathname-name src) '("temporary-files") :test 'string=)
        (setf *break-on-signals* nil)
        (setf *break-on-signals* 'error))


    ;; exclude files using inline C code
    (unless (find (pathname-name src) '("sb-bsd-sockets" "sbcl" "c-functions" "whirlpool") :test 'string=)
      (format t "loading pathname src: ~s~%" (pathname-name src))
      (load src))

    (when (or *force-compile*
              (search (x:cc (ext:getenv "HOME") "/code/") file)
              (> (file-write-date src)
                 (if (probe-file obj)
                     (file-write-date obj)
                     0)))
      (unless (find (pathname-name src) '("NOTHING_YET") :test 'string=)
        (format t "cross:compile-file*: ~s~%" file)
        (cross:compile-file* file)))))

;; (cross:build-static-library* "build/app"
;;                              :lisp-files (mapcar (lambda (file) (x:cc file ".o"))
;;                                                  *files*)
;;                              :init-name "init_lib_APP_LISP"
;;                              ;; :epilogue-code '(test-app:start)
;;                              )


(let ((android-sysroot (x:cc (ext:getenv "ANDROID_NDK_TOOLCHAIN_64") "/sysroot/usr/lib/aarch64-linux-android/")))
  (cross:build-fasl* (x:cc "fas/" *target*)
                     :lisp-files (mapcar (lambda (file) (x:cc file ".o"))
                                         *files*)
                     :ld-flags (list (x:cc "-L" android-sysroot))
                     ))

