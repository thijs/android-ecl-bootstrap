(pushnew :android *features*)

(defvar *target*)
(defvar *type*)

(let* ((cmd-args (ext:command-args))
       (type-pos (position "--type" cmd-args :test #'string=))
       (type (if type-pos (nth (+ type-pos 1) cmd-args) "undefined"))
       (target-pos (position "--target" cmd-args :test #'string=)))
  (if target-pos
      (let ((target (nth (+ target-pos 1) cmd-args)))
        (setf *target* (subseq target 0 (- (length target) 4))))
      (error "need to specify --target on command-line"))
  (setf *type* (intern (string-upcase type) "KEYWORD")))

(format t "*target* = ~s~%" *target*)
(format t "*type* = ~s~%" *type*)

(defparameter *files-file* (format nil "~a.flist" *target*))

;;;
;;; (1) generate (recursive) file list for ASDF system
;;;

(let ((ecl (format nil "~a/bin/ecl" (si:getenv "HOST_ECL"))))
  (ext:run-program ecl `("--shell" "../utils/make-ASDF" "--type" ,*type* "--target" ,*target*) :output t :error t))

;;;
;;; (2) cross-compile (see 'files.txt')
;;;

(load "../utils/helpers")

(load "../utils/cross-compile-64")

(require :cmp)

;;(setf c::*compile-print* t)
(require :asdf)

(require :sb-bsd-sockets)

;;(setf *break-on-signals* 'error)

(setf *load-verbose* t)
(setf *compile-verbose* t)
(setf c::*suppress-compiler-warnings* nil)
(setf c::*suppress-compiler-notes* nil)

(setf c::*compile-in-constants* t)

;;(trace c::builder) ; print out all the object files involved

(pushnew :release *features*)

(defparameter *files* (with-open-file (s *files-file* :direction :input)
                        (loop :for line = (read-line s nil nil)
                           :while line :collect line)))

(defparameter *force-compile* (find "-f" (ext:command-args) :test 'string=))

(format t "*files*: ~s~%" *files*)

(defvar base-path)
(defvar all-files)
(defvar fas-files)

(if (eq *type* :seperate)
    (progn
      (setf base-path (first *files*))
      (setf all-files (rest *files*))
      (setf fas-files (remove-if-not (lambda (el) (search base-path el)) all-files))
      (format t "base-path: ~s~%" base-path)
      (format t "all-files: ~s~%" all-files)
      (format t "fas-files: ~s~%" fas-files))
    (setf all-files *files*
          fas-files *files*))

(dolist (file all-files)
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
              ;; (search (x:cc (ext:getenv "HOME") "/code/") file)
              (> (file-write-date src)
                 (if (probe-file obj)
                     (file-write-date obj)
                     0)))
      (unless (find (pathname-name src) '("NOTHING_YET") :test 'string=)
        (format t "cross:compile-file*: ~s~%" file)
        (cross:compile-file* file)))))

;; (cross:build-static-library* "build/app"
;;                              :lisp-files (mapcar (lambda (file) (x:cc file ".o"))
;;                                                  fas-files)
;;                              :init-name "init_lib_APP_LISP"
;;                              ;; :epilogue-code '(test-app:start)
;;                              )


(format t "~&** building fas **~%")
(let ((android-sysroot (x:cc (ext:getenv "ANDROID_NDK_TOOLCHAIN_64") "/sysroot/usr/lib/aarch64-linux-android/")))
  (cross:build-fasl* (x:cc "fas/" *target*)
                     :lisp-files (mapcar (lambda (file) (x:cc file ".o")) fas-files)
                     :ld-flags (list (x:cc "-L" android-sysroot))))

(format t "~&** done building ~a**~%" (x:cc "fas/" *target*))
