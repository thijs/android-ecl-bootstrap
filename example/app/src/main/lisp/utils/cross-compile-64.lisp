;;; Original file by Sylvain Ageneau for ECL.
;;;
;;; This is a much simplified version for EQL5.
;;; If you are on 64 bit Linux, you should not need to modify/adapt anything.
;;;
;;; requires NDK 18b standalone toolchain
;;;
;;; (See examples 'my' and 'REPL' for integrating with ASDF/Quicklisp)

(pushnew :android *features*)
(pushnew :aarch64 *features*)

(require :cmp)

(defpackage :cross
  (:use :common-lisp)
  (:export
   #:compile-file*
   #:build-static-library*
   #:build-shared-library*
   #:build-fasl*
   ))

(in-package :cross)

(defmacro with-android-env (() &body body)
  `(let* ((toolchain (ext:getenv "ANDROID_NDK_TOOLCHAIN_64"))
          (ecl-android (ext:getenv "ECL_ANDROID_64"))
          (compiler::*ecl-include-directory* (x:cc ecl-android "/include/"))
          (compiler::*ecl-library-directory* (x:cc ecl-android "/lib/"))
          ;; aarch64 (arm 64bit)
          (compiler::*cc* (x:cc toolchain "/bin/aarch64-linux-android-clang"))
          (compiler::*ld* (x:cc toolchain "/bin/aarch64-linux-android-ld"))
          (compiler::*ar* (x:cc toolchain "/bin/aarch64-linux-android-ar"))
          (compiler::*ranlib* (x:cc toolchain "/bin/aarch64-linux-android-ranlib"))
          (compiler::*cc-flags* (x:join (list "-DANDROID -DPLATFORM_ANDROID"
                                              "-O2 -fPIC -fno-common -D_THREAD_SAFE"
                                              (x:cc "-I" ecl-android "/build/gmp")))))
     ,@body))

(defun compile-file* (file &optional (system-p t))
  (with-android-env ()
    (compile-file file :system-p system-p)))

(defun build-static-library* (name &rest arguments)
  (with-android-env ()
    (apply 'c:build-static-library name arguments)))

(defun build-shared-library* (name &rest arguments)
  (with-android-env ()
    ;; (apply 'c:build-fasl name arguments)
    (apply 'c:build-shared-library name arguments)
    ))

(defun build-fasl* (name &rest arguments)
  (with-android-env ()
    (let ((compiler::*ld-rpath* nil))
      (apply 'c:build-fasl name arguments))))

(format t "~%*** cross compiling for 'aarch64' ***~%")
