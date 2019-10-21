;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:other-asd
  (:use :cl :asdf :uiop))

(in-package #:other-asd)

(defsystem #:other
  :name "OTHER"
  :version "1"
  :long-description "Other x does y"
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "other-package")
                                     (:file "other")

                                     )
                        ))
  :depends-on (
               :iterate
               )
  )


(provide :other)
