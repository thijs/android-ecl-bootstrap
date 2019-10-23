;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:module-asd
  (:use :cl :asdf :uiop))

(in-package #:module-asd)

(defsystem #:module
  :name "MODULE"
  :version "1"
  :long-description "Module x does y"
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "module")

                                     )
                        ))
  :depends-on (
               :bordeaux-threads
               )
  )


(provide :module)
